/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.selenium.element;

import static utam.core.selenium.element.DriverAdapter.ERR_SUPPORTED_FOR_MOBILE;
import static utam.core.selenium.element.DriverAdapter.find;
import static utam.core.selenium.element.DriverAdapter.findList;
import static utam.core.selenium.element.DriverAdapter.getNotFoundErr;
import static utam.core.selenium.element.DriverAdapter.getSeleniumDriver;

import java.awt.Point;
import java.util.Collections;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;
import org.openqa.selenium.ElementNotVisibleException;
import org.openqa.selenium.NoSuchElementException;
import org.openqa.selenium.SearchContext;
import org.openqa.selenium.StaleElementReferenceException;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.interactions.Action;
import org.openqa.selenium.interactions.Actions;
import utam.core.driver.Driver;
import utam.core.element.Element;
import utam.core.element.FindContext;
import utam.core.element.Locator;
import utam.core.framework.UtamCoreError;
import utam.core.selenium.appium.MobileElementAdapter;

/**
 * implementation for selenium element
 *
 * @author elizaveta.ivanova
 * @since 234
 */
public class ElementAdapter implements Element {

  public static final Element NULL_ELEMENT = new ElementAdapter(null, (WebDriver) null);
  public static final String SCROLL_TOP_VIA_JAVASCRIPT =
      "return arguments[0].scrollIntoView(true);";
  public static final String SCROLL_INTO_VIEW_JS =
      "if (document.documentElement"
          + " && document.documentElement.style"
          + " && 'scrollBehavior' in document.documentElement.style) {"
          + "arguments[0].scrollIntoView({behavior: 'instant', block: 'end', inline: 'nearest'});"
          + "} else {"
          + "arguments[0].scrollIntoView(false);"
          + "}";
  static final List<Element> EMPTY_LIST = Collections.emptyList();
  static final String CLICK_VIA_JAVASCRIPT = "arguments[0].click();";
  static final String FOCUS_VIA_JAVASCRIPT = "arguments[0].focus();";
  static final String SCROLL_CENTER_VIA_JAVASCRIPT = "arguments[0].scrollIntoView({block:'center'});";
  static final String BLUR_VIA_JAVASCRIPT = "arguments[0].blur();";
  static final String SCROLL_INTO_VIEW_ERR =
      "element is still not visible or clickable after scroll into view";
  private static final String SCROLL_TO_DOCUMENT_ORIGIN_JS =
      "window.scrollTo(0,0);";
  static final String ERR_DRAG_AND_DROP_OPTIONS = "Either target element of offset should be set for drag and drop";
  private final WebElement webElement;
  private final WebDriver driver;
  protected final Driver driverAdapter;

  /**
   * constructor
   *
   * @param element element instance
   * @param driver  instance of the driver, might be needed if underlying framework does not have
   *                implementation for some actions out of the box
   */
  public ElementAdapter(WebElement element, WebDriver driver) {
    this.webElement = element;
    this.driver = driver;
    this.driverAdapter = driver == null? null : new DriverAdapter(driver);
  }

  public ElementAdapter(WebElement element, Driver driverAdapter) {
    this.webElement = element;
    this.driver = getSeleniumDriver(driverAdapter);
    this.driverAdapter = driverAdapter;
  }

  public WebElement getWebElement() {
    if (webElement == null) {
      throw new NullPointerException("WebElement is null");
    }
    return webElement;
  }

  private Function<WebElement, Element> getElementBuilder() {
    return element -> this instanceof MobileElementAdapter ? new MobileElementAdapter(element, driverAdapter)
        : new ElementAdapter(element, driver);
  }

  @Override
  public String toString() {
    return isNull() ? "null" : webElement.toString();
  }

  @Override
  public Element findElement(Locator by, FindContext finderContext) {
    if (webElement == null && finderContext.isNullable()) {
      return NULL_ELEMENT;
    }
    WebElement element = find(getScope(by, finderContext), (LocatorBy) by, finderContext);
    return element == null ? NULL_ELEMENT : getElementBuilder().apply(element);
  }

  private SearchContext getScope(Locator by, FindContext findContext) {
    if (webElement == null) {
      throw new NullPointerException(getNotFoundErr(by) + ", scope element is null");
    }
    return findContext.isExpandScopeShadowRoot() ? new ShadowRootWebElement(webElement)
        : webElement;
  }

  @Override
  public List<Element> findElements(Locator by, FindContext finderContext) {
    if (webElement == null && finderContext.isNullable()) {
      return EMPTY_LIST;
    }
    List<WebElement> elements = findList(getScope(by, finderContext), (LocatorBy) by,
        finderContext);
    return elements == null ? EMPTY_LIST
        : elements.stream().map(el -> getElementBuilder().apply(el)).collect(Collectors.toList());
  }

  @Override
  public boolean isDisplayed() {
    return getWebElement().isDisplayed();
  }

  @Override
  public void clear() {
    getWebElement().clear();
  }

  @Override
  public void click() {
    getWebElement().click();
  }

  @Override
  public void deprecatedClick() {
    driverAdapter.executeScript(CLICK_VIA_JAVASCRIPT, getWebElement());
  }

  @Override
  public void scrollIntoView(ScrollOptions options) {
    if (options == ScrollOptions.TOP) {
      if (isDisplayed()) {
        return;
      }
      scrollWithCompliance(driverAdapter);
      if (isDisplayed()) {
        return;
      }
      driverAdapter.executeScript(SCROLL_TOP_VIA_JAVASCRIPT, getWebElement());
      if (!isDisplayed()) {
        driverAdapter.executeScript(SCROLL_TO_DOCUMENT_ORIGIN_JS);
        driverAdapter.executeScript(SCROLL_TOP_VIA_JAVASCRIPT, getWebElement());
      }
      if (!isDisplayed()) {
        throw new ElementNotVisibleException(SCROLL_INTO_VIEW_ERR);
      }
    } else {
      driverAdapter.executeScript(SCROLL_CENTER_VIA_JAVASCRIPT, getWebElement());
    }
  }

  private void scrollWithCompliance(Driver driver) {
    // History lesson: The original WebDriver JSON Wire Protocol, now known
    // as the OSS dialect of the protocol, had a command for getting the
    // location of an element after scrolling it into view. This was exposed
    // in Selenium by using ((Locatable)element).getCoordinates().inViewPort().
    // Drivers compliant with the W3C WebDriver Specification do not support
    // that command. In modern browsers and modern versions of Selenium,
    // all driver instances are compliant with the specification, and no
    // longer need special cases. For scrolling into view, the Selenium Java
    // language bindings require using JavaScript. Note carefully that we
    // should only attempt to scroll if either the element is not currently
    // in the view port (which should be handled by isDisplayed). The below
    // JavaScript code is designed to work across all browsers, including
    // Internet Explorer, and works around a bug in Firefox 57 and higher
    // regarding scrolling elements into view when frames are present on the
    // page.
    driver.executeScript(SCROLL_INTO_VIEW_JS, getWebElement());
  }

  @Override
  public String getAttribute(String attrName) {
    return getWebElement().getAttribute(attrName);
  }

  @Override
  public String getText() {
    return getWebElement().getText();
  }

  @Override
  public void setText(String text) {
    getWebElement().sendKeys(text);
  }

  @Override
  public int containsElements(Locator by, boolean isExpandShadowRoot) {
    List<Element> found = this.findElements(by, FindContext.Type.build(true, isExpandShadowRoot));
    if (found == null) {
      return 0;
    }
    return found.size();
  }

  @Override
  public boolean isEnabled() {
    return getWebElement().isEnabled();
  }

  @Override
  public boolean isExisting() {
    if (isNull()) {
      return false;
    }
    // try apply any action to the element
    try {
      getWebElement().isDisplayed();
      return true;
    } catch (StaleElementReferenceException | NoSuchElementException e) {
      return false;
    }
  }

  @Override
  public boolean isNull() {
    return this.webElement == null;
  }

  @Override
  public void moveTo() {
    Actions actions = new Actions(driver);
    actions.moveToElement(getWebElement()).perform();
  }

  @Override
  public boolean hasFocus() {
    return driver
        .switchTo()
        .activeElement()
        .equals(getWebElement());
  }

  @Override
  public void blur() {
    driverAdapter.executeScript(BLUR_VIA_JAVASCRIPT, getWebElement());
  }

  @Override
  public void focus() {
    driverAdapter.executeScript(FOCUS_VIA_JAVASCRIPT, getWebElement());
  }

  @Override
  public boolean flickItems(GestureDirection direction) {
    throw new IllegalStateException(ERR_SUPPORTED_FOR_MOBILE);
  }

  @Override
  public void flick(int xOffset, int yOffset) {
    throw new IllegalStateException(ERR_SUPPORTED_FOR_MOBILE);
  }

  @Override
  public void dragAndDrop(DragAndDropOptions options) {
    WebElement from = getWebElement();
    // create an object of Actions class to build composite actions
    Actions builder = new Actions(driver).clickAndHold(from);
    if(options.getHoldDuration() != null && !options.getHoldDuration().isZero()) {
      builder.pause(options.getHoldDuration());
    }
    if(options.getTargetElement() != null) {
      WebElement to = ((ElementAdapter) options.getTargetElement()).getWebElement();
      builder.moveToElement(to).release(to);
    } else if(options.getOffset() != null) {
      Point offset = options.getOffset();
      builder.moveByOffset((int) offset.getX(), (int) offset.getY()).release();
    } else {
      throw new UtamCoreError(ERR_DRAG_AND_DROP_OPTIONS);
    }
    // build a drag and drop action
    Action dragAndDrop = builder.build();
    // perform the drag and drop action
    dragAndDrop.perform();
  }
}
