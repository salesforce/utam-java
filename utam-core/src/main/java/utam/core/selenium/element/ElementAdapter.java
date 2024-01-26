/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.selenium.element;

import static utam.core.selenium.element.DriverAdapter.ERR_SUPPORTED_FOR_MOBILE;
import static utam.core.selenium.element.DriverAdapter.getNotFoundErr;
import static utam.core.selenium.element.DriverAdapter.getSeleniumDriver;

import java.time.Duration;
import java.util.List;
import java.util.stream.Collectors;
import org.openqa.selenium.By;
import org.openqa.selenium.NoSuchElementException;
import org.openqa.selenium.Rectangle;
import org.openqa.selenium.StaleElementReferenceException;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.interactions.Actions;
import utam.core.driver.Driver;
import utam.core.element.DragAndDropOptions;
import utam.core.element.Element;
import utam.core.element.Locator;
import utam.core.framework.consumer.UtamError;

/**
 * implementation for selenium element
 *
 * @author elizaveta.ivanova
 * @since 234
 */
public class ElementAdapter implements Element {

  /** The JavaScript snippet used to scroll elements into view to the top of the viewport */
  public static final String SCROLL_TOP_VIA_JAVASCRIPT =
      "return arguments[0].scrollIntoView(true);";

  /** The JavaScript snippet used to scroll elements into view to the center of the viewport */
  public static final String SCROLL_INTO_VIEW_JS =
      "if (document.documentElement"
          + " && document.documentElement.style"
          + " && 'scrollBehavior' in document.documentElement.style) {"
          + "arguments[0].scrollIntoView({behavior: 'instant', block: 'end', inline: 'nearest'});"
          + "} else {"
          + "arguments[0].scrollIntoView(false);"
          + "}";

  static final String CLICK_VIA_JAVASCRIPT = "arguments[0].click();";
  static final String FOCUS_VIA_JAVASCRIPT = "arguments[0].focus();";
  static final String SCROLL_CENTER_VIA_JAVASCRIPT =
      "arguments[0].scrollIntoView({block:'center'});";
  static final String BLUR_VIA_JAVASCRIPT = "arguments[0].blur();";
  static final String ERR_NULL_ELEMENT = "Element inside adapter is null";
  static final String IS_PARENT_NODE_SHADOW_ROOT_JS =
      "return arguments[0].getRootNode() instanceof ShadowRoot;";
  static final String ROOT_NODE_GET_ACTIVE_ELEMENT_JS =
      "return arguments[0].getRootNode().activeElement";
  private static final String SCROLL_INTO_VIEW_ERR =
      "element is still not visible or clickable after scroll into view";
  private static final String SCROLL_TO_DOCUMENT_ORIGIN_JS = "window.scrollTo(0,0);";

  /** The driver instance driving this element */
  protected final Driver driverAdapter;

  private final WebElement webElement;
  private final WebDriver driver;

  /**
   * Initializes a new instance of the ElementAdapter class
   *
   * @param element the WebElement to wrap
   * @param driverAdapter the driver used to drive the element
   */
  public ElementAdapter(WebElement element, Driver driverAdapter) {
    this.webElement = element;
    this.driver = getSeleniumDriver(driverAdapter);
    this.driverAdapter = driverAdapter;
  }

  /**
   * Create new instance of element adapter from an existing element. Used by
   * ShadowRootElementAdapter.
   *
   * @param element original element to wrap as ShadowRoot
   */
  ElementAdapter(Element element) {
    if (!(element instanceof ElementAdapter)) {
      throw new UnsupportedOperationException(
          "Internal bug in the utam-core: can't wrap element as shadow root");
    }
    ElementAdapter elementAdapter = (ElementAdapter) element;
    this.webElement = new ShadowRootWebElement(elementAdapter.getWebElement());
    this.driver = elementAdapter.driver;
    this.driverAdapter = elementAdapter.driverAdapter;
  }

  /**
   * Gets the underlying web element object
   *
   * @return the underlying web element object
   */
  public WebElement getWebElement() {
    if (webElement == null) {
      throw new NullPointerException(ERR_NULL_ELEMENT);
    }
    return webElement;
  }

  protected Element wrapElement(WebElement element) {
    return new ElementAdapter(element, driverAdapter);
  }

  @Override
  @SuppressWarnings("rawtypes")
  public Element findElement(Locator locator, boolean isNullable) {
    // return null for not found element, otherwise throws or returns non empty list
    List<Element> res = findElements(locator, isNullable);
    if (res == null) {
      return null;
    }
    return res.get(0);
  }

  @Override
  @SuppressWarnings("rawtypes")
  public List<Element> findElements(Locator locator, boolean isNullable) {
    By by = ((LocatorBy) locator).getValue();
    // per Selenium spec, this returns empty list if element not found
    List<WebElement> found = getWebElement().findElements(by);
    if (found == null || found.isEmpty()) {
      if (isNullable) {
        return null;
      } else {
        throw new NoSuchElementException(getNotFoundErr(locator));
      }
    }
    return found.stream().map(this::wrapElement).collect(Collectors.toList());
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
  public void doubleClick() {
    Actions actions = new Actions(driver);
    actions.doubleClick(getWebElement()).perform();
  }

  @Override
  public void rightClick() {
    Actions actions = new Actions(driver);
    actions.contextClick(getWebElement()).perform();
  }

  @Override
  public void clickAndHold(int holdDurationSec) {
    Actions actions = new Actions(driver);
    actions
        .clickAndHold(getWebElement())
        .pause(Duration.ofSeconds(holdDurationSec))
        .release()
        .perform();
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
        throw new UtamError(SCROLL_INTO_VIEW_ERR);
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
  public String getCssPropertyValue(String propertyName) {
    return getWebElement().getCssValue(propertyName);
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
  @SuppressWarnings("rawtypes")
  public int containsElements(Locator locator) {
    By by = ((LocatorBy) locator).getValue();
    return getWebElement().findElements(by).size();
  }

  @Override
  public boolean isEnabled() {
    return getWebElement().isEnabled();
  }

  @Override
  public boolean isExisting() {
    // try apply any action to the element
    try {
      getWebElement().isDisplayed();
      return true;
    } catch (StaleElementReferenceException | NoSuchElementException e) {
      return false;
    }
  }

  @Override
  public void moveTo() {
    Actions actions = new Actions(driver);
    actions.moveToElement(getWebElement()).perform();
  }

  @Override
  public boolean hasFocus() {
    WebElement self = getWebElement();
    // 1. check if current element's parent is shadowRoot
    // return arguments[0].getRootNode() instanceof ShadowRoot";
    Object isShadowHost = driverAdapter.executeScript(IS_PARENT_NODE_SHADOW_ROOT_JS, self);
    Object activeElement;
    if (Boolean.TRUE.equals(isShadowHost)) {
      // 2. get active element from shadowRoot
      // return arguments[0].getRootNode().activeElement;
      activeElement = driverAdapter.executeScript(ROOT_NODE_GET_ACTIVE_ELEMENT_JS, self);
    } else {
      // 2. or get active element from driver
      activeElement = driver.switchTo().activeElement();
    }
    // check if current element is same as active element
    return self.equals(activeElement);
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
  public void flick(int xOffset, int yOffset) {
    throw new IllegalStateException(ERR_SUPPORTED_FOR_MOBILE);
  }

  private boolean isHoldDurationZero(DragAndDropOptions options) {
    return options.getHoldDuration() == null || options.getHoldDuration().isZero();
  }

  private WebElement getTargetElement(DragAndDropOptions options) {
    if (options.getTargetElement() == null) {
      return null;
    }
    return ((ElementAdapter) options.getTargetElement()).getWebElement();
  }

  @Override
  public void dragAndDrop(DragAndDropOptions options) {

    WebElement source = getWebElement();
    WebElement target = getTargetElement(options);

    // create an object of Actions class to build composite actions
    Actions builder = new Actions(driver);

    if (isHoldDurationZero(options)) {
      // if duration is zero - use standard Selenium action
      if (target != null) {
        builder.dragAndDrop(source, target);
      } else {
        builder.dragAndDropBy(source, options.getXOffset(), options.getYOffset());
      }
    } else {
      // if duration is set - start from moving to source, click and hold
      builder.moveToElement(source).clickAndHold(source).pause(options.getHoldDuration());
      if (target != null) {
        builder.moveToElement(target).pause(options.getHoldDuration()).release(target);
      } else {
        builder
            .moveByOffset(options.getXOffset(), options.getYOffset())
            .pause(options.getHoldDuration())
            .release();
      }
    }

    // perform the drag and drop action
    builder.build().perform();
  }

  @Override
  public ElementRectangle getRect() {
    Rectangle rectangle = getWebElement().getRect();
    return new ElementRectangle() {
      @Override
      public int getX() {
        return rectangle.getX();
      }

      @Override
      public int getY() {
        return rectangle.getY();
      }

      @Override
      public int getWidth() {
        return rectangle.getWidth();
      }

      @Override
      public int getHeight() {
        return rectangle.getHeight();
      }
    };
  }
}
