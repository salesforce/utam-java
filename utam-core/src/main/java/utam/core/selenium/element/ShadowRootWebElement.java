/*
 * @Copyright, 1999-2018, salesforce.com
 *  All Rights Reserved
 *  Company Confidential
 *  Project LPOP
 */

package utam.core.selenium.element;

import org.openqa.selenium.*;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * COPY FROM SETI <br>
 * Class to represent a custom elements shadowRoot property.
 *
 * <p>The shadowRoot property on the client is represented as a DocumentFragment. Thus, if one tries
 * to return the shadowRoot property from the client via executeScript(), Selenium will simply
 * serialize the object rather than returning a WebElement.
 *
 * <p>Important notes about this class: - Not all WebElement APIs are supported on the shadowRoot
 * property, this is mainly for DOM tree traversal purposes. - findElement() and findElements()
 * *must* be called with By.cssSelector.
 *
 * @author tbliss, migrated by elizaveta.ivanova
 * @since 218
 */
@SuppressWarnings("all")
public class ShadowRootWebElement implements WebElement, WrapsElement, WrapsDriver {

  public static final String GET_SHADOW_ROOT_QUERY_SELECTOR_ALL = "return arguments[0].shadowRoot.querySelectorAll('%s')";
  public static final String GET_SHADOW_ROOT_QUERY_SELECTOR = "return arguments[0].shadowRoot.querySelector('%s')";
  public static final String SHADOW_ROOT_DETECTION_SCRIPT_FRAGMENT = "arguments[0].shadowRoot;";


  // The host element of the shadowRoot. Needed to execute queries off of.
  final WebElement rootElement;
  final JavascriptExecutor executor;

  public ShadowRootWebElement(WebElement we) {
    rootElement = we;
    WrapsDriver parentDriver = (WrapsDriver) rootElement;
    executor = (JavascriptExecutor) parentDriver.getWrappedDriver();
  }

  private static String escapeForQuery(String queryString) {
    return queryString.replace("\\", "\\\\").replace("'", "\\'").replace("\"", "\\\"");
  }

  @Override
  public <X> X getScreenshotAs(OutputType<X> target) throws WebDriverException {
    throw new UnsupportedCommandException(
        "Method <getScreenshotAs> not supported on shadowRoot element");
  }

  @Override
  public void click() {
    throw new UnsupportedCommandException("Method <click> not supported on shadowRoot element");
  }

  @Override
  public void submit() {
    throw new UnsupportedCommandException("Method <submit> not supported on shadowRoot element");
  }

  @Override
  public void sendKeys(CharSequence... keysToSend) {
    throw new UnsupportedCommandException("Method <sendKeys> not supported on shadowRoot element");
  }

  @Override
  public void clear() {
    throw new UnsupportedCommandException("Method <clear> not supported on shadowRoot element");
  }

  @Override
  public String getTagName() {
    throw new UnsupportedCommandException(
        "Method <getTagName> not supported on shadowRoot element");
  }

  @Override
  public String getAttribute(String name) {
    throw new UnsupportedCommandException(
        "Method <getAttribute> not supported on shadowRoot element");
  }

  @Override
  public boolean isSelected() {
    throw new UnsupportedCommandException(
        "Method <isSelected> not supported on shadowRoot element");
  }

  @Override
  public boolean isEnabled() {
    throw new UnsupportedCommandException("Method <isEnabled> not supported on shadowRoot element");
  }

  @Override
  public String getText() {
    throw new UnsupportedCommandException("Method <getText> not supported on shadowRoot element");
  }

  @Override
  public boolean isDisplayed() {
    throw new UnsupportedCommandException(
        "Method <isDisplayed> not supported on shadowRoot element");
  }

  @Override
  public Point getLocation() {
    throw new UnsupportedCommandException(
        "Method <getLocation> not supported on shadowRoot element");
  }

  @Override
  public Dimension getSize() {
    throw new UnsupportedCommandException("Method <getSize> not supported on shadowRoot element");
  }

  @Override
  public Rectangle getRect() {
    throw new UnsupportedCommandException("Method <getRect> not supported on shadowRoot element");
  }

  @Override
  public String getCssValue(String propertyName) {
    throw new UnsupportedCommandException(
        "Method <getCssValue> not supported on shadowRoot element");
  }

  @Override
  public WebElement getWrappedElement() {
    return rootElement;
  }

  static String getSelectorString(By by) {
    String byString = by.toString();
    if (!byString.startsWith("By.cssSelector")) {
      throw new InvalidArgumentException(
          "Must search for subelements of a shadowRoot element with By.ByCssSelector. Instead got: "
              + byString);
    }
    String queryString = byString.trim().replace("By.cssSelector: ", "");
    return escapeForQuery(queryString);
  }

  /**
   * When querying the shadowRoot property, this *must* be called with By.cssSelector. This is
   * because we translate the By.cssSelector parameter to a string to pass along to a client-side
   * element.querySelectorAll() call.
   */
  @SuppressWarnings("unchecked")
  @Override
  public List<WebElement> findElements(By by) {
    String selector = getSelectorString(by);
    String fullQuery = String.format(GET_SHADOW_ROOT_QUERY_SELECTOR_ALL, selector);
    Object elements = executor.executeScript(fullQuery, rootElement);
    return getElementsWithFirefoxWorkaround(elements);
  }

  @SuppressWarnings("unchecked")
  static List<WebElement> getElementsWithFirefoxWorkaround(Object elements) {
    try {
      if (elements == null) {
        return new ArrayList<>();
      }
      return (List<WebElement>) elements;
    } catch (ClassCastException cce) {
      /*
       * W-5608472: Temporary workaround for Firefox. LWC currently returns a "SyntheticNodeList" object that Firefox serializes
       * back as com.salesforce.seti.shaded.com.google.common.collect.Maps$TransformedEntriesMap and cannot be
       * cast as List<WebElement>.
       */
      List<WebElement> list = new ArrayList();
      Map<String, Object> result = (Map<String, Object>) elements;
      // TransformedEntriesMap does not return an ordered list.
      // Add items in the order according to their key. That is the order in which they are found
      // using querySelector.
      for (int i = 0; i < result.entrySet().size(); i++) {
        WebElement elm = (WebElement) result.get(String.valueOf(i));
        if (elm != null) {
          list.add(elm);
        }
      }
      return list;
    }
  }

  /**
   * When querying the shadowRoot property, this *must* be called with By.cssSelector. This is
   * because we translate the By.cssSelector parameter to a string to pass along to a client-side
   * element.querySelector() call.
   */
  @Override
  public WebElement findElement(By by) {
    String selector = getSelectorString(by);
    WebElement ret =
        (WebElement)
            executor.executeScript(
                String.format(GET_SHADOW_ROOT_QUERY_SELECTOR, selector), rootElement);

    if (ret == null) {
      throw new NoSuchElementException("Unable to locate element: " + by.toString());
    }
    return ret;
  }

  @Override
  public WebDriver getWrappedDriver() {
    return ((WrapsDriver) rootElement).getWrappedDriver();
  }
}
