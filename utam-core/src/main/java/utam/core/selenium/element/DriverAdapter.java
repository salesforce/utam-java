/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.selenium.element;

import static utam.core.framework.UtamLogger.error;
import static utam.core.selenium.element.ElementAdapter.EMPTY_LIST;
import static utam.core.selenium.element.ElementAdapter.NULL_ELEMENT;

import java.time.Duration;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.openqa.selenium.InvalidElementStateException;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.SearchContext;
import org.openqa.selenium.StaleElementReferenceException;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebDriverException;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.support.ui.FluentWait;
import utam.core.driver.Driver;
import utam.core.driver.Expectations;
import utam.core.element.Element;
import utam.core.element.FindContext;
import utam.core.element.Locator;
import utam.core.selenium.appium.MobileElementAdapter;

/**
 * selenium web driver implementation of the driver
 *
 * @author elizaveta.ivanova
 * @since 234
 */
public class DriverAdapter implements Driver {

  static final String ERR_SUPPORTED_FOR_MOBILE = "method is applicable only for iOS/Android";

  private static final List<Class<? extends Throwable>> IGNORE_EXCEPTIONS =
      Stream.of(
          NoSuchElementException.class,
          StaleElementReferenceException.class,
          InvalidElementStateException.class,
          WebDriverException.class)
          .collect(Collectors.toList());
  // not final because can be reset
  private WebDriver driver;

  public DriverAdapter(WebDriver driver) {
    this.driver = driver;
  }

  /**
   * if parameter is a WebElement, we need to unwrap it before passing to JS executor
   *
   * @param parameters parameters for JS query
   * @return array of parameters
   */
  private static Object[] unwrapParameters(Object... parameters) {
    if (parameters == null || parameters.length == 0) {
      return new Object[0];
    }
    return Stream.of(parameters).map(p ->
        p instanceof ElementAdapter ? ((ElementAdapter) p).getWebElement() : p
    ).toArray(Object[]::new);
  }

  static WebElement find(SearchContext searchContext, LocatorBy by, FindContext finderContext) {
    try {
      WebElement res = searchContext.findElement(by.getValue());
      // mocks return null, so need this condition
      if (res == null && !finderContext.isNullable()) {
        throw new NullPointerException(getNotFoundErr(by));
      }
      return res;
    } catch (org.openqa.selenium.NoSuchElementException e) {
      if (finderContext.isNullable()) {
        return null;
      }
      throw new org.openqa.selenium.NoSuchElementException(getNotFoundErr(by), e);
    }
  }

  static String getNotFoundErr(Locator by) {
    return String.format("can't find element with locator '%s'", by.getValue().toString());
  }

  static List<WebElement> findList(SearchContext searchContext, LocatorBy by,
      FindContext finderContext) {
    List<WebElement> founds = searchContext.findElements(by.getValue());
    if (founds == null || founds.isEmpty()) {
      if (finderContext.isNullable()) {
        return null;
      }
      throw new org.openqa.selenium.NoSuchElementException(getNotFoundErr(by));
    }
    return founds;
  }

  private Function<WebElement, Element> getElementBuilder() {
    return element -> isMobile() ? new MobileElementAdapter(element) : new ElementAdapter(element);
  }

  protected final void resetDriver(WebDriver driver) {
    this.driver = driver;
  }

  @Override
  public void setPageContextToNative() {
    throw new IllegalStateException(ERR_SUPPORTED_FOR_MOBILE);
  }

  @Override
  public void setPageContextToWebView(String title, Duration timeout, Duration pollingInterval) {
    throw new IllegalStateException(ERR_SUPPORTED_FOR_MOBILE);
  }

  @Override
  public boolean isNative() {
    return false;
  }

  @Override
  public Object executeScript(String script, Object... parameters) {
    return ((JavascriptExecutor) driver).executeScript(script, unwrapParameters(parameters));
  }

  @Override
  public Element findElement(Locator by, FindContext finderContext) {
    WebElement element = find(getSeleniumDriver(), (LocatorBy) by, finderContext);
    return element == null ? NULL_ELEMENT : getElementBuilder().apply(element);
  }

  @Override
  public List<Element> findElements(Locator by, FindContext finderContext) {
    List<WebElement> elements = findList(getSeleniumDriver(), (LocatorBy) by, finderContext);
    return elements == null ? EMPTY_LIST
        : elements.stream().map(el -> getElementBuilder().apply(el)).collect(Collectors.toList());
  }

  @Override
  public <T> T waitFor(Duration timeout, Duration pollingInterval, Expectations<T> expectations,
      Element element) {
    if (timeout == null || timeout.isZero()) {
      return expectations.apply(this, element);
    }
    return new DriverWait(this, timeout, pollingInterval, expectations.getLogMessage())
        .until(driver -> expectations.apply(driver, element));
  }

  @Override
  public <T> T waitFor(Duration timeout, Duration pollingInterval, Expectations<T> expectations) {
    return waitFor(timeout, pollingInterval, expectations, null);
  }

  public WebDriver getSeleniumDriver() {
    return this.driver;
  }

  @Override
  public boolean isMobile() {
    return false;
  }

  @Override
  public String getUrl() {
    return driver.getCurrentUrl();
  }

  @Override
  public String getContext() {
    throw new IllegalStateException(ERR_SUPPORTED_FOR_MOBILE);
  }

  static class DriverWait extends FluentWait<Driver> {

    DriverWait(Driver input, Duration timeout, Duration pollingInterval, String message) {
      super(input);
      withTimeout(timeout);
      pollingEvery(pollingInterval);
      ignoreAll(IGNORE_EXCEPTIONS);
      withMessage("waiting for " + message);
    }

    @Override
    protected RuntimeException timeoutException(String message, Throwable lastException) {
      if (lastException instanceof RuntimeException) {
        error(message);
        return (RuntimeException) lastException;
      }
      return super.timeoutException(message, lastException);
    }
  }
}
