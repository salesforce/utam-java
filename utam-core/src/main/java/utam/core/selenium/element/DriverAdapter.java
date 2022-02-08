/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.selenium.element;

import static utam.core.framework.UtamLogger.error;
import static utam.core.selenium.element.ElementAdapter.wrapElement;

import java.time.Duration;
import java.util.List;
import java.util.function.Supplier;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.openqa.selenium.By;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.NoSuchElementException;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.support.ui.FluentWait;
import utam.core.driver.Driver;
import utam.core.driver.DriverConfig;
import utam.core.element.Element;
import utam.core.element.Locator;
import utam.core.framework.UtamCoreError;

/**
 * selenium web driver implementation of the driver
 *
 * @author elizaveta.ivanova
 * @since 234
 */
public class DriverAdapter implements Driver {

  // used in tests to validate right message
  /**
   * Error prefix for element not found
   */
  public static final String ERR_ELEMENT_NOT_FOUND_PREFIX = "can't find element";
  static final String ERR_SUPPORTED_FOR_MOBILE = "method is applicable only for iOS/Android";
  static final String ERR_CANT_ENTER_NULL_FRAME = "Can't enter null frame element";
  // not final because can be reset
  private WebDriver driver;
  private final DriverConfig driverConfig;

  /**
   * Initializes a new instance of the DriverAdapter class
   * @param driver       the driver instance
   * @param driverConfig the driver configuration object
   */
  public DriverAdapter(WebDriver driver, DriverConfig driverConfig) {
    this.driver = driver;
    this.driverConfig = driverConfig;
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

  static String getNotFoundErr(Locator by) {
    return String.format("%s with locator '%s'", ERR_ELEMENT_NOT_FOUND_PREFIX, by.getValue().toString());
  }

  static WebDriver getSeleniumDriver(Driver driver) {
    return ((DriverAdapter) driver).getSeleniumDriver();
  }

  /**
   * Resets the underlying driver
   *
   * @param driver the driver with which to replace the active one
   */
  protected final void resetDriver(WebDriver driver) {
    this.driver = driver;
  }

  @Override
  public void setPageContextToNative() {
    throw new IllegalStateException(ERR_SUPPORTED_FOR_MOBILE);
  }

  @Override
  public void setPageContextToWebView(String title) {
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
  public Element findElement(Locator locator) {
    By by = ((LocatorBy) locator).getValue();
    WebElement res = getSeleniumDriver().findElement(by);
    if (res == null) { // can happen only for mock
      throw new NoSuchElementException(getNotFoundErr(locator));
    }
    return wrapElement(this, res);
  }

  @Override
  public List<Element> findElements(Locator locator) {
    By by = ((LocatorBy) locator).getValue();
    List<WebElement> found = getSeleniumDriver().findElements(by);
    if (found == null || found.isEmpty()) {
      throw new NoSuchElementException(getNotFoundErr(locator));
    }
    return found.stream().map(el -> wrapElement(this, el)).collect(Collectors.toList());
  }

  @Override
  public int containsElements(Locator locator) {
    By by = ((LocatorBy) locator).getValue();
    return getSeleniumDriver().findElements(by).size();
  }

  @Override
  public <T> T waitFor(Supplier<T> isTrue, String message, Duration timeout) {
    Duration waitDuration = timeout == null? driverConfig.getExplicitTimeout() : timeout;
    String errorMessage = message == null? "wait for condition" : message;
    DriverWait driverWait = new DriverWait(this, waitDuration, driverConfig.getPollingInterval(), errorMessage);
    return driverWait.until(driver -> isTrue.get());
  }

  @Override
  public void enterFrame(Element element) {
    if(element == null) {
      throw new UtamCoreError(ERR_CANT_ENTER_NULL_FRAME);
    }
    WebElement webElement = ((ElementAdapter)element).getWebElement();
    driver.switchTo().frame(webElement);
  }

  @Override
  public void exitToParentFrame() {
    driver.switchTo().parentFrame();
  }

  @Override
  public void exitFrame() {
    driver.switchTo().defaultContent();
  }

  /**
   * Gets the underlying Selenium WebDriver instance
   *
   * @return the underlying Selenium WebDriver instance
   */
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

  @Override
  public DriverConfig getDriverConfig() {
    return driverConfig;
  }

  static class DriverWait extends FluentWait<Driver> {

    DriverWait(Driver input, Duration timeout, Duration pollingInterval, String message) {
      super(input);
      withTimeout(timeout);
      pollingEvery(pollingInterval);
      ignoring(Exception.class);
      withMessage(message);
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
