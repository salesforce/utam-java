/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.driver;

import java.time.Duration;
import java.util.List;
import java.util.function.Supplier;
import utam.core.element.Element;
import utam.core.element.Locator;
import utam.core.framework.context.PlatformType;

/**
 * Driver interface allows to integrate any driver type, default is Selenium and Appium
 *
 * @author elizaveta.ivanova
 * @since 234
 */
public interface Driver {

  /**
   * executes given javascript
   *
   * @param script     string with javascript code
   * @param parameters parameters passed to the script
   * @return result of the script execution
   */
  Object executeScript(String script, Object... parameters);

  /**
   * find element inside driver
   *
   * @param by selector used to find an element
   * @return element or throws
   */
  Element findElement(Locator by);

  /**
   * find multiple elements inside driver
   *
   * @param by selector used to find elements
   * @return list of elements or throws
   */
  List<Element> findElements(Locator by);

  /**
   * get number of elements with a given locator inside current element
   *
   * @param by locator
   * @return number of elements or 0 if none found
   */
  int containsElements(Locator by);

  /**
   * polling wait repeatedly applies expectations until truthy value is return (not null or boolean
   * true)
   *
   * @param timeout timeout after which exception is thrown if condition is not met. If passed as
   *                null, timeout from config is used
   * @param isTrue  condition to apply
   * @param message error message to throw if timeout is reached, can be null
   * @param <T>     return type
   * @return result of the applied expectations
   */
  <T> T waitFor(Supplier<T> isTrue, String message, Duration timeout);

  /**
   * enters a frame or iframe element
   *
   * @param element the frame element to enter
   */
  void enterFrame(Element element);

  /**
   * exits focus from a frame or iframe to the immediate parent frame, or a no-op if already on the
   * top-level frame
   */
  void exitToParentFrame();

  /**
   * exits focus from a frame or iframe to the immediate parent frame, or a no-op if already on the
   * top-level frame
   */
  void exitFrame();

  /**
   * get current URL
   *
   * @return string with URL
   */
  String getUrl();

  /**
   * get Driver configuration parameters
   *
   * @return driver configuration parameters
   */
  DriverConfig getDriverConfig();

  /**
   * mobile only: get current context, wraps AppiumDriver.getContext
   *
   * @return string with current context
   */
  String getPageContext();

  /**
   * mobile only: switch context between native and webview
   *
   * @param mobileContextType mobile context type
   */
  void setPageContext(PlatformType mobileContextType);

  /**
   * mobile only: set active page context to NATIVE_APP
   */
  void setPageContextToNative();

  /**
   * mobile only: set active page context to the target WebView page, if view is different from
   * current, new driver is created
   *
   * @param title title to switch to
   */
  void setPageContextToWebView(String title);

  /**
   * mobile only: check if current context is native
   *
   * @return boolean true if current context is native
   */
  boolean isNativeContext();
}
