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
import java.util.Set;
import java.util.function.Supplier;
import utam.core.element.Element;
import utam.core.element.Locator;
import utam.core.framework.context.PlatformType;
import utam.core.selenium.element.Rect;

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
   * @param script string with javascript code
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
   *     null, timeout from config is used
   * @param isTrue condition to apply
   * @param message error message to throw if timeout is reached, can be null
   * @param <T> return type
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
   * resets the Driver configuration parameters
   *
   * @param config the configuration parameters to set
   */
  void resetDriverConfig(DriverConfig config);

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

  /** mobile only: set active page context to NATIVE_APP */
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

  /** Navigates backward in the current browsing context (mobile or web) */
  void back();

  /** Navigates forward in the current browsing context (mobile or web) */
  void forward();

  /**
   * Get window handle for the currently focussed window
   *
   * @return the window handle of the currently focussed window
   */
  String getWindowHandle();

  /**
   * Get the set of all window handles currently open in this driver This can be used to iterate
   * over all currently open windows in the driver
   *
   * @return the set of all window handles currently open in this driver
   */
  Set<String> getWindowHandles();

  /**
   * Switch focus to the window with the specified window handle
   *
   * @param windowHandle the window handle to have focus switched to
   */
  void switchTo(String windowHandle);

  /**
   * Get the Rect of the currently focused window
   *
   * @return the Rect of the currently focused window
   */
  Rect getRect();

  /**
   * Set the Rect of the currently focused window
   *
   * @param rect the Rect to set the currently focused window to
   */
  void setRect(Rect rect);

  /** Close the currently focused window */
  void close();
}
