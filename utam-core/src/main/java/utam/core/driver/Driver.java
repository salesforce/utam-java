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
import utam.core.element.Element;
import utam.core.element.FindContext;
import utam.core.element.Locator;

/**
 * Driver interface allows to plugin any driver type, default is Selenium
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
   * @param by            selector used to find an element
   * @param finderContext indicates conditions of the search, ex. element being nullable
   * @return if finder context is nullable can return null
   */
  Element findElement(Locator by, FindContext finderContext);

  /**
   * find multiple elements inside driver
   *
   * @param by            selector used to find elements
   * @param finderContext indicates conditions of the search, ex. element being nullable
   * @return if finder context is nullable can return null
   */
  List<Element> findElements(Locator by, FindContext finderContext);

  /**
   * polling wait repeatedly applies expectations until truthy value is return (not null or boolean
   * true)
   *
   * @param timeout         timeout after which exception is thrown if condition is not met
   * @param pollingInterval interval between calling expectations action
   * @param expectations    action to apply
   * @param element         element expectations are applied to
   * @param <T>             return type
   * @return result of the applied expectations
   */
  <T> T waitFor(Duration timeout, Duration pollingInterval, Expectations<T> expectations,
      Element element);

  /**
   * polling wait repeatedly applies expectations until truthy value is return (not null or boolean
   * true)
   *
   * @param timeout         timeout after which exception is thrown if condition is not met
   * @param pollingInterval interval between calling expectations action
   * @param expectations    action to apply
   * @param <T>             return type
   * @return result of the applied expectations
   */
  <T> T waitFor(Duration timeout, Duration pollingInterval, Expectations<T> expectations);

  /**
   * set active page context to NATIVE_APP
   */
  void setPageContextToNative();

  /**
   * set active page context to the target WebView page, if view is different from current, new
   * driver is created
   *
   * @param title         title to switch to
   * @param timeout         timeout after which exception is thrown if condition is not met
   * @param pollingInterval interval between calling expectations action
   */
  void setPageContextToWebView(String title, Duration timeout, Duration pollingInterval);

  /**
   * check if current context is native
   *
   * @return boolean true if current context is native
   */
  boolean isNative();

  /**
   * check if current context is mobile
   *
   * @return boolean true if current context is native
   */
  boolean isMobile();

  /**
   * get current URL
   *
   * @return string with URL
   */
  String getUrl();

  /**
   * get current context, wraps AppiumDriver.getContext
   *
   * @return string with current context
   */
  String getContext();
}
