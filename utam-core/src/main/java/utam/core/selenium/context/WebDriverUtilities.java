/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.selenium.context;

import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.SearchContext;
import org.openqa.selenium.WebDriver;

/**
 * commonly used web driver utilities
 *
 * @author elizaveta.ivanova
 * @since 226
 */
public interface WebDriverUtilities {

  JavascriptExecutor getExecutor();

  void executeJavaScript(String script, Object... parameters);

  Object returnJavaScript(String script, Object... parameters);

  SearchContext expandShadowRoot(SearchContext sc);

  WebDriver getWebDriver();
}
