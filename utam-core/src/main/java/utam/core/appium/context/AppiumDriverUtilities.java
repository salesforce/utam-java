/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.appium.context;


import org.openqa.selenium.WebDriver;

import utam.core.selenium.context.WebDriverUtilities;

/**
 * commonly used appium driver utilities
 * @author qren
 * @since 228
 */
public interface AppiumDriverUtilities extends WebDriverUtilities {
    /**
     * set active page context to NATIVE_APP
     * @return the new instance of the driver
     */
    WebDriver setPageContextToNative();

    /**
     * set active page context to the target WebView page
     * @return the new instance of the driver
     */
    WebDriver setPageContextToWebView();
    
    /**
     * set active page context to the WebView page with given title
     * @param title the title of the WebView page try to switch to
     * @return the new instance of the driver
     */
    WebDriver setPageContextToWebView(String title);
    
    /**
     * Returns boolean true if current context is native
     * @return true if current context is native
     */
    Boolean isNative();
    
}
