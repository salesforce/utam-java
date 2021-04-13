/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.context;

import io.appium.java_client.AppiumDriver;
import org.openqa.selenium.WebDriver;

/**
 * @author elizaveta.ivanova
 * @since 228
 */
public enum Driver {
  web,
  ios,
  android,
  chrome,
  firefox;

  public static boolean isMobileDriver(WebDriver driver) {
    return driver instanceof AppiumDriver;
  }
}
