/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.selenium.appium;

import io.appium.java_client.AppiumDriver;
import io.appium.java_client.android.AndroidDriver;
import io.appium.java_client.ios.IOSDriver;
import org.openqa.selenium.Platform;
import org.openqa.selenium.WebDriver;

/**
 * type of mobile platforms, used in Mobile Driver Utilities
 *
 * @author elizaveta.ivanova
 * @since 234
 */
public enum MobilePlatformType {

  WEB,
  ANDROID,
  IOS;

  /**
   * detect profile based on driver type
   *
   * @param driver driver instance
   * @return profile
   */
  static MobilePlatformType fromDriver(WebDriver driver) {
    if (driver instanceof AndroidDriver) {
      return ANDROID;
    }
    if (driver instanceof IOSDriver) {
      return IOS;
    }
    if (driver instanceof AppiumDriver) {
      // mock passed from test
      if(((AppiumDriver) driver).getCapabilities() == null) {
        return WEB;
      }
      Platform platform = ((AppiumDriver) driver).getCapabilities().getPlatform();
      if (platform == Platform.LINUX) {
        return ANDROID;
      }
      if (platform == Platform.MAC) {
        return IOS;
      }
    }
    return WEB;
  }
}
