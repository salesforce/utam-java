/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
/*
 * @Copyright, 1999-2018, salesforce.com
 *  All Rights Reserved
 *  Company Confidential
 *  Project LPOP
 */

package utam.core.selenium.factory;

/**
 * system properties setup for web driver location
 *
 * @author elizaveta.ivanova
 * @since 216
 */
public class SystemProperties {

  public static final String FILE_PATH_SEPARATOR = System.getProperty("file.separator");
  public static final String CHROME_DRIVER_PATH = "webdriver.chrome.driver";
  public static final String GECKO_DRIVER_PATH = "webdriver.gecko.driver";

  // Appium test related
  public static final String ANDROID_APP_PATH = "android.app";
  public static final String ANDROID_DEVICE_NAME = "android.device";
  public static final String APPIUM_PATH = "appium";
  public static final String APP_ACTIVITY = "app.activity";
  public static final String APP_BUNDLE_ID = "app.bundleid";
  public static final String IOS_APP_PATH = "ios.app";
  public static final String IOS_DEVICE_NAME = "ios.device";
  public static final String NODEJS_PATH = "nodejs";

  static void setChromeDriverPath() {
    if (System.getProperty(CHROME_DRIVER_PATH) == null) {
      System.setProperty(CHROME_DRIVER_PATH, getUserHomePath() + "chromedriver");
    }
  }

  private static String getUserHomePath() {
    return System.getProperty("user.home") + FILE_PATH_SEPARATOR;
  }

  static void setGeckoDriverPath() {
    if (System.getProperty(GECKO_DRIVER_PATH) == null) {
      System.setProperty(GECKO_DRIVER_PATH, getUserHomePath() + "geckodriver");
    }
  }

  // Appium test related
  static String getAndroidAppPath() {
    return System.getProperty(ANDROID_APP_PATH);
  }

  static String getAndroidDeviceName() {
    return System.getProperty(ANDROID_DEVICE_NAME);
  }

  static void setAppiumPath() {
    if (System.getProperty(APPIUM_PATH) == null) {
      System.setProperty(APPIUM_PATH, "/usr/local/lib/node_modules/appium/");
    }
  }

  static String getAppiumPath() {
    return System.getProperty(APPIUM_PATH);
  }

  static String getAppActivity() {
    return System.getProperty(APP_ACTIVITY);
  }

  static String getAppBundleID() {
    return System.getProperty(APP_BUNDLE_ID);
  }

  static String getIOSAppPath() {
    return System.getProperty(IOS_APP_PATH);
  }

  static String getIOSDeviceName() {
    return System.getProperty(IOS_DEVICE_NAME);
  }

  static void setNodeJSPath() {
    if (System.getProperty(NODEJS_PATH) == null) {
      System.setProperty(NODEJS_PATH, "/usr/local/bin/node");
    }
  }

  static String getNodeJSPath() {
    return System.getProperty(NODEJS_PATH);
  }
}
