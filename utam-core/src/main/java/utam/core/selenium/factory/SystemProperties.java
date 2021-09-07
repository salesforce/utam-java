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
class SystemProperties {

  static final String FILE_PATH_SEPARATOR = System.getProperty("file.separator");
  static final String CHROME_DRIVER_PATH = "webdriver.chrome.driver";
  static final String GECKO_DRIVER_PATH = "webdriver.gecko.driver";

  // Appium test related
  static final String ANDROID_APP_PATH = "android.app";
  static final String ANDROID_DEVICE_NAME = "android.device";
  static final String APPIUM_PATH = "appium";
  static final String APP_ACTIVITY = "app.activity";
  static final String APP_BUNDLE_ID = "app.bundleid";
  static final String IOS_APP_PATH = "ios.app";
  static final String IOS_DEVICE_NAME = "ios.device";
  static final String NODEJS_PATH = "nodejs";

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
  static void setAndroidAppPath() {
    if (System.getProperty(ANDROID_APP_PATH) == null) {
      System.setProperty(ANDROID_APP_PATH, getUserHomePath() + "SApp.apk");
    }
  }

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

  static void setAppActivity() {
    if (System.getProperty(APP_ACTIVITY) == null) {
      System.setProperty(APP_ACTIVITY, "com.salesforce.chatter.Chatter");
    }
  }

  static String getAppActivity() {
    return System.getProperty(APP_ACTIVITY);
  }

  static void setAppBundleID() {
    if (System.getProperty(APP_BUNDLE_ID) == null) {
      System.setProperty(APP_BUNDLE_ID, "com.salesforce.chatter");
    }
  }

  static String getAppBundleID() {
    return System.getProperty(APP_BUNDLE_ID);
  }

  static void setIOSAppPath() {
    if (System.getProperty(IOS_APP_PATH) == null) {
      System.setProperty(IOS_APP_PATH, getUserHomePath() + "SApp.app");
    }
  }

  static String getIOSAppPath() {
    return System.getProperty(IOS_APP_PATH);
  }

  static void setIOSDeviceName() {
    if (System.getProperty(IOS_DEVICE_NAME) == null) {
      System.setProperty(IOS_DEVICE_NAME, "iPhone 8 Plus");
    }
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
