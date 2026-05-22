/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.context;

import io.appium.java_client.AppiumDriver;
import io.appium.java_client.android.AndroidDriver;
import io.appium.java_client.ios.IOSDriver;
import org.openqa.selenium.Capabilities;
import org.openqa.selenium.Platform;
import org.openqa.selenium.WebDriver;

/**
 * type of a mobile platform is used to set active profile from tests
 *
 * @author elizaveta.ivanova
 * @since 234
 */
public enum MobilePlatformType implements Profile {

  /** mobile web platform */
  WEB,

  /** android platform */
  ANDROID,

  /** android tablet platform */
  ANDROID_TABLET,

  /** android phone platform */
  ANDROID_PHONE,

  /** ios platform */
  IOS,

  /** ios tablet platform */
  IOS_TABLET,

  /** ios phone platform */
  IOS_PHONE;

  /** Mobile platform profile name */
  public static final String PLATFORM_PROFILE_NAME = "platform";

  /**
   * gets a value indicating whether the platform type is iOS
   *
   * @return true if the platform type is iOS; otherwise false
   */
  public boolean isIOS() {
    return this == IOS || this == IOS_PHONE || this == IOS_TABLET;
  }

  /**
   * gets a value indicating whether the platform type is Android
   *
   * @return true if the platform type is Android; otherwise false
   */
  public boolean isAndroid() {
    return this == ANDROID || this == ANDROID_PHONE || this == ANDROID_TABLET;
  }

  /**
   * detect profile based on driver type
   *
   * @param driver driver instance
   * @return profile
   */
  public static MobilePlatformType fromDriver(WebDriver driver) {
    if (driver instanceof AndroidDriver) {
      return isTablet(driver) ? ANDROID_TABLET : ANDROID_PHONE;
    }
    if (driver instanceof IOSDriver) {
      return isIPad(driver) ? IOS_TABLET : IOS_PHONE;
    }
    if (driver instanceof AppiumDriver) {
      // mock passed from test
      if (((AppiumDriver) driver).getCapabilities() == null) {
        return WEB;
      }
      Platform platform = ((AppiumDriver) driver).getCapabilities().getPlatformName();
      if (platform == Platform.ANDROID) {
        return isTablet(driver) ? ANDROID_TABLET : ANDROID_PHONE;
      }
      if (platform == Platform.IOS) {
        return isIPad(driver) ? IOS_TABLET : IOS_PHONE;
      }
    }
    return WEB;
  }

  @Override
  public String getName() {
    return PLATFORM_PROFILE_NAME;
  }

  @Override
  public String getValue() {
    return name().toLowerCase();
  }

  /**
   * Reads an Appium capability, supporting both the W3C-prefixed key ("appium:&lt;name&gt;") used
   * by Appium 2 / Selenium 4 and the unprefixed legacy key. The prefixed key takes precedence.
   *
   * @param caps capabilities to read from
   * @param name unprefixed capability name
   * @return capability value, or null if neither key is present
   */
  private static Object getAppiumCapability(Capabilities caps, String name) {
    Object value = caps.getCapability("appium:" + name);
    return value != null ? value : caps.getCapability(name);
  }

  // Capability keys that may carry a human-readable device model. On Sauce Labs RDC,
  // "deviceName" is set to the device UDID and the model lives under "testobject_device" /
  // "testobject_device_name"; BrowserStack / LambdaTest commonly use "device".
  private static final String[] DEVICE_NAME_CAPS = {
    "deviceName", "testobject_device_name", "testobject_device", "device"
  };

  private static boolean deviceMatches(Capabilities caps, String needle) {
    for (String key : DEVICE_NAME_CAPS) {
      Object value = getAppiumCapability(caps, key);
      if (value != null && value.toString().toLowerCase().contains(needle)) {
        return true;
      }
    }
    return false;
  }

  private static boolean isIPad(WebDriver driver) {
    return deviceMatches(((AppiumDriver) driver).getCapabilities(), "ipad");
  }

  private static boolean isTablet(WebDriver driver) {
    Capabilities caps = ((AppiumDriver) driver).getCapabilities();
    Object deviceScreenSizeObject = getAppiumCapability(caps, "deviceScreenSize");
    Object deviceScreenDensityObject = getAppiumCapability(caps, "deviceScreenDensity");

    // For android, based on https://developer.android.com/training/multiscreen/screensizes
    // when device's dp is equal or bigger than 600, will be treated as tablet, otherwise will be
    // phone
    if (deviceScreenSizeObject != null && deviceScreenDensityObject != null) {
      int dp =
          Integer.parseInt(deviceScreenSizeObject.toString().split("[xX]")[0])
              * 160
              / Integer.parseInt(deviceScreenDensityObject.toString());
      return dp >= 600;
    }
    // Fallback for cloud farms (Sauce RDC, BrowserStack) that don't expose
    // deviceScreenSize / deviceScreenDensity: check device-name caps for "tablet".
    return deviceMatches(caps, "tablet");
  }
}
