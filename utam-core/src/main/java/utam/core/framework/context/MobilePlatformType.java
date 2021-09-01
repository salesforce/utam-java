/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.context;

import static utam.core.framework.context.StringValueProfile.getProfileConfigName;

import io.appium.java_client.AppiumDriver;
import io.appium.java_client.android.AndroidDriver;
import io.appium.java_client.ios.IOSDriver;
import org.openqa.selenium.Platform;
import org.openqa.selenium.WebDriver;

/**
 * type of a mobile platform is used to set active profile from tests
 *
 * @author elizaveta.ivanova
 * @since 234
 */
public enum MobilePlatformType implements Profile {

  WEB,
  ANDROID,
  ANDROID_TABLET,
  ANDROID_PHONE,
  IOS,
  IOS_TABLET,
  IOS_PHONE;

  public static final String PLATFORM_PROFILE_NAME = "platform";

  public boolean isIOS() {
    return this == IOS || this == IOS_PHONE || this == IOS_TABLET;
  }

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


  @Override
  public String getName() {
    return PLATFORM_PROFILE_NAME;
  }

  @Override
  public String getValue() {
    return name().toLowerCase();
  }

  @Override
  public String getConfigName(String moduleName) {
    return getProfileConfigName(this, moduleName);
  }

}
