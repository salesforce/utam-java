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

import utam.core.framework.context.Driver;
import io.appium.java_client.AppiumDriver;
import io.appium.java_client.android.AndroidDriver;
import io.appium.java_client.ios.IOSDriver;
import io.appium.java_client.service.local.AppiumDriverLocalService;

import org.openqa.selenium.Platform;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.chrome.ChromeDriver;
import org.openqa.selenium.chrome.ChromeDriverService;
import org.openqa.selenium.chrome.ChromeOptions;
import org.openqa.selenium.firefox.FirefoxDriver;
import org.openqa.selenium.remote.DesiredCapabilities;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

/**
 * web driver factory
 *
 * @since 216
 * @author elizaveta.ivanova
 */
public class WebDriverFactory {

  private static final String ERR_APPIUM_LOCAL_SERVER = "Need to start an Appium Server at local";
  static final String ERR_UNKNOWN_DRIVER_TYPE = "Browser [%s] not supported";

  private static boolean isLocalRun() {
    return !Boolean.TRUE.toString().equals(System.getProperty("Jenkins"));
  }

  public static WebDriver getWebDriver(Driver browserType) {
    return getWebDriver(browserType, null, null);
  }

  @SuppressWarnings("WeakerAccess")
  public static WebDriver getWebDriver(
          Driver browserType, 
          AppiumDriverLocalService service,
          AppiumCapabilityProvider desiredCapabilities) {
    WebDriver driver;
    if (Driver.chrome.equals(browserType)) {
      driver = chrome();
    } else if (Driver.firefox.equals(browserType)) {
      driver = firefox();
    } else if (Driver.ios.equals(browserType)) {
      driver = ios(service, desiredCapabilities);
    } else if (Driver.android.equals(browserType)) {
      driver = android(service, desiredCapabilities);
    } else {
      throw new IllegalArgumentException(String.format(ERR_UNKNOWN_DRIVER_TYPE, browserType));
    }
    return driver;
  }

  static ChromeOptions defaultChromeOptions(boolean isJenkinsRun) {
    Map<Object, Object> chromePrefs = new HashMap<>();
    chromePrefs.put("profile.default_content_setting_values.notifications", 2);
    ChromeOptions chromeOptions = new ChromeOptions();
    chromeOptions.addArguments("--use-fake-ui-for-media-stream");
    if (isJenkinsRun) {
      chromeOptions.addArguments("headless");
      chromeOptions.addArguments("no-sandbox");
      chromeOptions.addArguments("window-size=1200x600");
    }
    chromeOptions.setExperimentalOption("prefs", chromePrefs);
    return chromeOptions;
  }

  private static ChromeDriverService initializeChromeDriverService() throws IOException {
    ChromeDriverService.Builder serviceBuilder =
        new ChromeDriverService.Builder().usingAnyFreePort();
    ChromeDriverService chromeDriverService = serviceBuilder.build();
    chromeDriverService.start();
    return chromeDriverService;
  }

  private static WebDriver chrome() {
    SystemProperties.setChromeDriverPath();
    ChromeOptions chromeOptions = defaultChromeOptions(!isLocalRun());
    try {
      ChromeDriverService service = initializeChromeDriverService();
      return new ChromeDriver(service, chromeOptions);
    } catch (IOException e) {
      throw new RuntimeException(e);
    }
  }

  private static WebDriver firefox() {
    SystemProperties.setGeckoDriverPath();
    return new FirefoxDriver();
  }

  private static DesiredCapabilities iOSOptions() {
    DesiredCapabilities caps = new DesiredCapabilities();
    caps.setPlatform(Platform.IOS);
    caps.setCapability(CustomizedAppiumCapabilityType.AUTOMATION_NAME, "XCUITest");
    caps.setCapability(CustomizedAppiumCapabilityType.NATIVE_WEB_TAP, true);
    caps.setCapability(CustomizedAppiumCapabilityType.FULL_RESET, true);
    caps.setCapability(CustomizedAppiumCapabilityType.DEVICE_NAME, SystemProperties.getIOSDeviceName());
    caps.setCapability(CustomizedAppiumCapabilityType.APP, SystemProperties.getIOSAppPath());
    return caps;
  }

  private static DesiredCapabilities androidOptions() {
    DesiredCapabilities caps = new DesiredCapabilities();
    caps.setPlatform(Platform.ANDROID);
    caps.setCapability(CustomizedAppiumCapabilityType.AUTOMATION_NAME, "UIAutomator2");
    caps.setCapability(CustomizedAppiumCapabilityType.DEVICE_NAME, SystemProperties.getIOSDeviceName());
    caps.setCapability(CustomizedAppiumCapabilityType.APP_PACKAGE, SystemProperties.getAppBundleID());
    caps.setCapability(CustomizedAppiumCapabilityType.APP_ACTIVITY, SystemProperties.getAppActivity());
    caps.setCapability(CustomizedAppiumCapabilityType.APP, SystemProperties.getAndroidAppPath());
    return caps;
  }

  private static AppiumDriver ios(AppiumDriverLocalService service, AppiumCapabilityProvider desiredCapabilities) {
    SystemProperties.setNodeJSPath();
    SystemProperties.setAppiumPath();
    if (service == null) {
      throw new NullPointerException(ERR_APPIUM_LOCAL_SERVER);
    }
    DesiredCapabilities caps = iOSOptions();
    caps.merge(desiredCapabilities.getDesiredCapabilities());
    return new IOSDriver(service, caps);
  }

  private static AppiumDriver android(AppiumDriverLocalService service, AppiumCapabilityProvider desiredCapabilities) {
    SystemProperties.setNodeJSPath();
    SystemProperties.setAppiumPath();
    if (service == null) {
      throw new NullPointerException(ERR_APPIUM_LOCAL_SERVER);
    }
    DesiredCapabilities caps = androidOptions();
    caps.merge(desiredCapabilities.getDesiredCapabilities());
    return new AndroidDriver(service, caps);
  }
}
