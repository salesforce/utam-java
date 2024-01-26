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

import static utam.core.driver.DriverConfig.TEST_SIMULATOR_DRIVER_CONFIG;

import io.appium.java_client.AppiumDriver;
import io.appium.java_client.android.AndroidDriver;
import io.appium.java_client.ios.IOSDriver;
import io.appium.java_client.service.local.AppiumDriverLocalService;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import org.openqa.selenium.Platform;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.chrome.ChromeDriver;
import org.openqa.selenium.chrome.ChromeDriverService;
import org.openqa.selenium.chrome.ChromeOptions;
import org.openqa.selenium.firefox.FirefoxDriver;
import org.openqa.selenium.remote.DesiredCapabilities;
import utam.core.driver.Driver;
import utam.core.driver.DriverConfig;
import utam.core.driver.DriverType;
import utam.core.selenium.appium.MobileDriverAdapter;
import utam.core.selenium.element.DriverAdapter;

/**
 * web driver factory
 *
 * @author elizaveta.ivanova
 * @since 216
 */
public class WebDriverFactory {

  static final String ERR_UNKNOWN_DRIVER_TYPE = "Browser [%s] not supported";
  private static final String ERR_APPIUM_LOCAL_SERVER = "Need to start an Appium Server at local";

  private static boolean isLocalRun() {
    return !Boolean.TRUE.toString().equals(System.getProperty("Jenkins"));
  }

  /**
   * Creates a WebDriver instance
   *
   * @param browserType the browser type to create the instance
   * @return the created WebDriver instance
   */
  public static WebDriver getWebDriver(DriverType browserType) {
    return getWebDriver(browserType, null, null);
  }

  /**
   * creates a DriverAdapter
   *
   * @param webDriver the WebDriver instance to wrap
   * @param driverConfig the configuration object to configure the driver adapter
   * @return the driver adapter
   */
  public static Driver getAdapter(WebDriver webDriver, DriverConfig driverConfig) {
    return webDriver instanceof AppiumDriver
        ? new MobileDriverAdapter((AppiumDriver) webDriver, driverConfig)
        : new DriverAdapter(webDriver, driverConfig);
  }

  /**
   * creates DriverAdapter for testing purposes with very low explicit wait
   *
   * @param driver mock of WebDriver
   * @return instance of Driver
   */
  public static Driver getAdapterMock(WebDriver driver) {
    return getAdapter(driver, TEST_SIMULATOR_DRIVER_CONFIG);
  }

  /**
   * Creates a WebDriver instance
   *
   * @param browserType the type of browser for which to create the WebDriver instance
   * @param service the service to create the driver
   * @param desiredCapabilities the desired capabilities of the resulting WebDriver instance
   * @return the created WebDriver instance
   */
  @SuppressWarnings("WeakerAccess")
  public static WebDriver getWebDriver(
      DriverType browserType,
      AppiumDriverLocalService service,
      AppiumCapabilityProvider desiredCapabilities) {
    WebDriver driver;
    if (DriverType.chrome.equals(browserType)) {
      driver = chrome();
    } else if (DriverType.firefox.equals(browserType)) {
      driver = firefox();
    } else if (DriverType.ios.equals(browserType)) {
      driver = ios(service, desiredCapabilities);
    } else if (DriverType.android.equals(browserType)) {
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
    SystemProperties.setIOSDeviceName();
    SystemProperties.setIOSAppPath();

    DesiredCapabilities caps = new DesiredCapabilities();
    caps.setPlatform(Platform.IOS);
    caps.setCapability(AppiumCustomCapabilityType.AUTOMATION_NAME, "XCUITest");
    caps.setCapability(AppiumCustomCapabilityType.NATIVE_WEB_TAP, true);
    caps.setCapability(AppiumCustomCapabilityType.DEVICE_NAME, SystemProperties.getIOSDeviceName());
    caps.setCapability(AppiumCustomCapabilityType.APP, SystemProperties.getIOSAppPath());
    return caps;
  }

  private static DesiredCapabilities androidOptions() {
    SystemProperties.setAppBundleID();
    SystemProperties.setAndroidAppPath();
    SystemProperties.setAppActivity();

    DesiredCapabilities caps = new DesiredCapabilities();
    caps.setPlatform(Platform.ANDROID);
    caps.setCapability(AppiumCustomCapabilityType.AUTOMATION_NAME, "UIAutomator2");
    caps.setCapability(AppiumCustomCapabilityType.APP_PACKAGE, SystemProperties.getAppBundleID());
    caps.setCapability(AppiumCustomCapabilityType.APP_ACTIVITY, SystemProperties.getAppActivity());
    caps.setCapability(AppiumCustomCapabilityType.APP, SystemProperties.getAndroidAppPath());
    return caps;
  }

  private static AppiumDriver ios(
      AppiumDriverLocalService service, AppiumCapabilityProvider desiredCapabilities) {
    if (service == null) {
      throw new NullPointerException(ERR_APPIUM_LOCAL_SERVER);
    }
    DesiredCapabilities caps = iOSOptions();
    caps.merge(desiredCapabilities.getDesiredCapabilities());
    return new IOSDriver(service, caps);
  }

  private static AppiumDriver android(
      AppiumDriverLocalService service, AppiumCapabilityProvider desiredCapabilities) {
    if (service == null) {
      throw new NullPointerException(ERR_APPIUM_LOCAL_SERVER);
    }
    DesiredCapabilities caps = androidOptions();
    caps.merge(desiredCapabilities.getDesiredCapabilities());
    return new AndroidDriver(service, caps);
  }
}
