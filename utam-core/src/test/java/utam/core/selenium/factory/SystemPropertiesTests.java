/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.selenium.factory;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;

import org.testng.annotations.Test;

public class SystemPropertiesTests {
  /**
   * The static setChromeDriverPath method should set the webdriver.chrome.driver system property to
   * the proper value
   */
  @Test
  public void testSetChromeDriverPath() {
    String userHome = System.getProperty("user.home");
    assertThat(System.getProperty(SystemProperties.CHROME_DRIVER_PATH), is(nullValue()));
    SystemProperties.setChromeDriverPath();
    assertThat(
        System.getProperty(SystemProperties.CHROME_DRIVER_PATH),
        is(equalTo(userHome + SystemProperties.FILE_PATH_SEPARATOR + "chromedriver")));

    // Test that subsequent calls to the same method succeed
    SystemProperties.setChromeDriverPath();
    assertThat(
        System.getProperty(SystemProperties.CHROME_DRIVER_PATH),
        is(equalTo(userHome + SystemProperties.FILE_PATH_SEPARATOR + "chromedriver")));
  }

  /**
   * The static setGeckoDriverPath method should set the webdriver.gecko.driver system property to
   * the proper value
   */
  @Test
  public void testSetGeckoDriverPath() {
    String userHome = System.getProperty("user.home");
    assertThat(System.getProperty(SystemProperties.GECKO_DRIVER_PATH), is(nullValue()));
    SystemProperties.setGeckoDriverPath();
    assertThat(
        System.getProperty(SystemProperties.GECKO_DRIVER_PATH),
        is(equalTo(userHome + SystemProperties.FILE_PATH_SEPARATOR + "geckodriver")));

    // Test that subsequent calls to the same method succeed
    SystemProperties.setGeckoDriverPath();
    assertThat(
        System.getProperty(SystemProperties.GECKO_DRIVER_PATH),
        is(equalTo(userHome + SystemProperties.FILE_PATH_SEPARATOR + "geckodriver")));
  }

  /**
   * The static setAndroidAppPath method should set the android.app system property to the proper
   * value, and the static getAndroidAppPath method should return the proper value
   */
  @Test
  public void testSetAndroidAppPath() {
    assertThat(System.getProperty(SystemProperties.ANDROID_APP_PATH), is(nullValue()));
    SystemProperties.setAndroidAppPath();
    assertThat(
        System.getProperty(SystemProperties.ANDROID_APP_PATH),
        is(equalTo(SystemProperties.getAndroidAppPath())));

    // Test that subsequent calls to the same method succeed
    SystemProperties.setAndroidAppPath();
    assertThat(
        System.getProperty(SystemProperties.ANDROID_APP_PATH),
        is(equalTo(SystemProperties.getAndroidAppPath())));
  }

  /**
   * The static setAppiumPath method should set the appium system property to the proper value, and
   * the static getAppiumPath method should return the proper value
   */
  @Test
  public void testSetAppiumPath() {
    assertThat(System.getProperty(SystemProperties.APPIUM_PATH), is(nullValue()));
    SystemProperties.setAppiumPath();
    assertThat(
        System.getProperty(SystemProperties.APPIUM_PATH),
        is(equalTo(SystemProperties.getAppiumPath())));

    // Test that subsequent calls to the same method succeed
    SystemProperties.setAppiumPath();
    assertThat(
        System.getProperty(SystemProperties.APPIUM_PATH),
        is(equalTo(SystemProperties.getAppiumPath())));
  }

  /**
   * The static setAppActivity method should set the app.activity system property to the proper
   * value, and the static getAppActivity method should return the proper value
   */
  @Test
  public void testSetAppActivity() {
    assertThat(System.getProperty(SystemProperties.APP_ACTIVITY), is(nullValue()));
    SystemProperties.setAppActivity();
    assertThat(
        System.getProperty(SystemProperties.APP_ACTIVITY),
        is(equalTo(SystemProperties.getAppActivity())));

    // Test that subsequent calls to the same method succeed
    SystemProperties.setAppActivity();
    assertThat(
        System.getProperty(SystemProperties.APP_ACTIVITY),
        is(equalTo(SystemProperties.getAppActivity())));
  }

  /**
   * The static setAppBundleID method should set the app.bundleid system property to the proper
   * value, and the static getAppBundleID method should return the proper value
   */
  @Test
  public void testSetAppBundleID() {
    assertThat(System.getProperty(SystemProperties.APP_BUNDLE_ID), is(nullValue()));
    SystemProperties.setAppBundleID();
    assertThat(
        System.getProperty(SystemProperties.APP_BUNDLE_ID),
        is(equalTo(SystemProperties.getAppBundleID())));

    // Test that subsequent calls to the same method succeed
    SystemProperties.setAppBundleID();
    assertThat(
        System.getProperty(SystemProperties.APP_BUNDLE_ID),
        is(equalTo(SystemProperties.getAppBundleID())));
  }

  /**
   * The static setIOSAppPath method should set the ios.app system property to the proper value, and
   * the static getIOSAppPath method should return the proper value
   */
  @Test
  public void testSetIOSAppPath() {
    assertThat(System.getProperty(SystemProperties.IOS_APP_PATH), is(nullValue()));
    SystemProperties.setIOSAppPath();
    assertThat(
        System.getProperty(SystemProperties.IOS_APP_PATH),
        is(equalTo(SystemProperties.getIOSAppPath())));

    // Test that subsequent calls to the same method succeed
    SystemProperties.setIOSAppPath();
    assertThat(
        System.getProperty(SystemProperties.IOS_APP_PATH),
        is(equalTo(SystemProperties.getIOSAppPath())));
  }

  /**
   * The static setIOSDeviceName method should set the ios.device system property to the proper
   * value, and the static getIOSDeviceName method should return the proper value
   */
  @Test
  public void testSetIOSDeviceName() {
    assertThat(System.getProperty(SystemProperties.IOS_DEVICE_NAME), is(nullValue()));
    SystemProperties.setIOSDeviceName();
    assertThat(
        System.getProperty(SystemProperties.IOS_DEVICE_NAME),
        is(equalTo(SystemProperties.getIOSDeviceName())));

    // Test that subsequent calls to the same method succeed
    SystemProperties.setIOSDeviceName();
    assertThat(
        System.getProperty(SystemProperties.IOS_DEVICE_NAME),
        is(equalTo(SystemProperties.getIOSDeviceName())));
  }

  /**
   * The static setNodeJSPath method should set the nodejs system property to the proper value, and
   * the static getNodeJSPath method should return the proper value
   */
  @Test
  public void testSetNodeJSPath() {
    assertThat(System.getProperty(SystemProperties.NODEJS_PATH), is(nullValue()));
    SystemProperties.setNodeJSPath();
    assertThat(
        System.getProperty(SystemProperties.NODEJS_PATH),
        is(equalTo(SystemProperties.getNodeJSPath())));

    // Test that subsequent calls to the same method succeed
    SystemProperties.setNodeJSPath();
    assertThat(
        System.getProperty(SystemProperties.NODEJS_PATH),
        is(equalTo(SystemProperties.getNodeJSPath())));
  }
}
