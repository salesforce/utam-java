/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.selenium.appium;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static utam.core.selenium.appium.MobilePlatformType.ANDROID;
import static utam.core.selenium.appium.MobilePlatformType.IOS;
import static utam.core.selenium.appium.MobilePlatformType.WEB;
import static utam.core.selenium.appium.MobilePlatformType.fromDriver;

import io.appium.java_client.AppiumDriver;
import io.appium.java_client.android.AndroidDriver;
import io.appium.java_client.ios.IOSDriver;
import org.openqa.selenium.Platform;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.remote.DesiredCapabilities;
import org.testng.annotations.Test;

/**
 * @author elizaveta.ivanova
 * @since 234
 */
public class MobilePlatformTypeTests {

  @Test
  public void testGetActivePlatformProfile() {
    assertThat(fromDriver(mock(WebDriver.class)), is(WEB));
    assertThat(fromDriver(mock(AndroidDriver.class)), is(ANDROID));
    assertThat(fromDriver(mock(IOSDriver.class)), is(IOS));
    AppiumDriver driver = mock(AppiumDriver.class);
    assertThat(fromDriver(driver), is(WEB));
    DesiredCapabilities capabilities = new DesiredCapabilities();
    capabilities.setPlatform(Platform.MAC);
    when(driver.getCapabilities()).thenReturn(capabilities);
    assertThat(fromDriver(driver), is(IOS));
    capabilities.setPlatform(Platform.LINUX);
    when(driver.getCapabilities()).thenReturn(capabilities);
    assertThat(fromDriver(driver), is(ANDROID));
    capabilities.setPlatform(Platform.WINDOWS);
    when(driver.getCapabilities()).thenReturn(capabilities);
    assertThat(fromDriver(driver), is(WEB));
  }
}
