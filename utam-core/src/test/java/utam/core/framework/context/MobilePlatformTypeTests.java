/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.context;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static utam.core.framework.context.MobilePlatformType.PLATFORM_PROFILE_NAME;
import static utam.core.selenium.appium.MobileDriverUtils.getActivePlatformProfile;
import static utam.core.framework.context.MobilePlatformType.ANDROID_TABLET;
import static utam.core.framework.context.MobilePlatformType.ANDROID_PHONE;
import static utam.core.framework.context.MobilePlatformType.IOS_PHONE;
import static utam.core.framework.context.MobilePlatformType.IOS_TABLET;
import static utam.core.framework.context.MobilePlatformType.WEB;
import static utam.core.framework.context.MobilePlatformType.fromDriver;

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
  public void testGetActivePlatform() {
    AppiumDriver driver = mock(IOSDriver.class);
    when(driver.getSessionDetail("device")).thenReturn("iphone");
    assertThat(fromDriver(driver), is(IOS_PHONE));
    driver = mock(AndroidDriver.class);
    when(driver.getSessionDetail("deviceScreenSize")).thenReturn("1200x1920");
    when(driver.getSessionDetail("deviceScreenDensity")).thenReturn("320");
    assertThat(fromDriver(driver), is(ANDROID_TABLET));
    when(driver.getSessionDetail("deviceScreenSize")).thenReturn("1080x1920");
    when(driver.getSessionDetail("deviceScreenDensity")).thenReturn("480");
    assertThat(fromDriver(driver), is(ANDROID_PHONE));
    driver = mock(AppiumDriver.class);
    assertThat(fromDriver(driver), is(WEB));
    DesiredCapabilities capabilities = new DesiredCapabilities();
    capabilities.setPlatform(Platform.MAC);
    when(driver.getCapabilities()).thenReturn(capabilities);
    when(driver.getSessionDetail("device")).thenReturn("iPad");
    assertThat(fromDriver(driver), is(IOS_TABLET));
    capabilities.setPlatform(Platform.LINUX);
    when(driver.getCapabilities()).thenReturn(capabilities);
    when(driver.getSessionDetail("deviceScreenSize")).thenReturn("1080x1920");
    when(driver.getSessionDetail("deviceScreenDensity")).thenReturn("480");
    assertThat(fromDriver(driver), is(ANDROID_PHONE));
    when(driver.getSessionDetail("deviceScreenSize")).thenReturn("1200x1920");
    when(driver.getSessionDetail("deviceScreenDensity")).thenReturn("320");
    assertThat(fromDriver(driver), is(ANDROID_TABLET));
    capabilities.setPlatform(Platform.WINDOWS);
    when(driver.getCapabilities()).thenReturn(capabilities);
    assertThat(fromDriver(driver), is(WEB));
  }

  @Test
  public void testGetActivePlatformProfile() {
    Profile profile = getActivePlatformProfile(mock(WebDriver.class));
    assertThat(profile.getName(), is(equalTo(PLATFORM_PROFILE_NAME)));
    assertThat(profile.getValue(), is(equalTo("web")));
  }

  @Test
  public void testAsProfileAndroidPhone() {
    Profile profile = ANDROID_PHONE;
    assertThat(profile.getName(), is(equalTo(PLATFORM_PROFILE_NAME)));
    assertThat(profile.getValue(), is(equalTo("android_phone")));
    assertThat(profile.getConfigName(null), is(equalTo("platform_android_phone_config")));
    assertThat(profile.getConfigName(""), is(equalTo("platform_android_phone_config")));
    assertThat(profile.getConfigName("my"), is(equalTo("my_platform_android_phone_config")));
  }

  @Test
  public void testAsProfileAnddroidTablet() {
    Profile profile = ANDROID_TABLET;
    assertThat(profile.getName(), is(equalTo(PLATFORM_PROFILE_NAME)));
    assertThat(profile.getValue(), is(equalTo("android_tablet")));
    assertThat(profile.getConfigName(null), is(equalTo("platform_android_tablet_config")));
    assertThat(profile.getConfigName(""), is(equalTo("platform_android_tablet_config")));
    assertThat(profile.getConfigName("my"), is(equalTo("my_platform_android_tablet_config")));
  }

  @Test
  public void testAsProfileiOSPhone() {
    Profile profile = IOS_PHONE;
    assertThat(profile.getName(), is(equalTo(PLATFORM_PROFILE_NAME)));
    assertThat(profile.getValue(), is(equalTo("ios_phone")));
    assertThat(profile.getConfigName(null), is(equalTo("platform_ios_phone_config")));
    assertThat(profile.getConfigName(""), is(equalTo("platform_ios_phone_config")));
    assertThat(profile.getConfigName("my"), is(equalTo("my_platform_ios_phone_config")));
  }

  @Test
  public void testAsProfileiPad() {
    Profile profile = IOS_TABLET;
    assertThat(profile.getName(), is(equalTo(PLATFORM_PROFILE_NAME)));
    assertThat(profile.getValue(), is(equalTo("ios_tablet")));
    assertThat(profile.getConfigName(null), is(equalTo("platform_ios_tablet_config")));
    assertThat(profile.getConfigName(""), is(equalTo("platform_ios_tablet_config")));
    assertThat(profile.getConfigName("my"), is(equalTo("my_platform_ios_tablet_config")));
  }
}
