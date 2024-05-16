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
import static utam.core.framework.context.MobilePlatformType.ANDROID_PHONE;
import static utam.core.framework.context.MobilePlatformType.ANDROID_TABLET;
import static utam.core.framework.context.MobilePlatformType.IOS_PHONE;
import static utam.core.framework.context.MobilePlatformType.IOS_TABLET;
import static utam.core.framework.context.MobilePlatformType.PLATFORM_PROFILE_NAME;
import static utam.core.framework.context.MobilePlatformType.WEB;
import static utam.core.framework.context.MobilePlatformType.fromDriver;
import static utam.core.selenium.appium.MobileDriverUtils.getActivePlatformProfile;

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
    final String DEVICE_NAME_NAME = "deviceName";
    final String DEVICE_NAME_VALUE_IPHONE = "iPhone";
    final String DEVICE_NAME_VALUE_IPAD = "iPad";
    final String DEVICE_SCREEN_SIZE_NAME = "deviceScreenSize";
    final String DEVICE_SCREEN_DENSITY_NAME = "deviceScreenDensity";
    final String DEVICE_SCREEN_SIZE_VALUE_TABLET = "1200x1920";
    final String DEVICE_SCREEN_DENSITY_VALUE_TABLET = "320";
    final String DEVICE_SCREEN_SIZE_VALUE_PHONE = "1080x1920";
    final String DEVICE_SCREEN_DENSITY_VALUE_PHONE = "480";

    AppiumDriver driver = mock(IOSDriver.class);
    when(driver.getSessionDetail("device")).thenReturn("iphone");
    assertThat(fromDriver(driver), is(IOS_PHONE));

    driver = mock(AndroidDriver.class);
    DesiredCapabilities desiredCaps = new DesiredCapabilities();
    desiredCaps.setCapability(DEVICE_SCREEN_SIZE_NAME, DEVICE_SCREEN_SIZE_VALUE_TABLET);
    desiredCaps.setCapability(DEVICE_SCREEN_DENSITY_NAME, DEVICE_SCREEN_DENSITY_VALUE_TABLET);
    when(driver.getCapabilities()).thenReturn(desiredCaps);
    assertThat(fromDriver(driver), is(ANDROID_TABLET));
    desiredCaps.setCapability(DEVICE_SCREEN_SIZE_NAME, DEVICE_SCREEN_SIZE_VALUE_PHONE);
    desiredCaps.setCapability(DEVICE_SCREEN_DENSITY_NAME, DEVICE_SCREEN_DENSITY_VALUE_PHONE);
    when(driver.getCapabilities()).thenReturn(desiredCaps);
    assertThat(fromDriver(driver), is(ANDROID_PHONE));

    driver = mock(AppiumDriver.class);
    assertThat(fromDriver(driver), is(WEB));
    desiredCaps = new DesiredCapabilities();
    desiredCaps.setPlatform(Platform.MAC);
    when(driver.getCapabilities()).thenReturn(desiredCaps);
    when(driver.getSessionDetail("device")).thenReturn("iPad");
    assertThat(fromDriver(driver), is(IOS_TABLET));
    desiredCaps.setPlatform(Platform.LINUX);
    desiredCaps.setCapability(DEVICE_SCREEN_SIZE_NAME, DEVICE_SCREEN_SIZE_VALUE_PHONE);
    desiredCaps.setCapability(DEVICE_SCREEN_DENSITY_NAME, DEVICE_SCREEN_DENSITY_VALUE_PHONE);
    when(driver.getCapabilities()).thenReturn(desiredCaps);
    assertThat(fromDriver(driver), is(ANDROID_PHONE));
    desiredCaps.setCapability(DEVICE_SCREEN_SIZE_NAME, DEVICE_SCREEN_SIZE_VALUE_TABLET);
    desiredCaps.setCapability(DEVICE_SCREEN_DENSITY_NAME, DEVICE_SCREEN_DENSITY_VALUE_TABLET);
    when(driver.getCapabilities()).thenReturn(desiredCaps);
    assertThat(fromDriver(driver), is(ANDROID_TABLET));
    desiredCaps = new DesiredCapabilities();
    desiredCaps.setPlatform(Platform.WINDOWS);
    when(driver.getCapabilities()).thenReturn(desiredCaps);
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
  }

  @Test
  public void testAsProfileAnddroidTablet() {
    Profile profile = ANDROID_TABLET;
    assertThat(profile.getName(), is(equalTo(PLATFORM_PROFILE_NAME)));
    assertThat(profile.getValue(), is(equalTo("android_tablet")));
  }

  @Test
  public void testAsProfileiOSPhone() {
    Profile profile = IOS_PHONE;
    assertThat(profile.getName(), is(equalTo(PLATFORM_PROFILE_NAME)));
    assertThat(profile.getValue(), is(equalTo("ios_phone")));
  }

  @Test
  public void testAsProfileiPad() {
    Profile profile = IOS_TABLET;
    assertThat(profile.getName(), is(equalTo(PLATFORM_PROFILE_NAME)));
    assertThat(profile.getValue(), is(equalTo("ios_tablet")));
  }
}
