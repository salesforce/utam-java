/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.context;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;
import static org.mockito.Mockito.*;
import org.openqa.selenium.WebDriver;
import org.testng.annotations.Test;

import io.appium.java_client.AppiumDriver;
import io.appium.java_client.android.AndroidDriver;
import io.appium.java_client.ios.IOSDriver;
import utam.core.framework.context.Driver;

/**
 * Provides tests for the methods on the DriverType enumerated type
 * @author james.evans
 *
 */
public class DriverTypeTests {
  
  /**
   * The isMobileDriver method should return true for mobile browsers and
   * false for non-mobile browsers
   */
  @SuppressWarnings({"rawtypes"})
  @Test
  public void testIsMobileDriver() {
    IOSDriver iosDriver = mock(IOSDriver.class);
    assertThat(Driver.isMobileDriver(iosDriver), is(equalTo(true)));
    
    AndroidDriver androidDriver = mock(AndroidDriver.class);
    assertThat(Driver.isMobileDriver(androidDriver), is(equalTo(true)));

    AppiumDriver appiumDriver = mock(AppiumDriver.class);
    assertThat(Driver.isMobileDriver(appiumDriver), is(equalTo(true)));

    WebDriver driver = mock(WebDriver.class);
    assertThat(Driver.isMobileDriver(driver), is(equalTo(false)));
  }
}
