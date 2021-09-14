/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.selenium.factory;

import io.appium.java_client.AppiumDriver;
import io.appium.java_client.android.AndroidDriver;
import io.appium.java_client.ios.IOSDriver;
import org.openqa.selenium.WebDriver;
import utam.core.driver.DriverType;
import org.testng.annotations.Test;
import utam.core.selenium.appium.MobileDriverAdapter;
import utam.core.selenium.element.DriverAdapter;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.core.Is.is;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.testng.Assert.assertThrows;
import static org.testng.Assert.expectThrows;
import static utam.core.selenium.factory.WebDriverFactory.ERR_UNKNOWN_DRIVER_TYPE;

/**
 * @author elizaveta.ivanova
 * @since 230
 */
public class WebDriverFactoryTests {

    @Test
    void testChromeOptions() {
        WebDriverFactory.defaultChromeOptions(false);
        WebDriverFactory.defaultChromeOptions(true);
        assertThrows(() -> WebDriverFactory.getWebDriver(mock(DriverType.class)));
    }

    @Test
    void testGetDriverError() {
        IllegalArgumentException e = expectThrows(IllegalArgumentException.class, () -> WebDriverFactory.getWebDriver(null));
        assertThat(e.getMessage(), is(equalTo(String.format(ERR_UNKNOWN_DRIVER_TYPE, "null"))));
    }

    @Test
    public void testGetAdapter() {
        IOSDriver iosDriver = mock(IOSDriver.class);
        when(iosDriver.getSessionDetail("device")).thenReturn("iphone");
        assertThat(WebDriverFactory.getAdapter(iosDriver), instanceOf(
            MobileDriverAdapter.class));

        AndroidDriver androidDriver = mock(AndroidDriver.class);
        when(androidDriver.getSessionDetail("deviceScreenSize")).thenReturn("1080x1920");
        when(androidDriver.getSessionDetail("deviceScreenDensity")).thenReturn("480");
        assertThat(WebDriverFactory.getAdapter(androidDriver), instanceOf(
            MobileDriverAdapter.class));

        AppiumDriver appiumDriver = mock(AppiumDriver.class);
        assertThat(WebDriverFactory.getAdapter(appiumDriver), instanceOf(
            MobileDriverAdapter.class));

        WebDriver driver = mock(WebDriver.class);
        assertThat(WebDriverFactory.getAdapter(driver), instanceOf(
            DriverAdapter.class));
    }
}
