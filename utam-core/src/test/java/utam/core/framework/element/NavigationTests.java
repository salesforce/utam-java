/*
 * Copyright (c) 2022, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.element;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import io.appium.java_client.AppiumDriver;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebDriver.Navigation;
import org.testng.annotations.Test;
import utam.core.MockUtilities;

/**
 * Test for the Navigation implementation
 *
 * @author elizaveta.ivanova
 * @since 242
 */
public class NavigationTests {

  @Test
  public void testBackNavigation() {
    MockUtilities mock = new MockUtilities();
    Navigation navigationMock = mock(Navigation.class);
    WebDriver driver = mock.getWebDriverMock();
    when(driver.navigate()).thenReturn(navigationMock);
    new NavigationImpl(mock.getDriverAdapter()).back();
    verify(navigationMock, times(1)).back();
  }

  @Test
  public void testForwardNavigation() {
    MockUtilities mock = new MockUtilities(AppiumDriver.class);
    Navigation navigationMock = mock(Navigation.class);
    AppiumDriver driver = mock.getAppiumDriverMock();
    when(driver.navigate()).thenReturn(navigationMock);
    new NavigationImpl(mock.getDriverAdapter()).forward();
    verify(navigationMock, times(1)).forward();
  }
}
