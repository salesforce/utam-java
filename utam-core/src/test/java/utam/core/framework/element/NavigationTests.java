/*
 * Copyright (c) 2022, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.element;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import io.appium.java_client.AppiumDriver;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebDriver.Navigation;
import org.testng.annotations.Test;
import utam.core.MockUtilities;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/**
 * Test for the Navigation implementation
 *
 * @author elizaveta.ivanova
 * @since 242
 */
public class NavigationTests {

  public static final String TEST_URL = "http://www.example2.com";

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

  @Test
  public void testSwitchWindow() {
    MockUtilities mock = new MockUtilities(AppiumDriver.class);
    WebDriver driver = mock.getWebDriverMock();
    // Create a Map to store the associations between handles and URLs
    Map<String, String> handleUrlMap = new HashMap<String, String>();
    handleUrlMap.put("tab1", "http://www.example1.com");
    handleUrlMap.put("tab2", TEST_URL);

    // Specify the behavior of the getWindowHandles() method
    Set<String> handles = new HashSet<String>();
    handles.add("tab1");
    handles.add("tab2");
    handles.add("tab3");
    handles.add("tab4");
    handles.add("tab5");
    handles.add("tab6");
    when(driver.getWindowHandles()).thenReturn(handles);

    // Specify the behavior of the getCurrentUrl() method using the Map
    when(driver.getWindowHandle()).thenReturn("tab2");
    when(driver.getCurrentUrl()).thenAnswer(invocation -> handleUrlMap.get(driver.getWindowHandle()));

    // Switch to tab2 and assert the URL
    new NavigationImpl(mock.getDriverAdapter()).switchToWindow(TEST_URL);
    String url1 = driver.getCurrentUrl();
    assertThat(driver.getCurrentUrl(), is(equalTo(TEST_URL)));


  }
}
