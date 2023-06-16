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

  public static final String TEST_URL = "http://www.testurl.com";
  public static final String TEST_TAB = "test_tab";

  @Test
  public void testBackNavigation() {
    MockUtilities mock = new MockUtilities();
    Navigation navigationMock = mock(Navigation.class);
    WebDriver driver = mock.getWebDriverMock();
    when(driver.navigate()).thenReturn(navigationMock);
    new NavigationImpl(mock.getFactory()).back();
    verify(navigationMock, times(1)).back();
  }

  @Test
  public void testForwardNavigation() {
    MockUtilities mock = new MockUtilities(AppiumDriver.class);
    Navigation navigationMock = mock(Navigation.class);
    AppiumDriver driver = mock.getAppiumDriverMock();
    when(driver.navigate()).thenReturn(navigationMock);
    new NavigationImpl(mock.getFactory()).forward();
    verify(navigationMock, times(1)).forward();
  }

  @Test
  public void testSwitchWindow() {
    MockUtilities mock = new MockUtilities(AppiumDriver.class);
//    MockUtilities mock = new MockUtilities();
    WebDriver driver = mock.getWebDriverMock();
    // Create a Map to store the associations between handles and URLs
    Map<String, String> handleUrlMap = new HashMap<>();
    handleUrlMap.put("tab1", "http://www.example1.com");
    handleUrlMap.put(TEST_TAB, TEST_URL);
    handleUrlMap.put("tab3", "http://www.example3.com");
    handleUrlMap.put("tab4", "http://www.example4.com");
    handleUrlMap.put("tab5", "http://www.example5.com");
    handleUrlMap.put("tab6", "http://www.example6.com");

    // Specify the behavior of the getWindowHandles() method
    Set<String> handles = new HashSet<>();
    handles.add("tab1");
    handles.add(TEST_TAB);
    handles.add("tab3");
    handles.add("tab4");
    handles.add("tab5");
    handles.add("tab6");
    when(driver.getWindowHandles()).thenReturn(handles);

    // Specify the behavior of the getCurrentUrl() method using the Map
    when(driver.getWindowHandle()).thenReturn(TEST_TAB);
    when(driver.getCurrentUrl()).thenAnswer(invocation -> handleUrlMap.get(driver.getWindowHandle()));

    // Switch to test_url and assert the URL
    new NavigationImpl(mock.getFactory()).switchToWindow(TEST_URL);
    assertThat(driver.getCurrentUrl(), is(equalTo(TEST_URL)));
  }

  @Test
  public void testGetWindowCount() {
    MockUtilities mock = new MockUtilities();
    WebDriver driver = mock.getWebDriverMock();

    // Specify the behavior of the getWindowHandles() method
    Set<String> handles = new HashSet<>();
    handles.add("tab1");
    when(driver.getWindowHandles()).thenReturn(handles);

    assertThat(new NavigationImpl(mock.getFactory()).getWindowCount(), is(equalTo(1)));

    handles.add("tab2");
    handles.add("tab3");
    handles.add("tab4");
    handles.add("tab5");
    handles.add("tab6");

    assertThat(new NavigationImpl(mock.getFactory()).getWindowCount(), is(equalTo(6)));
  }

  @Test
  public void testCurrentWindow() {}

  @Test
  public void testWaitForNewWindowWithoutSetup() {}

  @Test
  public void testWaitForNewWindowWithSetup() {}

  @Test
  public void testWaitForNewWindowAndLoad() {}

  @Test
  public void testSwitchToWindowAndLoad() {}
}
