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
import static org.testng.Assert.assertThrows;

import io.appium.java_client.AppiumDriver;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebDriver.Navigation;
import org.testng.annotations.Test;
import utam.core.MockUtilities;
import utam.core.framework.base.BasePageObject;
import utam.core.framework.base.PageMarker;
import utam.core.framework.base.PageObject;
import utam.core.framework.base.RootPageObject;
import utam.core.framework.consumer.UtamError;

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
    when(driver.getCurrentUrl())
        .thenAnswer(invocation -> handleUrlMap.get(driver.getWindowHandle()));

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
  public void testCurrentWindow() {
    MockUtilities mock = new MockUtilities();

    NavigationImpl navigation = new NavigationImpl(mock.getFactory());
    assertThat(navigation.currentWindow().getClass(), is(WindowImpl.class));
  }

  @Test
  public void testWaitForNewWindowWithoutSetup() {
    MockUtilities mock = new MockUtilities();

    NavigationImpl navigation = new NavigationImpl(mock.getFactory());
    assertThrows(UtamError.class, navigation::waitForNewWindow);
  }

  @Test
  public void testWaitForNewWindowWithSetup() throws InterruptedException {
    // Setup
    MockUtilities mock = new MockUtilities();
    WebDriver driver = mock.getWebDriverMock();
    NavigationImpl navigation = new NavigationImpl(mock.getFactory());
    AtomicBoolean finishedWaiting = new AtomicBoolean(false);

    // Hashmap mocks currently open windows
    Map<String, String> handleUrlMap = new HashMap<>();
    handleUrlMap.put("tab1", "http://www.example1.com");
    when(driver.getWindowHandles()).thenReturn(handleUrlMap.keySet());

    // Thread for testing the wait
    Thread waitForThread =
        new Thread(
            () -> {
              navigation.waitForNewWindow();
              finishedWaiting.set(true);
            });

    // Setup and start the method
    navigation.setupWaitForNewWindow();
    waitForThread.start();

    // The method should not finish until a new window is opened
    assertThat(finishedWaiting.get(), is(false));

    // Mocks the opening of a new window by adding another handle to the map
    handleUrlMap.put("tab2", "http://www.example2.com");

    // After 'opening a new window' the waitFor should return
    waitForThread.join(1000);
    assertThat(finishedWaiting.get(), is(true));
  }

  @Test
  public void testWaitForNewWindowAndLoad() throws InterruptedException {
    MockUtilities mock = new MockUtilities();
    WebDriver driver = mock.getWebDriverMock();
    NavigationImpl navigation = new NavigationImpl(mock.getFactory());
    AtomicBoolean finishedWaiting = new AtomicBoolean(false);

    // Hashmap mocks currently open windows
    Map<String, String> handleUrlMap = new HashMap<>();
    handleUrlMap.put("tab1", "http://www.example1.com");
    when(driver.getWindowHandles()).thenReturn(handleUrlMap.keySet());

    // Thread for testing the wait
    AtomicReference<PageObject> pageObject = new AtomicReference<>(null);
    Thread waitForThread =
        new Thread(
            () -> {
              pageObject.set(navigation.waitForNewWindowAndLoad(TestLoad.class));
              finishedWaiting.set(true);
            });

    // Setup and start the method
    navigation.setupWaitForNewWindow();
    waitForThread.start();

    handleUrlMap.put("tab2", "http://www.example2.com");
    waitForThread.join();

    assertThat(pageObject.get().getClass(), equalTo(TestLoad.class));
  }

  @Test
  public void testSwitchToNewWindowAndLoad() {
    MockUtilities mock = new MockUtilities();
    WebDriver driver = mock.getWebDriverMock();
    // Create a Map to store the associations between handles and URLs
    Map<String, String> handleUrlMap = new HashMap<>();
    handleUrlMap.put("tab1", "http://www.example1.com");
    handleUrlMap.put("tab2", "http://www.example2.com");

    // Specify the behavior of the getWindowHandles() method
    HashSet<String> handles = new HashSet<>();
    handles.add("tab1");
    handles.add("tab2");
    when(driver.getWindowHandles()).thenReturn(handles);

    // Specify the behavior of the getCurrentUrl() method using the Map
    when(driver.getWindowHandle()).thenReturn("tab2");
    when(driver.getCurrentUrl())
        .thenAnswer(invocation -> handleUrlMap.get(driver.getWindowHandle()));

    // Switch to test_url and assert the URL
    PageObject pageObject =
        new NavigationImpl(mock.getFactory())
            .switchToNewWindowAndLoad("http://www.example2.com", TestLoad.class);
    assertThat(driver.getCurrentUrl(), is(equalTo("http://www.example2.com")));
    assertThat(pageObject.getClass(), is(TestLoad.class));
  }

  @PageMarker.Find(css = "found")
  static class TestLoad extends BasePageObject implements RootPageObject {
    // has to be public for reflection to create instance
    public TestLoad() {}
  }
}
