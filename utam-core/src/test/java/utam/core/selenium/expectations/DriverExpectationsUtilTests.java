/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.selenium.expectations;

import org.openqa.selenium.Alert;
import org.openqa.selenium.TimeoutException;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebDriver.TargetLocator;
import org.openqa.selenium.WebDriverException;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import utam.core.appium.context.AppiumContextProvider;
import io.appium.java_client.AppiumDriver;
import utam.core.selenium.context.SeleniumContextProvider;

import java.time.Duration;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.testng.Assert.expectThrows;

/**
 * Tests for the DriverExpectationsUtil class
 *
 * @author james.evans
 */
@SuppressWarnings("rawtypes")
public class DriverExpectationsUtilTests {

  private static final String TEST_URL = "https://utam.dev/rocks/";
  private static final String FIRST_WINDOW_HANDLE = "windowOne";
  private static final String SECOND_WINDOW_HANDLE = "windowTwo";

  private WebDriver mockDriver;
  private TargetLocator mockTargetLocator;

  @BeforeMethod
  public void setUp() {
    mockTargetLocator = mock(TargetLocator.class);
    mockDriver = mock(WebDriver.class);
    when(mockDriver.getCurrentUrl()).thenReturn(TEST_URL);
    when(mockDriver.switchTo()).thenReturn(mockTargetLocator);
  }

  /** Tests that the getUrl method calls the WebDriver getCurrentUrl method */
  @Test
  public void testGetUrl() {
    SeleniumContextProvider provider = new SeleniumContextProvider(mockDriver);
    DriverWait wait = provider.getDriverWait();
    assertThat(wait.get(DriverExpectationsUtil.getUrl()), is(equalTo(TEST_URL)));
  }

  /** Tests that the setUrl method returns the driver object */
  @Test
  public void testSetUrl() {
    SeleniumContextProvider provider = new SeleniumContextProvider(mockDriver);
    DriverWait wait = provider.getDriverWait();
    assertThat(wait.get(DriverExpectationsUtil.setUrl(TEST_URL)), is(equalTo(mockDriver)));
  }

  /** Tests that the alert method will wait to switch to an active alert */
  @Test
  public void testAlert() {
    Alert mockAlert = mock(Alert.class);
    when(mockTargetLocator.alert()).thenReturn(mockAlert);
    SeleniumContextProvider provider = new SeleniumContextProvider(mockDriver);
    DriverWait wait = provider.getDriverWait();
    assertThat(wait.get(DriverExpectationsUtil.alert()), is(instanceOf(Alert.class)));
  }

  /** Tests that the switchToTab method will wait for the driver to switch to a new tab */
  @SuppressWarnings("unchecked")
  @Test
  public void testSwitchToTab() {
    WindowHandleTracker tracker = new WindowHandleTracker();
    Set<String> originalWindows = new HashSet<>();
    Collections.addAll(originalWindows, FIRST_WINDOW_HANDLE);
    Set<String> newWindows = new HashSet<>();
    Collections.addAll(newWindows, FIRST_WINDOW_HANDLE, SECOND_WINDOW_HANDLE);
    when(mockDriver.getWindowHandles()).thenReturn(originalWindows, newWindows);
    when(mockTargetLocator.window(any()))
        .then(
            (invocation) -> {
              tracker.setCurrentHandle(invocation.getArgument(0));
              return mockDriver;
            });
    when(mockDriver.getWindowHandle()).then((invocation) -> tracker.getCurrentHandle());

    SeleniumContextProvider provider = new SeleniumContextProvider(mockDriver);
    DriverWait wait = provider.getDriverWait();
    assertThat(wait.get(DriverExpectationsUtil.switchToTab()), is(equalTo(mockDriver)));
    assertThat(mockDriver.getWindowHandle(), is(equalTo(SECOND_WINDOW_HANDLE)));
  }

  /** Tests that the switchToTab method will wait and timeout if no new tab appears */
  @SuppressWarnings("unchecked")
  @Test
  public void testSwitchToTabWithNoNewTab() {
    WindowHandleTracker tracker = new WindowHandleTracker();
    Set<String> originalWindows = new HashSet<>();
    Collections.addAll(originalWindows, FIRST_WINDOW_HANDLE);
    Set<String> newWindows = new HashSet<>();
    Collections.addAll(newWindows, FIRST_WINDOW_HANDLE);
    when(mockDriver.getWindowHandles()).thenReturn(originalWindows, newWindows);
    when(mockDriver.getWindowHandle()).then((invocation) -> tracker.getCurrentHandle());

    SeleniumContextProvider provider = new SeleniumContextProvider(mockDriver);
    provider.setPollingTimeout(Duration.ofMillis(10));
    provider.setPollingInterval(Duration.ofMillis(1));
    DriverWait wait = provider.getDriverWait();

    WebDriverException e =
        expectThrows(
            WebDriverException.class, () -> wait.get(DriverExpectationsUtil.switchToTab()));
    assertThat(
        e.getMessage(), containsString("waiting for utam.core.selenium.context.SeleniumContextProvider"));
    assertThat(mockDriver.getWindowHandle(), is(equalTo(FIRST_WINDOW_HANDLE)));
  }

  /* Tests that the expectation to check if there is any WebView context available */
  /** negative case */
  @Test
  public void testIsAnyWebViewContextAvailableWithNoWebView() {
    AppiumDriver mockAppiumDriver = mock(AppiumDriver.class);
    AppiumContextProvider provider = new AppiumContextProvider(
        mockAppiumDriver, 
        Duration.ofMillis(10), 
        "Salesforce");
    provider.setPollingInterval(Duration.ofMillis(1));

    Set<String> contextHandles = new HashSet<>(Collections.singletonList(AppiumContextProvider.NATIVE_CONTEXT_HANDLE));
    
    when(mockAppiumDriver.getContextHandles()).thenReturn(contextHandles);
    
    DriverWait wait = provider.getDriverWait();
    TimeoutException e = expectThrows(
        TimeoutException.class,
        () -> wait.get(DriverExpectationsUtil.isAnyWebViewContextAvailable()));
    assertThat(
        e.getMessage(), 
        containsString("waiting for utam.core.selenium.context.SeleniumContextProvider"));
  }

  /* Tests that the expectation to check if there is any WebView context available */
  /** positive case */
  @Test
  public void testIsAnyWebViewContextAvailable() {
    AppiumDriver  mockAppiumDriver = mock(AppiumDriver.class);
    AppiumContextProvider provider = new AppiumContextProvider(
        mockAppiumDriver, 
        "Salesforce");

    Set<String> contextHandles = new HashSet<>(Arrays.asList(AppiumContextProvider.NATIVE_CONTEXT_HANDLE,
            AppiumContextProvider.WEBVIEW_CONTEXT_HANDLE_PREFIX + "_1"));
    
    when(mockAppiumDriver.getContextHandles()).thenReturn(contextHandles);
    
    DriverWait wait = provider.getDriverWait();
    assertThat(wait.get(
        DriverExpectationsUtil.isAnyWebViewContextAvailable()), 
        is(equalTo(true)));
  }

  /* Tests that the expectation to switch to one of WebView contexts */
  /** positive case */
  @Test
  public void testSwitchToWebView() {
    ContextTracker tracker = new ContextTracker();

    AppiumDriver  mockAppiumDriver = mock(AppiumDriver.class);
    AppiumContextProvider provider = new AppiumContextProvider(
        mockAppiumDriver, 
        "Salesforce");

    String testWebViewHandle = 
        AppiumContextProvider.WEBVIEW_CONTEXT_HANDLE_PREFIX + "_1";

    Set<String> contextHandles = new HashSet<>(Arrays.asList(AppiumContextProvider.NATIVE_CONTEXT_HANDLE,
            testWebViewHandle));

    String testWebViewTitle =  "Test Application";
    when(mockAppiumDriver.getContextHandles())
        .thenReturn(contextHandles);
    when(mockAppiumDriver.getTitle())
        .thenReturn(testWebViewTitle);
    when(mockAppiumDriver.context(anyString())).then((arg) -> {
      tracker.currentContext = arg.getArgument(0);
      return mockAppiumDriver;
    });
    when(mockAppiumDriver.getContext())
        .thenReturn(tracker.currentContext);

    DriverWait wait = provider.getDriverWait();
    assertThat(
        wait.get(DriverExpectationsUtil.switchToWebView(testWebViewTitle)), 
        is(sameInstance(mockAppiumDriver)));
    assertThat(
        tracker.currentContext, 
        is(equalTo(testWebViewHandle)));
  }

  /* Tests that the expectation to switch to one of WebView contexts */
  /** negative case: there is no target page switch to */
  @Test
  public void testSwitchToWebViewWithNoTargetWebView() {
    String testWebViewHandle = 
        AppiumContextProvider.WEBVIEW_CONTEXT_HANDLE_PREFIX + "_1";

    Set<String> contextHandles = new HashSet<>(Arrays.asList(AppiumContextProvider.NATIVE_CONTEXT_HANDLE,
            testWebViewHandle));
    
    AppiumDriver mockAppiumDriver = mock(AppiumDriver.class);
    AppiumContextProvider provider = new AppiumContextProvider(
        mockAppiumDriver, 
        Duration.ofMillis(10), 
        "Salesforce");
    provider.setPollingInterval(Duration.ofMillis(1));

    when(mockAppiumDriver.getContextHandles())
        .thenReturn(contextHandles);
    when(mockAppiumDriver.getContext())
        .thenReturn(AppiumContextProvider.NATIVE_CONTEXT_HANDLE);
    when(mockAppiumDriver.getTitle())
        .thenReturn("Test Application");
    when(mockAppiumDriver.context(testWebViewHandle))
        .thenReturn(mockAppiumDriver);

    DriverWait wait = provider.getDriverWait();
    TimeoutException e = expectThrows(
        TimeoutException.class,
        () -> wait.get(DriverExpectationsUtil.switchToWebView("Test Application 2")));
    assertThat(e.getMessage(), containsString("Expected condition failed"));
  }

  /* Tests that the expectation to switch to one of WebView contexts */
  /** negative case: there is no target page switch to */
  @Test
  public void testSwitchToWebViewWithNativeOnly() {
    Set<String> contextHandles = new HashSet<>(Collections.singletonList(AppiumContextProvider.NATIVE_CONTEXT_HANDLE));
    
    AppiumDriver mockAppiumDriver = mock(AppiumDriver.class);
    AppiumContextProvider provider = new AppiumContextProvider(
        mockAppiumDriver, 
        Duration.ofMillis(10), 
        "Salesforce");
    provider.setPollingInterval(Duration.ofMillis(1));

    when(mockAppiumDriver.getContextHandles()).thenReturn(contextHandles);
    when(mockAppiumDriver.getContext())
        .thenReturn(AppiumContextProvider.NATIVE_CONTEXT_HANDLE);
    
    DriverWait wait = provider.getDriverWait();
    TimeoutException e = expectThrows(
        TimeoutException.class,
        () -> wait.get(DriverExpectationsUtil.switchToWebView("Test Application")));
    assertThat(e.getMessage(), containsString("Expected condition failed"));
  }

  /* Tests that the expectation to switch to one of WebView contexts 
   * when there multiple WebViews on iOS platform*/
  /** positive case */
  @Test
  public void testSwitchToWebViewWithMultipleWebViewsiOS() {
    ContextTracker tracker = new ContextTracker();

    AppiumDriver  mockAppiumDriver = mock(AppiumDriver.class);
    AppiumContextProvider provider = new AppiumContextProvider(
        mockAppiumDriver, 
        "Salesforce");

    String testWebViewHandle = 
        AppiumContextProvider.WEBVIEW_CONTEXT_HANDLE_PREFIX + "_1";
    String testWebViewHandle2 = 
            AppiumContextProvider.WEBVIEW_CONTEXT_HANDLE_PREFIX + "_2";

    Set<String> contextHandles = new HashSet<>(Arrays.asList(AppiumContextProvider.NATIVE_CONTEXT_HANDLE,
            testWebViewHandle, testWebViewHandle2));

    String testWebViewTitle =  "Test Application";
    when(mockAppiumDriver.getPlatformName())
    .thenReturn("ios");
    when(mockAppiumDriver.getContextHandles())
        .thenReturn(contextHandles);
    when(mockAppiumDriver.context(anyString())).then((arg) -> {
        tracker.currentContext = arg.getArgument(0);
        return mockAppiumDriver;
    });
    when(mockAppiumDriver.getTitle())
        .thenReturn("")
        .thenReturn(testWebViewTitle);
    when(mockAppiumDriver.getContext())
        .thenReturn(tracker.currentContext);

    DriverWait wait = provider.getDriverWait();
    assertThat(
        wait.get(DriverExpectationsUtil.switchToWebView(testWebViewTitle)), 
        is(sameInstance(mockAppiumDriver)));
    assertThat(
        tracker.currentContext, 
        is(equalTo(testWebViewHandle)));
  }

  /* Tests that the expectation to switch to one of WebView windows 
   * when there multiple WebViews on Android platform*/
  /** positive case */
  @Test
  public void testSwitchToWebViewWithMultipleWebViewsAndroid() {
    ContextTracker contextTracker = new ContextTracker();
    WindowHandleTracker windowHandleTracker = new WindowHandleTracker();

    AppiumDriver  mockAppiumDriver = mock(AppiumDriver.class);
    TargetLocator mockLocator = mock(TargetLocator.class);
    AppiumContextProvider provider = new AppiumContextProvider(
        mockAppiumDriver, 
        "Salesforce");

    String testWebViewHandle = 
        AppiumContextProvider.WEBVIEW_CONTEXT_HANDLE_PREFIX + "_1";
    String testWindowHandle = 
            AppiumContextProvider.WEBVIEW_CONTEXT_HANDLE_PREFIX + "_2";
    String testWindowHandle2 = 
            AppiumContextProvider.WEBVIEW_CONTEXT_HANDLE_PREFIX + "_3";

    Set<String> contextHandles = new HashSet<>(Arrays.asList(AppiumContextProvider.NATIVE_CONTEXT_HANDLE,
            testWebViewHandle));
    Set<String> windowHandles = new HashSet<>(Arrays.asList(AppiumContextProvider.NATIVE_CONTEXT_HANDLE,
            testWindowHandle, testWindowHandle2));

    String testWebViewTitle =  "Test Application";
    when(mockAppiumDriver.getPlatformName())
    .thenReturn("android");
    when(mockAppiumDriver.getContextHandles())
        .thenReturn(contextHandles);
    when(mockAppiumDriver.context(anyString())).then((arg) -> {
        contextTracker.currentContext = arg.getArgument(0);
        return mockAppiumDriver;
    });
    when(mockAppiumDriver.context(anyString())).then((arg) -> {
        contextTracker.currentContext = arg.getArgument(0);
        return mockAppiumDriver;
    });
    when(mockAppiumDriver.switchTo()).thenReturn(mockLocator);
    when(mockLocator.window(anyString())).then((arg) -> {
        windowHandleTracker.currentHandle = arg.getArgument(0);
        return mockAppiumDriver;
    });
    when(mockAppiumDriver.getWindowHandles())
    .thenReturn(windowHandles);
    when(mockAppiumDriver.getTitle())
        .thenReturn("")
        .thenReturn(testWebViewTitle);
    when(mockAppiumDriver.getContext())
        .thenReturn(contextTracker.currentContext);

    DriverWait wait = provider.getDriverWait();
    assertThat(
        wait.get(DriverExpectationsUtil.switchToWebView(testWebViewTitle)), 
        is(sameInstance(mockAppiumDriver)));
    assertThat(
        windowHandleTracker.currentHandle,
        is(equalTo(testWindowHandle)));
  }

  private static class ContextTracker {
      private String currentContext;
      ContextTracker() {
          currentContext = AppiumContextProvider.NATIVE_CONTEXT_HANDLE;
        }
  }

  private static class WindowHandleTracker {
    private String currentHandle;

    WindowHandleTracker() {
      currentHandle = DriverExpectationsUtilTests.FIRST_WINDOW_HANDLE;
    }

    String getCurrentHandle() {
      return currentHandle;
    }

    void setCurrentHandle(String handle) {
      currentHandle = handle;
    }
  }
}
