/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.selenium.appium;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.instanceOf;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.nullValue;
import static org.hamcrest.Matchers.sameInstance;
import static org.mockito.Mockito.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.testng.Assert.assertThrows;
import static org.testng.Assert.expectThrows;
import static utam.core.selenium.appium.MobileDriverAdapter.NATIVE_CONTEXT_HANDLE;
import static utam.core.selenium.appium.MobileDriverAdapter.WEBVIEW_CONTEXT_HANDLE_PREFIX;

import io.appium.java_client.AppiumDriver;
import io.appium.java_client.remote.SupportsContextSwitching;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.openqa.selenium.Platform;
import org.openqa.selenium.TimeoutException;
import org.openqa.selenium.UnsupportedCommandException;
import org.openqa.selenium.WebDriver.Navigation;
import org.openqa.selenium.WebDriver.TargetLocator;
import org.testng.annotations.Test;
import utam.core.MockUtilities;
import utam.core.framework.consumer.UtamError;
import utam.core.framework.context.PlatformType;

/**
 * @author qren
 * @since 230
 */
@SuppressWarnings("rawtypes")
public class MobileDriverAdapterTests {

  private static final String DEFAULT_APP_CONTEXT_TITLE = "Salesforce";
  private static final String DEFAULT_WEBVIEW_TITLE = "Test Application";
  private static final String FIRST_WINDOW_HANDLE = "windowOne";

  @Test
  public void testCreation() {
    MobileDriverAdapter driverAdapter =
        new MockUtilities(AppiumDriver.class).getMobileDriverAdapter();
    assertThat(driverAdapter, is(not(nullValue())));
    assertThat(driverAdapter.getAppiumDriver(), is(instanceOf(AppiumDriver.class)));
    assertThat(driverAdapter.isNativeContext(), is(false));
    assertThat(driverAdapter.getSeleniumDriver(), is(instanceOf(AppiumDriver.class)));
  }

  @Test
  public void testIsNative() {
    MockUtilities mock = new MockUtilities(AppiumDriver.class);
    when(mock.getContextSwitcherMock().getContext()).thenReturn(NATIVE_CONTEXT_HANDLE);
    assertThat(mock.getDriverAdapter().isNativeContext(), is(true));
  }

  @Test
  public void testGetContext() {
    MockUtilities mock = new MockUtilities(AppiumDriver.class);
    when(mock.getContextSwitcherMock().getContext()).thenReturn(NATIVE_CONTEXT_HANDLE);
    mock.getDriverAdapter().getPageContext();
  }

  /** Switching to native context should succeed */
  @Test
  public void testSwitchToNative() {
    ContextTracker tracker = new ContextTracker(WEBVIEW_CONTEXT_HANDLE_PREFIX + "_1");
    MockUtilities mock = new MockUtilities(AppiumDriver.class);
    SupportsContextSwitching driver = mock.getContextSwitcherMock();
    when(driver.context(anyString()))
        .then(
            (arg) -> {
              tracker.currentContext = arg.getArgument(0);
              return driver;
            });
    when(driver.getContext()).thenReturn(tracker.currentContext);

    mock.getDriverAdapter().setPageContextToNative();

    verify(driver, times(1)).context(NATIVE_CONTEXT_HANDLE);
    assertThat(mock.getMobileDriverAdapter().getAppiumDriver(), is(sameInstance(driver)));
    assertThat(tracker.currentContext, is(equalTo(NATIVE_CONTEXT_HANDLE)));
  }

  /** Switching to native context should succeed when the context is already native */
  @Test
  public void testSwitchToNativeAlreadyOnNative() {
    ContextTracker tracker = new ContextTracker(NATIVE_CONTEXT_HANDLE);

    MockUtilities mock = new MockUtilities(AppiumDriver.class);
    SupportsContextSwitching driver = mock.getContextSwitcherMock();
    MobileDriverAdapter provider = mock.getMobileDriverAdapter();
    when(mock.getContextSwitcherMock().getContext()).thenReturn(tracker.currentContext);
    provider.setPageContextToNative();

    verify(driver, times(0)).context(anyString());
    assertThat(provider.getAppiumDriver(), is(sameInstance(driver)));
    assertThat(tracker.currentContext, is(equalTo(NATIVE_CONTEXT_HANDLE)));
  }

  /**
   * Tests that the expectation to switch to one of WebView contexts, negative case: there is no
   * target page switch to
   */
  @Test
  public void testSwitchToWebViewTimeout() {
    String testWebViewHandle = WEBVIEW_CONTEXT_HANDLE_PREFIX + "_1";

    Set<String> contextHandles =
        new HashSet<>(Arrays.asList(NATIVE_CONTEXT_HANDLE, testWebViewHandle));

    MockUtilities mock = new MockUtilities(AppiumDriver.class);
    AppiumDriver driver = mock.getAppiumDriverMock();
    SupportsContextSwitching contextSwitcher = mock.getContextSwitcherMock();
    MobileDriverAdapter provider = mock.getMobileDriverAdapter();
    when(contextSwitcher.getContextHandles()).thenReturn(contextHandles);
    when(contextSwitcher.getContext()).thenReturn(NATIVE_CONTEXT_HANDLE);
    when(driver.getTitle()).thenReturn(DEFAULT_WEBVIEW_TITLE);
    when(contextSwitcher.context(testWebViewHandle)).thenReturn(driver);
    mock.setMobilePlatform(Platform.ANDROID);
    TimeoutException e =
        expectThrows(
            TimeoutException.class, () -> provider.setPageContextToWebView("Nonexistent Title"));
    assertThat(e.getMessage(), containsString("Expected condition failed"));
  }

  /** Tests that the expectation to switch to one of WebView contexts, positive case */
  @Test
  public void testSwitchToWebView() {
    String testWebViewHandle = WEBVIEW_CONTEXT_HANDLE_PREFIX + "_1";
    Set<String> contextHandles =
        new HashSet<>(Arrays.asList(NATIVE_CONTEXT_HANDLE, testWebViewHandle));
    ContextTracker tracker = new ContextTracker(NATIVE_CONTEXT_HANDLE);
    MockUtilities mock = new MockUtilities(AppiumDriver.class);
    AppiumDriver driver = mock.getAppiumDriverMock();
    SupportsContextSwitching contextSwitcher = mock.getContextSwitcherMock();
    MobileDriverAdapter provider = mock.getMobileDriverAdapter();
    when(contextSwitcher.getContextHandles()).thenReturn(contextHandles);
    when(driver.getTitle()).thenReturn(DEFAULT_WEBVIEW_TITLE);
    when(contextSwitcher.context(anyString()))
        .then(
            (arg) -> {
              tracker.currentContext = arg.getArgument(0);
              return driver;
            });
    when(contextSwitcher.getContext()).thenReturn(tracker.currentContext);
    provider.setPageContextToWebView(DEFAULT_WEBVIEW_TITLE);

    verify(contextSwitcher, times(1)).context(testWebViewHandle);
    assertThat(provider.getAppiumDriver(), is(sameInstance(driver)));
    assertThat(tracker.currentContext, is(equalTo(testWebViewHandle)));
  }

  /** Tests that the expectation to switch a WebView context that is currently on, positive case */
  @Test
  public void testSwitchToWebViewAlreadyOnTargetPage() {
    String testWebViewHandle = WEBVIEW_CONTEXT_HANDLE_PREFIX + "_1";

    Set<String> contextHandles =
        new HashSet<>(Arrays.asList(NATIVE_CONTEXT_HANDLE, testWebViewHandle));

    ContextTracker tracker = new ContextTracker(testWebViewHandle);

    MockUtilities mock = new MockUtilities(AppiumDriver.class);
    AppiumDriver driver = mock.getAppiumDriverMock();
    SupportsContextSwitching contextSwitcher = mock.getContextSwitcherMock();
    MobileDriverAdapter provider = mock.getMobileDriverAdapter();
    when(contextSwitcher.getContextHandles()).thenReturn(contextHandles);
    when(driver.getTitle()).thenReturn(DEFAULT_WEBVIEW_TITLE);
    when(contextSwitcher.context(anyString()))
        .then(
            (arg) -> {
              tracker.currentContext = arg.getArgument(0);
              return driver;
            });
    when(contextSwitcher.getContext()).thenReturn(tracker.currentContext);

    provider.setPageContextToWebView(DEFAULT_WEBVIEW_TITLE);
    verify(contextSwitcher, times(2)).getContextHandles();
    verify(contextSwitcher, times(2)).context(anyString());
    assertThat(provider.getAppiumDriver(), is(sameInstance(driver)));
    assertThat(tracker.currentContext, is(equalTo(testWebViewHandle)));
  }

  /** Tests that switch to Bridge.app, positive case */
  @Test
  public void testSwitchToBridge() {
    String testWebViewHandle = WEBVIEW_CONTEXT_HANDLE_PREFIX + "_1";
    Set<String> contextHandles =
        Stream.of(NATIVE_CONTEXT_HANDLE, testWebViewHandle).collect(Collectors.toSet());
    ContextTracker tracker = new ContextTracker(NATIVE_CONTEXT_HANDLE);
    MockUtilities mock = new MockUtilities(AppiumDriver.class);
    AppiumDriver driver = mock.getAppiumDriverMock();
    SupportsContextSwitching contextSwitcher = mock.getContextSwitcherMock();
    MobileDriverAdapter provider = mock.getMobileDriverAdapter();
    when(contextSwitcher.getContextHandles()).thenReturn(contextHandles);
    when(driver.getTitle()).thenReturn(DEFAULT_APP_CONTEXT_TITLE);
    when(contextSwitcher.context(anyString()))
        .then(
            (arg) -> {
              tracker.currentContext = arg.getArgument(0);
              return driver;
            });
    when(contextSwitcher.getContext()).thenReturn(tracker.currentContext);
    mock.setMobilePlatform(Platform.LINUX);
    provider.setPageContextToWebView(DEFAULT_APP_CONTEXT_TITLE);
    verify(contextSwitcher, times(1)).context(testWebViewHandle);
    assertThat(provider.getAppiumDriver(), is(sameInstance(driver)));
    assertThat(tracker.currentContext, is(equalTo(testWebViewHandle)));
  }

  /** Calling setPageContextToWebView with a null title should throw the proper exception */
  @Test
  public void testSwitchToWebWithNullTitleThrows() {
    MockUtilities mock = new MockUtilities(AppiumDriver.class);
    UtamError e =
        expectThrows(UtamError.class, () -> mock.getDriverAdapter().setPageContextToWebView(null));
    assertThat(e.getMessage(), containsString(MobileDriverAdapter.ERR_BRIDGE_TITLE_NULL));
  }

  /**
   * Tests that the expectation to check if there is any WebView context available, negative case
   */
  @Test
  public void testIsAnyWebViewContextAvailableWithNoWebView() {
    MockUtilities mock = new MockUtilities(AppiumDriver.class);
    SupportsContextSwitching driver = mock.getContextSwitcherMock();
    MobileDriverAdapter provider = (MobileDriverAdapter) mock.getDriverAdapter();
    Set<String> contextHandles = new HashSet<>(Collections.singletonList(NATIVE_CONTEXT_HANDLE));
    when(driver.getContextHandles()).thenReturn(contextHandles);
    assertThrows(TimeoutException.class, () -> provider.waitFor(provider::isWebViewAvailable));
  }

  /**
   * Tests that the expectation to check if there is any WebView context available, positive case
   */
  @Test
  public void testIsAnyWebViewContextAvailable() {
    MockUtilities mock = new MockUtilities(AppiumDriver.class);
    SupportsContextSwitching contextSwitcher = mock.getContextSwitcherMock();
    MobileDriverAdapter provider = (MobileDriverAdapter) mock.getDriverAdapter();

    Set<String> contextHandles =
        new HashSet<>(
            Arrays.asList(
                MobileDriverAdapter.NATIVE_CONTEXT_HANDLE,
                MobileDriverAdapter.WEBVIEW_CONTEXT_HANDLE_PREFIX + "_1"));

    when(contextSwitcher.getContextHandles()).thenReturn(contextHandles);
    boolean res = provider.waitFor(provider::isWebViewAvailable);
    assertThat(res, is(equalTo(true)));
  }

  /** Tests that the expectation to switch to one of WebView contexts, positive case */
  @Test
  public void testSwitchToWebViewWait() {
    ContextTracker tracker = new ContextTracker();
    MockUtilities mock = new MockUtilities(AppiumDriver.class);
    AppiumDriver driver = (AppiumDriver) mock.getWebDriverMock();
    SupportsContextSwitching contextSwitcher = mock.getContextSwitcherMock();
    MobileDriverAdapter driverAdapter = (MobileDriverAdapter) mock.getDriverAdapter();

    String testWebViewHandle = MobileDriverAdapter.WEBVIEW_CONTEXT_HANDLE_PREFIX + "_1";

    Set<String> contextHandles =
        new HashSet<>(Arrays.asList(NATIVE_CONTEXT_HANDLE, testWebViewHandle));

    String testWebViewTitle = "Test Application";
    when(contextSwitcher.getContextHandles()).thenReturn(contextHandles);
    when(driver.getTitle()).thenReturn(testWebViewTitle);
    when(contextSwitcher.context(anyString()))
        .then(
            (arg) -> {
              tracker.currentContext = arg.getArgument(0);
              return driver;
            });
    when(contextSwitcher.getContext()).thenReturn(tracker.currentContext);
    AppiumDriver sdriver =
        driverAdapter.waitFor(() -> driverAdapter.switchToWebView(testWebViewTitle));
    assertThat(driver, is(sameInstance(sdriver)));
    assertThat(tracker.currentContext, is(equalTo(testWebViewHandle)));
  }

  /**
   * Tests that the expectation to switch to one of WebView contexts, negative case: there is no
   * target page switch to
   */
  @Test
  public void testSwitchToWebViewWithNoTargetWebView() {
    String testWebViewHandle = MobileDriverAdapter.WEBVIEW_CONTEXT_HANDLE_PREFIX + "_1";

    Set<String> contextHandles =
        new HashSet<>(Arrays.asList(MobileDriverAdapter.NATIVE_CONTEXT_HANDLE, testWebViewHandle));

    MockUtilities mock = new MockUtilities(AppiumDriver.class);
    AppiumDriver driver = mock.getAppiumDriverMock();
    SupportsContextSwitching contextSwitcher = mock.getContextSwitcherMock();

    when(contextSwitcher.getContextHandles()).thenReturn(contextHandles);
    when(contextSwitcher.getContext()).thenReturn(MobileDriverAdapter.NATIVE_CONTEXT_HANDLE);
    when(driver.getTitle()).thenReturn("Test Application");
    when(contextSwitcher.context(testWebViewHandle)).thenReturn(driver);
    MobileDriverAdapter adapter = mock.getMobileDriverAdapter();
    TimeoutException e =
        expectThrows(
            TimeoutException.class,
            () -> adapter.waitFor(() -> adapter.switchToWebView("Test Application 2")));
    assertThat(e.getMessage(), containsString("Expected condition failed"));
  }

  /**
   * Tests that the expectation to switch to one of WebView contexts when there multiple WebViews on
   * iOS platform, positive case
   */
  @Test
  public void testSwitchToWebViewWithMultipleWebViewsiOS() {
    ContextTracker tracker = new ContextTracker();

    MockUtilities mock = new MockUtilities(AppiumDriver.class);
    AppiumDriver driver = mock.getAppiumDriverMock();
    SupportsContextSwitching contextSwitcher = mock.getContextSwitcherMock();

    String testWebViewHandle = MobileDriverAdapter.WEBVIEW_CONTEXT_HANDLE_PREFIX + "_1";
    String testWebViewHandle2 = MobileDriverAdapter.WEBVIEW_CONTEXT_HANDLE_PREFIX + "_2";

    Set<String> contextHandles =
        new HashSet<>(
            Arrays.asList(
                MobileDriverAdapter.NATIVE_CONTEXT_HANDLE, testWebViewHandle, testWebViewHandle2));

    String testWebViewTitle = "Test Application";
    when(driver.getCapabilities().getPlatformName()).thenReturn(Platform.IOS);
    when(contextSwitcher.getContextHandles()).thenReturn(contextHandles);
    when(contextSwitcher.context(anyString()))
        .then(
            (arg) -> {
              tracker.currentContext = arg.getArgument(0);
              return driver;
            });
    when(driver.getTitle()).thenReturn("").thenReturn(testWebViewTitle);
    when(contextSwitcher.getContext()).thenReturn(tracker.currentContext);
    MobileDriverAdapter adapter = mock.getMobileDriverAdapter();
    assertThat(
        adapter.waitFor(() -> adapter.switchToWebView(testWebViewTitle)), is(sameInstance(driver)));
    assertThat(tracker.currentContext, is(equalTo(testWebViewHandle)));
  }

  /**
   * Tests that the expectation to switch to one of WebView windows when there multiple WebViews on
   * Android platform, positive case
   */
  @Test
  public void testSwitchToWebViewWithMultipleWebViewsAndroid() {
    ContextTracker contextTracker = new ContextTracker();
    WindowHandleTracker windowHandleTracker = new WindowHandleTracker();

    MockUtilities mock = new MockUtilities(AppiumDriver.class);
    AppiumDriver driver = mock.getAppiumDriverMock();
    SupportsContextSwitching contextSwitcher = mock.getContextSwitcherMock();
    TargetLocator mockLocator = mock(TargetLocator.class);

    String testWebViewHandle = MobileDriverAdapter.WEBVIEW_CONTEXT_HANDLE_PREFIX + "_1";
    String testWindowHandle = MobileDriverAdapter.WEBVIEW_CONTEXT_HANDLE_PREFIX + "_2";
    String testWindowHandle2 = MobileDriverAdapter.WEBVIEW_CONTEXT_HANDLE_PREFIX + "_3";

    Set<String> contextHandles =
        new HashSet<>(Arrays.asList(MobileDriverAdapter.NATIVE_CONTEXT_HANDLE, testWebViewHandle));
    Set<String> windowHandles =
        new HashSet<>(
            Arrays.asList(
                MobileDriverAdapter.NATIVE_CONTEXT_HANDLE, testWindowHandle, testWindowHandle2));

    String testWebViewTitle = "Test Application";
    when(driver.getCapabilities().getPlatformName()).thenReturn(Platform.ANDROID);
    when(contextSwitcher.getContextHandles()).thenReturn(contextHandles);
    when(contextSwitcher.context(anyString()))
        .then(
            (arg) -> {
              contextTracker.currentContext = arg.getArgument(0);
              return driver;
            });
    when(driver.switchTo()).thenReturn(mockLocator);
    when(mockLocator.window(anyString()))
        .then(
            (arg) -> {
              windowHandleTracker.currentHandle = arg.getArgument(0);
              return driver;
            });
    when(driver.getWindowHandles()).thenReturn(windowHandles);
    when(driver.getTitle()).thenReturn("").thenReturn(testWebViewTitle);
    when(contextSwitcher.getContext())
        .thenReturn(contextTracker.currentContext)
        .thenReturn(testWebViewHandle);
    mock.setMobilePlatform(Platform.LINUX);
    MobileDriverAdapter adapter = mock.getMobileDriverAdapter();
    assertThat(
        adapter.waitFor(() -> adapter.switchToWebView(testWebViewTitle)), is(sameInstance(driver)));
    assertThat(windowHandleTracker.currentHandle, is(equalTo(testWindowHandle)));
  }

  /**
   * Tests that the expectation to switch to no windows and not attempt to get handles when only
   * native context exists on Android platform, negative case
   */
  @Test
  public void testSwitchToWebViewWithNoWebViewsAndroid() {
    ContextTracker contextTracker = new ContextTracker();

    MockUtilities mock = new MockUtilities(AppiumDriver.class);
    AppiumDriver driver = mock.getAppiumDriverMock();
    SupportsContextSwitching contextSwitcher = mock.getContextSwitcherMock();

    Set<String> contextHandles = new HashSet<>(List.of(NATIVE_CONTEXT_HANDLE));

    String testWebViewTitle = "Test Application";
    when(driver.getCapabilities().getPlatformName()).thenReturn(Platform.ANDROID);
    when(contextSwitcher.getContextHandles()).thenReturn(contextHandles);
    when(driver.getWindowHandles())
        .thenThrow(new UnsupportedCommandException("getWindowHandles {}"));
    when(contextSwitcher.getContext()).thenReturn(contextTracker.currentContext);
    mock.setMobilePlatform(Platform.LINUX);
    MobileDriverAdapter adapter = mock.getMobileDriverAdapter();
    assertThat(adapter.switchToWebView(testWebViewTitle), nullValue());
    assertThat(contextTracker.currentContext, is(equalTo(NATIVE_CONTEXT_HANDLE)));
  }

  @Test
  public void testSetPageContext() {
    MockUtilities mock = new MockUtilities(AppiumDriver.class);
    ContextTracker tracker = new ContextTracker(WEBVIEW_CONTEXT_HANDLE_PREFIX + "_1");
    AppiumDriver driver = mock.getAppiumDriverMock();
    SupportsContextSwitching contextSwitcher = mock.getContextSwitcherMock();
    when(contextSwitcher.context(anyString()))
        .then(
            (arg) -> {
              tracker.currentContext = arg.getArgument(0);
              return driver;
            });
    when(contextSwitcher.getContext()).thenReturn(tracker.currentContext);
    mock.getDriverAdapter().setPageContext(PlatformType.NATIVE);
  }

  @Test
  public void testBackNavigation() {
    MockUtilities mock = new MockUtilities(AppiumDriver.class);
    Navigation navigationMock = mock(Navigation.class);
    AppiumDriver driver = mock.getAppiumDriverMock();
    when(driver.navigate()).thenReturn(navigationMock);
    mock.getDriverAdapter().back();
    verify(navigationMock, times(1)).back();
  }

  @Test
  public void testForwardNavigation() {
    MockUtilities mock = new MockUtilities(AppiumDriver.class);
    Navigation navigationMock = mock(Navigation.class);
    AppiumDriver driver = mock.getAppiumDriverMock();
    when(driver.navigate()).thenReturn(navigationMock);
    mock.getDriverAdapter().forward();
    verify(navigationMock, times(1)).forward();
  }

  private static class ContextTracker {

    private String currentContext;

    ContextTracker(String initialContext) {
      currentContext = initialContext;
    }

    ContextTracker() {
      currentContext = NATIVE_CONTEXT_HANDLE;
    }
  }

  private static class WindowHandleTracker {

    private String currentHandle;

    WindowHandleTracker() {
      currentHandle = FIRST_WINDOW_HANDLE;
    }
  }
}
