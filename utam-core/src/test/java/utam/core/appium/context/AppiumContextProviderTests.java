package utam.core.appium.context;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;
import static org.mockito.Mockito.*;
import static org.testng.Assert.expectThrows;
import org.testng.annotations.Test;
import utam.core.framework.consumer.UtamError;
import java.time.Duration;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.TimeoutException;
import org.openqa.selenium.WebDriver;

import io.appium.java_client.AppiumDriver;
import io.appium.java_client.android.AndroidDriver;
import io.appium.java_client.ios.IOSDriver;

/**
 * Tests for AppiumContextProvider
 * @author qren
 * @since 230
 */
@SuppressWarnings("rawtypes")
public class AppiumContextProviderTests {
  
  private static final String DEFAULT_APP_CONTEXT_TITLE = "Salesforce";
  private static final String DEFAULT_WEBVIEW_TITLE = "Test Application";

  private static AppiumDriver getDriver() { 
    return mock(
        AppiumDriver.class,
        withSettings().extraInterfaces(JavascriptExecutor.class));
  }
  
  private static AppiumContextProvider getProvider(AppiumDriver driver) {
    return new AppiumContextProvider(
        driver,
        Duration.ofMillis(10),
        DEFAULT_APP_CONTEXT_TITLE);
  }

  /** 
   * The no-timeout constructor should produce an object with default timeouts
   */
  @Test
  public void testProviderFromDriver() {
    AppiumContextProvider provider = new AppiumContextProvider(getDriver());
    assertThat(provider, is(not(nullValue())));
  }
  /** 
   * The no-timeout constructor should produce an object with default timeouts
   */
  @Test
  public void testProviderDefaultTimeouts() {
    AppiumContextProvider provider = new AppiumContextProvider(
        getDriver(),
        DEFAULT_APP_CONTEXT_TITLE);
    assertThat(provider.getPollingTimeout().getSeconds(), is(equalTo(20L)));
  }

  /** 
   * The method to get Appium driver utilities should return an instance 
   * of AppiumDriverUtilities
   */
  @Test
  public void testGetAppiumDriverUtils() {
    assertThat(
        getProvider(getDriver()).getWebDriverUtils(),
        is(instanceOf(AppiumDriverUtilities.class)));
  }

  /** 
   * The method to get a iOS Driver object should return an instance of WebDriver
   */
  @Test
  public void testGetIOSDriver() {
    AppiumDriver driver =
        mock(
            IOSDriver.class,
            withSettings().extraInterfaces(JavascriptExecutor.class));
    AppiumContextProvider provider = getProvider(driver);
    assertThat(provider.getWebDriver(), is(instanceOf(IOSDriver.class)));
  }

  /** 
   * The method to get a Android Driver object should return an instance of WebDriver
   */
  @Test
  public void testGetAndroidDriver() {
    AppiumDriver driver =
        mock(
            AndroidDriver.class,
            withSettings().extraInterfaces(JavascriptExecutor.class));
    AppiumContextProvider provider = getProvider(driver);
    assertThat(provider.getWebDriver(), is(instanceOf(AndroidDriver.class)));
  }

  /** 
   * Switching to native context should succeed
   */
  @Test
  public void testSwitchToNative() {
    ContextTracker tracker = new ContextTracker(
            AppiumContextProvider.WEBVIEW_CONTEXT_HANDLE_PREFIX + "_1");
    
    AppiumDriver driver = getDriver();
    when(driver.context(anyString())).then((arg) -> {
      tracker.currentContext = arg.getArgument(0);
      return driver;
    });
    when(driver.getContext()).thenReturn(tracker.currentContext);
    
    AppiumContextProvider provider = getProvider(driver);
    WebDriver returnValue = provider.setPageContextToNative();
    
    verify(driver, times(1)).context(AppiumContextProvider.NATIVE_CONTEXT_HANDLE);
    assertThat(returnValue, is(sameInstance(driver)));
    assertThat(
        tracker.currentContext,
        is(equalTo(AppiumContextProvider.NATIVE_CONTEXT_HANDLE)));
  }

  /** 
   * Switching to native context should succeed when the context is already native
   */
  @Test
  public void testSwitchToNativeAlreadyOnNative() {
    ContextTracker tracker = new ContextTracker(
            AppiumContextProvider.NATIVE_CONTEXT_HANDLE);
    
    AppiumDriver driver = getDriver();
    when(driver.getContext()).thenReturn(tracker.currentContext);
    
    AppiumContextProvider provider = getProvider(driver);
    WebDriver returnValue = provider.setPageContextToNative();
    
    verify(driver, times(0)).context(anyString());
    assertThat(returnValue, is(sameInstance(driver)));
    assertThat(
        tracker.currentContext,
        is(equalTo(AppiumContextProvider.NATIVE_CONTEXT_HANDLE)));
  }

  /* Tests that the expectation to switch to one of WebView contexts */
  /** negative case: there is no target page switch to */
  @Test
  public void testSwitchToWebViewTimeout() {
    String testWebViewHandle = 
        AppiumContextProvider.WEBVIEW_CONTEXT_HANDLE_PREFIX + "_1";

    Set<String> contextHandles = new HashSet<>(Arrays.asList(AppiumContextProvider.NATIVE_CONTEXT_HANDLE,
            testWebViewHandle));
    
    AppiumDriver driver = getDriver();
    AppiumContextProvider provider = getProvider(driver);
    provider.setPollingInterval(Duration.ofMillis(1));
    when(driver.getContextHandles()).thenReturn(contextHandles);
    when(driver.getContext()).thenReturn(AppiumContextProvider.NATIVE_CONTEXT_HANDLE);
    when(driver.getTitle()).thenReturn(DEFAULT_WEBVIEW_TITLE);
    when(driver.context(testWebViewHandle)).thenReturn(driver);
    
    TimeoutException e = expectThrows(
        TimeoutException.class,
        () -> provider.setPageContextToWebView("Nonexistent Title"));
    assertThat(e.getMessage(), containsString("Expected condition failed"));
  }

  /* Tests that the expectation to switch to one of WebView contexts */
  /** positive case */
  @Test
  public void testSwitchToWebView() {
    String testWebViewHandle = 
        AppiumContextProvider.WEBVIEW_CONTEXT_HANDLE_PREFIX + "_1";

    Set<String> contextHandles = new HashSet<>(Arrays.asList(AppiumContextProvider.NATIVE_CONTEXT_HANDLE,
            testWebViewHandle));

    ContextTracker tracker = new ContextTracker(
            AppiumContextProvider.NATIVE_CONTEXT_HANDLE);
    
    AppiumDriver driver = getDriver();
    when(driver.getContextHandles()).thenReturn(contextHandles);
    when(driver.getTitle()).thenReturn(DEFAULT_WEBVIEW_TITLE);
    when(driver.context(anyString())).then((arg) -> {
      tracker.currentContext = arg.getArgument(0);
      return driver;
    });
    when(driver.getContext()).thenReturn(tracker.currentContext);
    
    AppiumContextProvider provider = getProvider(driver);
    WebDriver returnValue = provider.setPageContextToWebView(DEFAULT_WEBVIEW_TITLE);
    
    verify(driver, times(1)).context(testWebViewHandle);
    assertThat(returnValue, is(sameInstance(driver)));
    assertThat(
        tracker.currentContext,
        is(equalTo(testWebViewHandle)));
  }

  /* Tests that the expectation to switch a WebView context that is currently on */
  /** positive case */
  @Test
  public void testSwitchToWebViewAlreadyOnTargetPage() {
    String testWebViewHandle = 
        AppiumContextProvider.WEBVIEW_CONTEXT_HANDLE_PREFIX + "_1";

    Set<String> contextHandles = new HashSet<>(Arrays.asList(AppiumContextProvider.NATIVE_CONTEXT_HANDLE,
            testWebViewHandle));

    ContextTracker tracker = new ContextTracker(
            testWebViewHandle);
    
    AppiumDriver driver = getDriver();
    when(driver.getContextHandles()).thenReturn(contextHandles);
    when(driver.getTitle()).thenReturn(DEFAULT_WEBVIEW_TITLE);
    when(driver.getContext()).thenReturn(tracker.currentContext);
    
    AppiumContextProvider provider = getProvider(driver);
    WebDriver returnValue = provider.setPageContextToWebView(DEFAULT_WEBVIEW_TITLE);
    
    verify(driver, times(0)).context(anyString());
    assertThat(returnValue, is(sameInstance(driver)));
    assertThat(
        tracker.currentContext,
        is(equalTo(testWebViewHandle)));
  }

  /* Tests that switch to Bridge.app */
  /** positive case */
  @Test
  public void testSwitchToBridge() {
    String testWebViewHandle = 
        AppiumContextProvider.WEBVIEW_CONTEXT_HANDLE_PREFIX + "_1";

    Set<String> contextHandles = new HashSet<>(Arrays.asList(AppiumContextProvider.NATIVE_CONTEXT_HANDLE,
            testWebViewHandle));

    ContextTracker tracker = new ContextTracker(
            AppiumContextProvider.NATIVE_CONTEXT_HANDLE);
    
    AppiumDriver driver = getDriver();
    when(driver.getContextHandles()).thenReturn(contextHandles);
    when(driver.getTitle()).thenReturn(DEFAULT_APP_CONTEXT_TITLE);
    when(driver.executeScript("return window.aura.finishedInit"))
        .thenReturn(Boolean.TRUE.toString());
    when(driver.context(anyString())).then((arg) -> {
      tracker.currentContext = arg.getArgument(0);
      return driver;
    });
    when(driver.getContext()).thenReturn(tracker.currentContext);
    
    AppiumContextProvider provider = getProvider(driver);
    WebDriver returnValue = provider.setPageContextToWebView();
    
    verify(driver, times(1)).context(testWebViewHandle);
    assertThat(returnValue, is(sameInstance(driver)));
    assertThat(
        tracker.currentContext,
        is(equalTo(testWebViewHandle)));
  }

  /**
   * Calling setPageContextToWebView with a null title should throw the proper exception
   */
  @Test
  public void testSwitchToWebWithNullTitleThrows() {
    AppiumDriver driver = getDriver();
    AppiumContextProvider provider = getProvider(driver);
    UtamError e = expectThrows(
            UtamError.class, () -> provider.setPageContextToWebView(null));
    assertThat(
        e.getMessage(),
        containsString(AppiumContextProvider.ERR_BRIDGE_TITLE_NULL));
  }

  private static class ContextTracker {
    private String currentContext;

    ContextTracker(String initialContext) {
      currentContext = initialContext;
    }
  }
}
