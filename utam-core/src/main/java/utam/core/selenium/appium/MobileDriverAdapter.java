/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.selenium.appium;

import static utam.core.framework.UtamLogger.error;
import static utam.core.framework.UtamLogger.info;
import static utam.core.framework.UtamLogger.warning;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.appium.java_client.AppiumDriver;
import io.appium.java_client.remote.SupportsContextSwitching;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Set;
import java.util.function.Supplier;
import org.openqa.selenium.By;
import org.openqa.selenium.NoSuchWindowException;
import org.openqa.selenium.Platform;
import org.openqa.selenium.WebDriverException;
import org.openqa.selenium.WebElement;
import utam.core.driver.Driver;
import utam.core.driver.DriverConfig;
import utam.core.element.Element;
import utam.core.framework.consumer.UtamError;
import utam.core.framework.context.MobilePlatformType;
import utam.core.framework.context.PlatformType;
import utam.core.selenium.element.DriverAdapter;

/**
 * Appium Driver wrapper
 *
 * @author qren
 * @since 232
 */
@SuppressWarnings("rawtypes")
public class MobileDriverAdapter extends DriverAdapter implements Driver {

  static final String WEBVIEW_CONTEXT_HANDLE_PREFIX = "WEBVIEW";
  static final String WEBVIEW_CONTEXT_KEY_ANDROID = "webviewName";
  static final String WEBVIEW_CONTEXT_KEY_IOS = "id";
  static final String WEBVIEW_PAGES_KEY = "pages";
  static final String WEBVIEW_PAGE_KEY = "id";
  static final String WEBVIEW_PAGE_DESCRIPTION_KEY = "description";
  static final String WEBVIEW_PAGE_DESCRIPTION_VISIBILITY_KEY = "visible";
  static final String WEBVIEW_TITLE_KEY_ANDROID = "title";
  static final String WEBVIEW_TITLE_KEY_IOS = "title";
  static final String NATIVE_CONTEXT_HANDLE = "NATIVE_APP";
  static final String ERR_BRIDGE_TITLE_NULL = "Bridge application title is null, please configure";

  private final MobilePlatformType mobilePlatform;

  /**
   * Initializes a new instance of the MobileDriverAdapter class
   *
   * @param driver the driver to use
   * @param driverConfig the driver config to use
   */
  public MobileDriverAdapter(AppiumDriver driver, DriverConfig driverConfig) {
    super(driver, driverConfig);
    this.mobilePlatform = MobilePlatformType.fromDriver(driver);
  }

  static AppiumDriver getAppiumDriver(Driver driver) {
    return ((MobileDriverAdapter) driver).getAppiumDriver();
  }

  final boolean isIOSPlatform() {
    return mobilePlatform == MobilePlatformType.IOS
        || mobilePlatform == MobilePlatformType.IOS_PHONE
        || mobilePlatform == MobilePlatformType.IOS_TABLET;
  }

  final Boolean isWebViewAvailable() {
    AppiumDriver appiumDriver = getAppiumDriver();
    Set<String> contextHandles = ((SupportsContextSwitching) appiumDriver).getContextHandles();
    return contextHandles.stream()
        .anyMatch(handle -> handle.contains(WEBVIEW_CONTEXT_HANDLE_PREFIX));
  }

  final AppiumDriver switchToWebView(String title) {
    AppiumDriver appiumDriver = getAppiumDriver();
    if (!isIOSPlatform()) {
      // Set current context to native to get the updated available contexts
      // Otherwise, the closed webview that is the current context will not be dropped
      // from the return of getContextHandles. This is Android unique.
      setPageContextToNative();
    }
    // https://webdriver.io/docs/api/mobile/getContexts/
    Map<String, Object> args = Map.of("returnDetailedContexts", true);
    Object result = appiumDriver.executeScript("mobile: getContexts", args);
    List<Map<String, Object>> contexts = (List<Map<String, Object>>) result;
    int contextsSize = contexts.size();
    ListIterator<Map<String, Object>> li = contexts.listIterator(contextsSize);
    AppiumDriver newDriver = null;
    // Iterate in reverse since the WebView on screen is often the last context in the list.
    while (li.hasPrevious()) {
      Map<String, Object> context = li.previous();
      if (appiumDriver.getCapabilities().getPlatformName().equals(Platform.ANDROID)
          && context.containsKey(WEBVIEW_CONTEXT_KEY_ANDROID)) {
        newDriver = checkAndroidWebViewContext(appiumDriver, title, context);
      } else if (appiumDriver.getCapabilities().getPlatformName().equals(Platform.IOS)
          && context.containsKey(WEBVIEW_CONTEXT_KEY_IOS)) {
        newDriver = checkIOSWebViewContext(appiumDriver, title, context);
      }
      if (newDriver != null) {
        return newDriver;
      }
    }
    return null;
  }

  /**
   * Check if the Android WebView context is the one with the desired title.
   *
   * @see <a
   *     href="https://github.com/appium/appium-uiautomator2-driver/?tab=readme-ov-file#mobile-getcontexts">Appium
   *     UIAutomator2 Driver</a>
   * @see <a href="https://chromedevtools.github.io/devtools-protocol/">Chrome DevTools Protocol</a>
   * @see <a
   *     href="https://github.com/appium/appium-android-driver/blob/master/lib/commands/types.ts">Appium
   *     Android Driver Types</a>
   * @param appiumDriver the Appium driver instance
   * @param title the desired WebView title to match
   * @param context the context map containing WebView information
   * @return the AppiumDriver if context switch successful, null otherwise
   */
  private static AppiumDriver checkAndroidWebViewContext(
      AppiumDriver appiumDriver, String title, Map<String, Object> context) {
    String webviewName = (String) context.get(WEBVIEW_CONTEXT_KEY_ANDROID);
    if (!context.containsKey(WEBVIEW_PAGES_KEY)) {
      return null;
    }
    List<Map<String, Object>> pages = (List<Map<String, Object>>) context.get(WEBVIEW_PAGES_KEY);
    for (Map<String, Object> page : pages) {
      String newTitle = (String) page.get(WEBVIEW_TITLE_KEY_ANDROID);
      // 'description' doesn't cast to Map, only String
      String serializedDescription = (String) page.get(WEBVIEW_PAGE_DESCRIPTION_KEY);
      boolean visible = false;
      try {
        JsonNode descriptionNode = new ObjectMapper().readTree(serializedDescription);
        if (descriptionNode.has(WEBVIEW_PAGE_DESCRIPTION_VISIBILITY_KEY)) {
          visible = descriptionNode.get(WEBVIEW_PAGE_DESCRIPTION_VISIBILITY_KEY).asBoolean();
        }
      } catch (JsonProcessingException e) {
        error(e);
      }
      if (newTitle.equalsIgnoreCase(title) && visible) {
        String id = (String) page.get(WEBVIEW_PAGE_KEY);
        AppiumDriver newDriver = switchToContext(appiumDriver, webviewName, id);
        if (newDriver != null) {
          return newDriver;
        }
      }
    }
    return null;
  }

  /**
   * Check if the iOS WebView context is the one with the desired title.
   *
   * @see <a
   *     href="https://github.com/appium/appium-xcuitest-driver/blob/master/docs/reference/execute-methods.md#mobile-getcontexts">Appium
   *     XCUITest Driver - Execute Methods</a>
   * @see <a
   *     href="https://github.com/appium/appium-xcuitest-driver/blob/master/lib/commands/types.ts">Appium
   *     XCUITest Driver - Types</a>
   * @param appiumDriver the Appium driver instance
   * @param title the desired WebView title to match
   * @param context the context map containing WebView information
   * @return the AppiumDriver if context switch successful, null otherwise
   */
  private static AppiumDriver checkIOSWebViewContext(
      AppiumDriver appiumDriver, String title, Map<String, Object> context) {
    String id = (String) context.get(WEBVIEW_CONTEXT_KEY_IOS);
    if (id.equals(NATIVE_CONTEXT_HANDLE)) {
      return null;
    }
    String newTitle = (String) context.get(WEBVIEW_TITLE_KEY_IOS);
    if (newTitle.equalsIgnoreCase(title)) {
      return switchToContext(appiumDriver, id, null);
    }
    return null;
  }

  private static AppiumDriver switchToContext(
      AppiumDriver appiumDriver, String context, String window) {
    SupportsContextSwitching contextSwitcher = (SupportsContextSwitching) appiumDriver;
    AppiumDriver newDriver = null;
    try {
      newDriver = (AppiumDriver) contextSwitcher.context(context);
    } catch (WebDriverException e) {
      error(e);
      error("Failed to switch to context: " + context);
    }
    // Window switching is on Android only.
    if (newDriver != null
        && window != null
        && appiumDriver.getCapabilities().getPlatformName().equals(Platform.ANDROID)) {
      // On emulators if it's the first window and Android < 13, it will fail if tried without
      // the
      // 'CDwindow-' prefix.
      try {
        newDriver.switchTo().window(window);
      } catch (NoSuchWindowException e1) {
        warning("Failed to switch to window: " + window);
        String cdWindow = "CDwindow-" + window;
        try {
          newDriver.switchTo().window(cdWindow);
          info("Successfully switched to window: " + cdWindow + ", retrying with prefix");
        } catch (NoSuchWindowException e2) {
          error("Failed to switch to window: " + cdWindow);
          newDriver = null;
        }
      }
    }
    return newDriver;
  }

  @Override
  public void setPageContextToNative() {
    MobileDriverUtils.setContextToNative(getAppiumDriver());
  }

  @Override
  public void setPageContextToWebView(String title) {
    if (title == null) {
      throw new UtamError(ERR_BRIDGE_TITLE_NULL);
    }
    waitFor(this::isWebViewAvailable, "wait for web view", null);
    AppiumDriver newDriver = waitFor(() -> switchToWebView(title), "switch to web view", null);
    resetDriver(newDriver);
  }

  @Override
  public boolean isNativeContext() {
    return MobileDriverUtils.isNative(getAppiumDriver());
  }

  final AppiumDriver getAppiumDriver() {
    return (AppiumDriver) getSeleniumDriver();
  }

  @Override
  public String getPageContext() {
    return ((SupportsContextSwitching) getAppiumDriver()).getContext();
  }

  final WebElement getWebViewElement() {
    if (isIOSPlatform()) {
      return getAppiumDriver().findElement(By.className("XCUIElementTypeWebView"));
    }
    return getAppiumDriver().findElement(By.className("android.webkit.WebView"));
  }

  // for tests
  final <T> T waitFor(Supplier<T> isTrue) {
    return waitFor(isTrue, null, null);
  }

  @Override
  public void setPageContext(PlatformType mobileContextType) {
    if (mobileContextType.equals(PlatformType.WEB)) {
      setPageContextToWebView(getDriverConfig().getBridgeAppTitle());
    } else {
      setPageContextToNative();
    }
  }

  @Override
  protected Element wrapElement(WebElement element) {
    return new MobileElementAdapter(element, this);
  }

  @Override
  public void back() {
    getAppiumDriver().navigate().back();
  }

  @Override
  public void forward() {
    getAppiumDriver().navigate().forward();
  }
}
