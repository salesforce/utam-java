/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.selenium.appium;

import static utam.core.framework.UtamLogger.warning;

import io.appium.java_client.AppiumDriver;
import java.util.Set;
import java.util.function.Supplier;
import org.openqa.selenium.By;
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
    Set<String> contextHandles = appiumDriver.getContextHandles();
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
    Set<String> contextHandles = appiumDriver.getContextHandles();
    for (String contextHandle : contextHandles) {
      if (!contextHandle.equals(NATIVE_CONTEXT_HANDLE)) {
        AppiumDriver newDriver;
        try {
          newDriver = (AppiumDriver) appiumDriver.context(contextHandle);
        } catch (WebDriverException e) {
          warning(
              String.format(
                  "Context switch to webview '%s' failed. Error: %s",
                  contextHandle, e.getMessage()));
          continue;
        }
        if (newDriver != null) {
          String newTitle = newDriver.getTitle();
          if (!newTitle.isEmpty() && newTitle.equalsIgnoreCase(title)) {
            return newDriver;
          }
        }
      }
    }
    // For the Appium chromedriver limitation to handle multiple WebViews,
    // If switch to context fail to find the target WebView, then switch to
    // use window
    if (mobilePlatform == MobilePlatformType.ANDROID
        || mobilePlatform == MobilePlatformType.ANDROID_PHONE
        || mobilePlatform == MobilePlatformType.ANDROID_TABLET) {
      Set<String> windowHandles = appiumDriver.getWindowHandles();
      for (String windowHandle : windowHandles) {
        if (!windowHandle.equals(NATIVE_CONTEXT_HANDLE)) {
          AppiumDriver newDriver = (AppiumDriver) appiumDriver.switchTo().window(windowHandle);
          String currentTitle = newDriver.getTitle();
          if (!currentTitle.isEmpty() && currentTitle.equalsIgnoreCase(title)) {
            return newDriver;
          }
        }
      }
    }
    return null;
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
    return getAppiumDriver().getContext();
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
