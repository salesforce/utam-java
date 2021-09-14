/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.selenium.appium;

import io.appium.java_client.AppiumDriver;
import java.time.Duration;
import java.util.Set;
import org.openqa.selenium.By;
import org.openqa.selenium.WebElement;
import utam.core.driver.Driver;
import utam.core.driver.Expectations;
import utam.core.framework.consumer.UtamError;
import utam.core.framework.context.MobilePlatformType;
import utam.core.framework.element.ExpectationsImpl;
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
  static final String ERR_BRIDGE_TITLE_NULL =
      "Bridge application title is null, please configure";

  static final Expectations<Boolean> WEBVIEW_AVAILABILITY = new ExpectationsImpl<>(
      "get available web view", (driver, element) -> {
    Set<String> contextHandles = getAppiumDriver(driver).getContextHandles();
    return contextHandles.stream().
        anyMatch(handle -> handle.contains(WEBVIEW_CONTEXT_HANDLE_PREFIX));
  });

  private final MobilePlatformType mobilePlatform;

  public MobileDriverAdapter(AppiumDriver driver) {
    super(driver);
    this.mobilePlatform = MobilePlatformType.fromDriver(driver);
  }

  static AppiumDriver getAppiumDriver(Driver driver) {
    return ((MobileDriverAdapter) driver).getAppiumDriver();
  }

  final boolean isIOSPlatform() {
    return mobilePlatform == MobilePlatformType.IOS ||
            mobilePlatform == MobilePlatformType.IOS_PHONE ||
            mobilePlatform == MobilePlatformType.IOS_TABLET;
  }

  Expectations<AppiumDriver> getSwitchToWebViewExpectations(String title) {
    return new ExpectationsImpl<>(
        "switch to web view",
        element -> switchToWebView(title));
  }

  private AppiumDriver switchToWebView(String title) {
    AppiumDriver appiumDriver = getAppiumDriver();
    Set<String> contextHandles = appiumDriver.getContextHandles();
    for (String contextHandle : contextHandles) {
      if (!contextHandle.equals(NATIVE_CONTEXT_HANDLE)) {
        AppiumDriver newDriver = (AppiumDriver) appiumDriver.context(contextHandle);
        String newTitle = newDriver.getTitle();
        if (!newTitle.isEmpty() && newTitle.equalsIgnoreCase(title)) {
          return newDriver;
        }
      }
    }
    // For the Appium chromedriver limitation to handle multiple WebViews,
    // If switch to context fail to find the target WebView, then switch to
    // use window
    if (mobilePlatform == MobilePlatformType.ANDROID ||
        mobilePlatform == MobilePlatformType.ANDROID_PHONE ||
        mobilePlatform == MobilePlatformType.ANDROID_TABLET) {
      Set<String> windowHandles = appiumDriver.getWindowHandles();
      for (String windowHandle : windowHandles) {
        AppiumDriver newDriver = (AppiumDriver) appiumDriver.switchTo().window(windowHandle);
        String currentTitle = newDriver.getTitle();
        if (!currentTitle.isEmpty() && currentTitle.equalsIgnoreCase(title)) {
          return newDriver;
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
  public void setPageContextToWebView(String title, Duration timeout, Duration pollingInterval) {
    if (title == null) {
      throw new UtamError(ERR_BRIDGE_TITLE_NULL);
    }
    if (!isNative() && title.equalsIgnoreCase(getAppiumDriver().getTitle())) {
      return;
    }
    waitFor(timeout, pollingInterval, WEBVIEW_AVAILABILITY);
    AppiumDriver newDriver = waitFor(timeout, pollingInterval, getSwitchToWebViewExpectations(title));
    resetDriver(newDriver);
  }

  @Override
  public boolean isNative() {
    return MobileDriverUtils.isNative(getAppiumDriver());
  }

  AppiumDriver getAppiumDriver() {
    return (AppiumDriver) getSeleniumDriver();
  }

  @Override
  public boolean isMobile() {
    return true;
  }

  @Override
  public String getContext() {
    return getAppiumDriver().getContext();
  }

  // to mock from tests
  public final WebElement getWebViewElement() {
    if (isIOSPlatform()) {
      return getAppiumDriver().findElement(By.className("XCUIElementTypeWebView"));
    }
    return getAppiumDriver().findElement(By.className("android.webkit.WebView"));
  }
}
