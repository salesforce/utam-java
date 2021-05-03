/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.selenium.appium;

import static utam.core.selenium.appium.MobileDriverUtils.isIOSPlatform;

import io.appium.java_client.AppiumDriver;
import java.time.Duration;
import java.util.Set;
import org.openqa.selenium.By;
import org.openqa.selenium.WebElement;
import utam.core.driver.Driver;
import utam.core.driver.Expectations;
import utam.core.framework.consumer.UtamError;
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

  public MobileDriverAdapter(AppiumDriver driver) {
    super(driver);
  }

  static Expectations<AppiumDriver> switchToWebView(AppiumDriver driver, String title) {
    return new ExpectationsImpl<>(
        "switch to web view",
        element -> MobileDriverUtils.switchToWebView(driver, title));
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
    AppiumDriver newDriver = waitFor(timeout, pollingInterval, switchToWebView(getAppiumDriver(), title));
    resetDriver(newDriver);
  }

  @Override
  public boolean isNative() {
    return MobileDriverUtils.isNative(getAppiumDriver());
  }

  AppiumDriver getAppiumDriver() {
    return (AppiumDriver) getSeleniumDriver();
  }

  static AppiumDriver getAppiumDriver(Driver driver) {
    return ((MobileDriverAdapter) driver).getAppiumDriver();
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
    if (isIOSPlatform(getAppiumDriver())) {
      return getAppiumDriver().findElement(By.className("XCUIElementTypeWebView"));
    }
    return getAppiumDriver().findElement(By.className("android.webkit.WebView"));
  }
}
