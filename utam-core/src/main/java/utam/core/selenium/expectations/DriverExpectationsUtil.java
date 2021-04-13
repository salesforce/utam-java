/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.selenium.expectations;

import org.openqa.selenium.Alert;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebDriverException;
import org.openqa.selenium.support.ui.ExpectedConditions;

import utam.core.appium.context.AppiumContextProvider;
import utam.core.framework.context.PlatformType;
import io.appium.java_client.AppiumDriver;

import java.util.Set;

/**
 * static methods that can be applied for page object <br>
 * since those are used by generator, it's marked by translations binding it with enum of supported
 * values
 *
 * @author elizaveta.ivanova
 * @since 226
 */
@SuppressWarnings({"rawtypes", "unused"})
public class DriverExpectationsUtil {

  public static DriverExpectations<Alert> alert() {
    return () -> driver -> ExpectedConditions.alertIsPresent().apply(driver);
  }

  public static DriverExpectations<String> getUrl() {
    return () -> WebDriver::getCurrentUrl;
  }

  public static DriverExpectations<WebDriver> switchToTab() {
    return () ->
        driver -> {
          Set<String> handles = driver.getWindowHandles();
          // wait till additional tab is opened
          Set<String> newHandles = driver.getWindowHandles();
          newHandles.removeAll(handles);
          if (newHandles.isEmpty()) {
            throw new WebDriverException("Could not open new browser tab");
          }
          driver.switchTo().window(newHandles.iterator().next());
          return driver;
        };
  }

  public static DriverExpectations<WebDriver> setUrl(String fullUrl) {
    return () ->
        driver -> {
          driver.get(fullUrl);
          return driver;
        };
  }

  /**
   * via driver to set a context that is for the WebView page with the 
   * given title as the current context.
   * @param title the title of the target WebView page
   * @return the new instance of driver
 */
  @SuppressWarnings("unchecked")
  public static DriverExpectations<AppiumDriver> switchToWebView(
          final String title) {
    return () ->
        driver -> {
          AppiumDriver appiumDriver = (AppiumDriver)driver;
          Set<String> contextHandles = appiumDriver.getContextHandles();
          for (String contextHandle : contextHandles) {
            if (!contextHandle.equals(AppiumContextProvider.NATIVE_CONTEXT_HANDLE)) {
              AppiumDriver newDriver = (AppiumDriver)appiumDriver.context(contextHandle);
              String newTitle = newDriver.getTitle();
              if (!newTitle.isEmpty() && newTitle.equalsIgnoreCase(title)) {
                return newDriver;
              }
            }
          }
          // For the Appium chromedriver limitation to handle multiple WebViews,
          // If switch to context fail to find the target WebView, then switch to 
          // use window
          String platformName = ((AppiumDriver)driver).getPlatformName();
          if (platformName.equalsIgnoreCase(PlatformType.PLATFORM_ANDROID.getValue())) {
              Set<String> windowHandles = driver.getWindowHandles();
              if (windowHandles.size() > 1) {
                  for (String windowHandle : windowHandles) {
                      AppiumDriver newDriver = (AppiumDriver)appiumDriver.switchTo().window(windowHandle);
                      String currentTitle = newDriver.getTitle();
                      if (!currentTitle.isEmpty() && currentTitle.equalsIgnoreCase(title)) {
                          return newDriver;
                      }
                  }
              }
          }
          return null;
        };
  }

  /**
   * wait any WebView context is available.
   * @return true if there is any WebView context available, otherwise return false
  */
  @SuppressWarnings("unchecked")
  public static DriverExpectations<Boolean> isAnyWebViewContextAvailable() {
    return () -> 
        driver -> {
          Set<String> contextHandles = ((AppiumDriver)driver).getContextHandles();
          return  contextHandles.stream().
              anyMatch(x -> x.contains(AppiumContextProvider.WEBVIEW_CONTEXT_HANDLE_PREFIX));
        };
  }

  public enum Type {
    getUrl(new Class[0], String.class),
    setUrl(new Class[] {String.class}, WebDriver.class),
    switchToTab(new Class[0], WebDriver.class),
    switchToWebView(new Class[] {String.class}, AppiumDriver.class),
    isAnyWebViewContextAvailable(new Class[0], Boolean.class);

    /** return type by the action */
    private final Class returns;
    /** parameter accepted by the action */
    private final Class[] actionParameter;

    Type(Class[] parameter, Class returns) {
      this.actionParameter = parameter;
      this.returns = returns;
    }

    public final Class[] getParameterTypes() {
      return actionParameter;
    }

    public final Class getReturnType() {
      return returns;
    }
  }
}
