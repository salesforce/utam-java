/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.selenium.appium;

import static utam.core.selenium.appium.MobileDriverAdapter.NATIVE_CONTEXT_HANDLE;

import io.appium.java_client.AppiumDriver;
import io.appium.java_client.android.AndroidDriver;
import io.appium.java_client.ios.IOSDriver;
import java.util.Set;
import org.openqa.selenium.Dimension;
import org.openqa.selenium.Platform;
import org.openqa.selenium.Point;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import utam.core.framework.context.PlatformType;
import utam.core.framework.context.Profile;

/**
 * helper class for Appium implementation of mobile driver <br>, should be public because of public
 * static method
 *
 * @author r.rajasekaran
 * @since 232
 */
@SuppressWarnings("rawtypes")
public abstract class MobileDriverUtils {

  static Point[] getFlickCoordinates(MobileDriverAdapter driverAdapter, Point nativeStartPoint,
      Point nativeEndPoint) {
    AppiumDriver driver = driverAdapter.getAppiumDriver();
    if (!driverAdapter.isNative()) {
      WebElement webViewElement = driverAdapter.getWebViewElement();
      nativeStartPoint = convertWebViewLocationToNativeCoordinates(
          driver,
          webViewElement,
          nativeStartPoint);
      nativeEndPoint = convertWebViewLocationToNativeCoordinates(
          driver,
          webViewElement,
          nativeEndPoint);
    }
    Point start = boundCoordinates(driver, nativeStartPoint);
    Point end = boundCoordinates(driver, nativeEndPoint);
    return new Point[]{start, end};
  }

  private static int getBoundedCoordinate(int c, int dimension) {
    if (c <= 0) {
      return 5;
    }
    if (dimension <= c) {
      return dimension - 5;
    }
    return c;
  }

  /**
   * Bound the coordinates within the screen in NATIVE context and within the WebView in WEB
   * context.
   *
   * @return the original coordinate or the nearest coordinate within the bounds to the original
   */
  private static Point boundCoordinates(AppiumDriver driver, Point location) {
    int width, height;
    if (isNative(driver)) {
      width = driver.manage().window().getSize().getWidth();
      height = driver.manage().window().getSize().getHeight();
    } else {
      Dimension dimension = getWebViewDocumentSize(driver);
      width = dimension.getWidth();
      height = dimension.getHeight();
    }
    return new Point(
        getBoundedCoordinate(location.getX(), width),
        getBoundedCoordinate(location.getY(), height)
    );
  }

  /**
   * Gets the height and width of web view within the window
   *
   * @param driver AppiumDriver instance
   * @return Dimension of the web view
   */
  private static Dimension getWebViewDocumentSize(AppiumDriver driver) {
    Object res = driver
        .executeScript("return window.innerWidth || document.body.clientWidth");
    // null can be returned fro mocks in tests
    int webViewWidth = res != null ? ((Long) res).intValue() : -1;
    res = driver
        .executeScript("return window.innerHeight || document.body.clientHeight");
    int webViewHeight = res != null ? ((Long) res).intValue() : -1;
    return new Dimension(webViewWidth, webViewHeight);
  }

  private static Dimension getScrollOffset(AppiumDriver driver) {
    Object res = driver.executeScript("return window.pageXOffset");
    // null can be returned fro mocks in tests
    int scrollOffsetX = res != null ? ((Float) res).intValue() : -1;
    res = driver.executeScript("return window.pageYOffset");
    int scrollOffsetY = res != null ? ((Float) res).intValue() : -1;
    return new Dimension(scrollOffsetX, scrollOffsetY);
  }

  private static int webViewToNative(int coordinate, int scroll, double elementSize,
      int docDimension) {
    return (int)
        ((coordinate - scroll) * elementSize) / docDimension;
  }

  /**
   * Convert the location to native location
   *
   * @param driver          Appium Driver instance
   * @param webViewElement  WebView element
   * @param webViewLocation the coordinates within the WebView
   * @return the location of x,y target within the native context
   */
  private static Point convertWebViewLocationToNativeCoordinates(AppiumDriver driver,
      final WebElement webViewElement,
      Point webViewLocation) {
    int x, y;
    if (isIOSPlatform(driver)) {
      // for IOS scale for WebView to Native coordinates is 1:1 so just need to convert to absolute coordinates
      x = webViewLocation.getX();
      y = webViewLocation.getY();
    } else {
      Dimension docDimension = getWebViewDocumentSize(driver);
      Dimension scrollOffset = getScrollOffset(driver);
      Dimension webViewElementSize = webViewElement.getSize();
      x = webViewToNative(webViewLocation.getX(), scrollOffset.getWidth(),
          webViewElementSize.getWidth(), docDimension.getWidth());
      y = webViewToNative(webViewLocation.getY(), scrollOffset.getHeight(),
          webViewElementSize.getHeight(), docDimension.getHeight());
    }
    return getAbsoluteCoordinates(webViewElement, x, y);
  }

  /**
   * detect platform profile based on driver type
   *
   * @param driver driver instance
   * @return profile
   */
  public static Profile getActivePlatformProfile(WebDriver driver) {
    if (driver instanceof AndroidDriver) {
      return PlatformType.PLATFORM_ANDROID;
    }
    if (driver instanceof IOSDriver) {
      return PlatformType.PLATFORM_IOS;
    }
    if (driver instanceof AppiumDriver) {
      Platform platform = ((AppiumDriver) driver).getCapabilities().getPlatform();
      if (platform == Platform.LINUX) {
        return PlatformType.PLATFORM_ANDROID;
      }
      if (platform == Platform.MAC) {
        return PlatformType.PLATFORM_IOS;
      }
    }
    return PlatformType.PLATFORM_WEB;
  }

  private static boolean isAndroidPlatform(WebDriver driver) {
    return getActivePlatformProfile(driver).equals(PlatformType.PLATFORM_ANDROID);
  }

  static boolean isIOSPlatform(WebDriver driver) {
    return getActivePlatformProfile(driver).equals(PlatformType.PLATFORM_IOS);
  }

  static void setContextToNative(AppiumDriver driver) {
    if (!isNative(driver)) {
      driver.context(NATIVE_CONTEXT_HANDLE);
    }
  }

  static boolean isNative(AppiumDriver driver) {
    return NATIVE_CONTEXT_HANDLE.equals(driver.getContext());
  }

  /**
   * Calculate the absolute position of the given coordinates.
   *
   * @param webView  The WebView that contains the coordinates
   * @param xWebView The x coordinate relative to the WebView
   * @param yWebView The y coordinate relative to the WebView
   * @return The absolute position of the target
   */
  private static Point getAbsoluteCoordinates(WebElement webView, int xWebView,
      int yWebView) {
    // The dimensions are all relative to the WebView, so calculate absolute coordinates
    int webViewX = webView.getLocation().getX();
    int webViewY = webView.getLocation().getY();
    int absoluteX = xWebView + webViewX;
    int absoluteY = yWebView + webViewY;
    return new Point(absoluteX, absoluteY);
  }

  static AppiumDriver switchToWebView(AppiumDriver appiumDriver, String title) {
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
    if (isAndroidPlatform(appiumDriver)) {
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
}
