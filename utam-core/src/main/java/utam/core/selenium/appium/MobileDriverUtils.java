/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.selenium.appium;

import static utam.core.framework.context.MobilePlatformType.PLATFORM_PROFILE_NAME;
import static utam.core.framework.context.MobilePlatformType.fromDriver;
import static utam.core.selenium.appium.MobileDriverAdapter.NATIVE_CONTEXT_HANDLE;

import io.appium.java_client.AppiumDriver;
import org.openqa.selenium.Dimension;
import org.openqa.selenium.Point;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import utam.core.framework.context.Profile;
import utam.core.framework.context.StringValueProfile;

/**
 * helper class for Appium implementation of mobile driver <br>
 * , should be public because of public static method
 *
 * @author r.rajasekaran
 * @since 232
 */
@SuppressWarnings("rawtypes")
public abstract class MobileDriverUtils {

  /**
   * this method is used in downstream projects to set proper profile
   *
   * @param driver selenium driver instance
   * @return profile for current platform
   */
  public static Profile getActivePlatformProfile(WebDriver driver) {
    return new StringValueProfile(PLATFORM_PROFILE_NAME, fromDriver(driver).name().toLowerCase());
  }

  static Point[] getFlickCoordinates(
      MobileDriverAdapter driverAdapter, Point nativeStartPoint, Point nativeEndPoint) {
    AppiumDriver driver = driverAdapter.getAppiumDriver();
    if (!driverAdapter.isNativeContext()) {
      WebElement webViewElement = driverAdapter.getWebViewElement();
      nativeStartPoint =
          convertWebViewLocationToNativeCoordinates(
              driverAdapter, webViewElement, nativeStartPoint);
      nativeEndPoint =
          convertWebViewLocationToNativeCoordinates(driverAdapter, webViewElement, nativeEndPoint);
    }
    Point start = boundCoordinates(driver, nativeStartPoint);
    Point end = boundCoordinates(driver, nativeEndPoint);
    return new Point[] {start, end};
  }

  private static int getBoundedCoordinate(int c, int dimension) {
    if (c <= 0) {
      // beyond the lower bounds, adjust to 10% of dimension to avoid over scroll
      return (int) (dimension * .1);
    }
    if (dimension <= c) {
      // beyond the upper bounds, adjust to 90% of dimension to avoid over scroll
      return (int) (dimension * .9);
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
    Dimension dimension;
    if (isNative(driver)) {
      dimension = driver.manage().window().getSize();
      width = dimension.getWidth();
      height = dimension.getHeight();
    } else {
      dimension = getWebViewDocumentSize(driver);
      width = dimension.getWidth();
      height = dimension.getHeight();
    }
    return new Point(
        getBoundedCoordinate(location.getX(), width),
        getBoundedCoordinate(location.getY(), height));
  }

  /**
   * Gets the height and width of web view within the window
   *
   * @param driver AppiumDriver instance
   * @return Dimension of the web view
   */
  private static Dimension getWebViewDocumentSize(AppiumDriver driver) {
    Object res = driver.executeScript("return window.innerWidth || document.body.clientWidth");
    // null can be returned fro mocks in tests
    int webViewWidth = res != null ? ((Long) res).intValue() : -1;
    res = driver.executeScript("return window.innerHeight || document.body.clientHeight");
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

  private static int webViewToNative(
      int coordinate, int scroll, double elementSize, int docDimension) {
    return (int) ((coordinate - scroll) * elementSize) / docDimension;
  }

  /**
   * Convert the location to native location
   *
   * @param driverAdapter driver wrapper instance
   * @param webViewElement WebView element
   * @param webViewLocation the coordinates within the WebView
   * @return the location of x,y target within the native context
   */
  private static Point convertWebViewLocationToNativeCoordinates(
      MobileDriverAdapter driverAdapter, final WebElement webViewElement, Point webViewLocation) {
    AppiumDriver driver = driverAdapter.getAppiumDriver();
    int x, y;
    if (driverAdapter.isIOSPlatform()) {
      // for IOS scale for WebView to Native coordinates is 1:1 so just need to convert to absolute
      // coordinates
      x = webViewLocation.getX();
      y = webViewLocation.getY();
    } else {
      Dimension docDimension = getWebViewDocumentSize(driver);
      Dimension scrollOffset = getScrollOffset(driver);
      Dimension webViewElementSize = webViewElement.getSize();
      x =
          webViewToNative(
              webViewLocation.getX(),
              scrollOffset.getWidth(),
              webViewElementSize.getWidth(),
              docDimension.getWidth());
      y =
          webViewToNative(
              webViewLocation.getY(),
              scrollOffset.getHeight(),
              webViewElementSize.getHeight(),
              docDimension.getHeight());
    }
    return getAbsoluteCoordinates(webViewElement, x, y);
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
   * @param webView The WebView that contains the coordinates
   * @param xWebView The x coordinate relative to the WebView
   * @param yWebView The y coordinate relative to the WebView
   * @return The absolute position of the target
   */
  private static Point getAbsoluteCoordinates(WebElement webView, int xWebView, int yWebView) {
    // The dimensions are all relative to the WebView, so calculate absolute coordinates
    Point location = webView.getLocation();
    int webViewX = location.getX();
    int webViewY = location.getY();
    int absoluteX = xWebView + webViewX;
    int absoluteY = yWebView + webViewY;
    return new Point(absoluteX, absoluteY);
  }
}
