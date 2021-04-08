/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.appium.expectations;

import java.time.Duration;

import org.openqa.selenium.By;
import org.openqa.selenium.Dimension;
import org.openqa.selenium.Point;
import org.openqa.selenium.SearchContext;
import org.openqa.selenium.WebElement;

import io.appium.java_client.AppiumDriver;
import io.appium.java_client.TouchAction;
import io.appium.java_client.touch.WaitOptions;
import io.appium.java_client.touch.offset.PointOption;
import utam.core.appium.context.AppiumContextProvider;
import utam.core.appium.context.AppiumDriverUtilities;
import utam.core.framework.consumer.UtamError;
import utam.core.framework.context.PlatformType;
import utam.core.selenium.expectations.AbstractElementExpectation;
import utam.core.selenium.expectations.ElementExpectations;

/**
 * helper class for appium implementation of mobile specific expectations <br>
 *
 * @author r.rajasekaran
 * @since 232
*/
@SuppressWarnings("rawtypes")
public class MobileExpectationsUtil {

  private static final String FLICK_MSG = "flick element at X '%d' Y '%d'";
  private static final String ERR_INVALID_METHOD_CALL = "Method is applicable only for iOS/Android";
  private static final int DEFAULT_FLICK_ACTION_WAIT_MILLISECONDS = 500;

  public static ElementExpectations<SearchContext> flick(int xOffset, int yOffset) {
    return new AbstractElementExpectation.ConsumesUtilsElement(
        String.format(FLICK_MSG, xOffset, yOffset),
        (utilities, element) ->  flickElement((AppiumDriverUtilities) utilities, element, xOffset, yOffset));
  }

  private static void flickElement(AppiumDriverUtilities driverUtils, WebElement element, int xOffset, int yOffset) {
    AppiumDriver driver = (AppiumDriver) driverUtils.getWebDriver();
    Point nativeStartPoint = element.getLocation();
    nativeStartPoint = nativeStartPoint.moveBy(
        element.getSize().getWidth() / 2,
        element.getSize().getHeight() / 2
    );
    Point nativeEndPoint = nativeStartPoint.moveBy(xOffset, yOffset);
    String originalContext = driver.getContext();
    try {
      if (!driverUtils.isNative()) {
        nativeStartPoint = convertWebViewLocationToNativeCoordinates(
            driverUtils,
            nativeStartPoint.getX(),
            nativeStartPoint.getY()
            );
        nativeEndPoint = convertWebViewLocationToNativeCoordinates(
            driverUtils,
            nativeEndPoint.getX(),
            nativeEndPoint.getY()
            );
        driverUtils.setPageContextToNative();
      }
      int startX = wrapXCoordinate(driverUtils, nativeStartPoint.getX());
      int startY = wrapYCoordinate(driverUtils, nativeStartPoint.getY());
      int endX = wrapXCoordinate(driverUtils, nativeEndPoint.getX());
      int endY = wrapYCoordinate(driverUtils, nativeEndPoint.getY());
      flickAtLocation(driver, startX, startY, endX, endY, DEFAULT_FLICK_ACTION_WAIT_MILLISECONDS);
    } finally {
      if (!originalContext.equals(AppiumContextProvider.NATIVE_CONTEXT_HANDLE)) {
        driverUtils.setPageContextToWebView(originalContext);
      }
    }
  }

  /**
   * Bound the x coordinate within the screen in NATIVE context and within the WebView in WEB context.
   * @param driverUtils AppiumDriverUtilities instance
   * @param x the x coordinate to check
   * @return the original x coordinate or the nearest x coordinate within the bounds to the original
   */
  private static int wrapXCoordinate(AppiumDriverUtilities driverUtils, int x) {
    AppiumDriver driver = (AppiumDriver) driverUtils.getWebDriver();
    int windowWidth;
    if (driverUtils.isNative()) {
      windowWidth = driver.manage().window().getSize().getWidth();
    } else {
      windowWidth = getWebViewDocumentSize(driver).getWidth();
    }
    
    if (x <= 0) {
      return 5;
    } else if (windowWidth <= x) {
      return windowWidth - 5;
    }
    return x;
  }

  /**
   * Bound the y coordinate within the screen in NATIVE context and within the WebView in WEB context.
   * @param driverUtils AppiumDriverUtilities instance
   * @param y the y coordinate to check
   * @return the original y coordinate or the nearest y coordinate within the bounds to the original
   */
  private static int wrapYCoordinate(AppiumDriverUtilities driverUtils, int y) {
    AppiumDriver driver = (AppiumDriver) driverUtils.getWebDriver();
    int windowHeight;
    if (driverUtils.isNative()) {
      windowHeight = driver.manage().window().getSize().getHeight();
    } else {
      windowHeight = getWebViewDocumentSize(driver).getHeight();
    }

    if (y <= 0) {
      return 5;
    } else if (windowHeight <= y) {
      return windowHeight - 5;
    }
    return y;
  }

  /**
   * Gets the height and width of web view within the window
   * @param driver AppiumDriver instance
   * @return Dimension of the web view
   */
  private static Dimension getWebViewDocumentSize(AppiumDriver driver) {
    int webViewWidth = ((Long) driver.executeScript("return window.innerWidth || document.body.clientWidth")).intValue();
    int webViewHeight = ((Long) driver.executeScript("return window.innerHeight || document.body.clientHeight")).intValue();
    return new Dimension(webViewWidth, webViewHeight);
  }

  /**
   * Simulates a flick using touch control
   * @param driver AppiumDriver instance
   * @param startX x-coordinate of starting point
   * @param startY y-coordinate of starting point
   * @param endX x-coordinate of end point
   * @param endY y-coordinate of end point
   * @param speedInMillis wait time in milliseconds before moving touch control from starting to end point
   */
  private static void flickAtLocation(AppiumDriver driver, int startX, int startY, int endX, int endY, int speedInMillis) {
    new TouchAction(driver)
    .press(PointOption.point(startX, startY))
    .waitAction(WaitOptions.waitOptions(Duration.ofMillis(speedInMillis)))
    .moveTo(PointOption.point(endX, endY))
    .release()
    .perform();
  }

  /**
   * Convert the location to native location
   * @param driverUtils AppiumDriverUtilities instance
   * @param coordinateXWebView the x-coordinate within the WebView
   * @param coordinateYWebView the y-coordinate within the WebView
   * @return the location of x,y target within the native context
   */
  private static Point convertWebViewLocationToNativeCoordinates(AppiumDriverUtilities driverUtils, int coordinateXWebView, int coordinateYWebView) {
    AppiumDriver driver = (AppiumDriver) driverUtils.getWebDriver();
    if (PlatformType.getActivePlatformProfile(driver).equals(PlatformType.PLATFORM_IOS)) {
      return convertWebViewLocationToNativeCoordinatesIOS(driverUtils, coordinateXWebView, coordinateYWebView);
    }
    else if (PlatformType.getActivePlatformProfile(driver).equals(PlatformType.PLATFORM_ANDROID)) {
      return convertWebViewLocationToNativeCoordinatesAndroid(driverUtils, coordinateXWebView, coordinateYWebView);
    }
    else {
      throw new UtamError(ERR_INVALID_METHOD_CALL);
    }
  }

  private static Point convertWebViewLocationToNativeCoordinatesAndroid(AppiumDriverUtilities driverUtils, int coordinateXWebView, int coordinateYWebView) {
    AppiumDriver driver = (AppiumDriver) driverUtils.getWebDriver();
    String currentContext = driver.getContext();
    try {
      // WebView Dimensions
      Dimension webViewDocDimension = getWebViewDocumentSize(driver);
      int webViewDocWidth = webViewDocDimension.getWidth();
      int webViewDocHeight = webViewDocDimension.getHeight();
      int scrollOffsetX = ((Float) driver.executeScript("return window.pageXOffset")).intValue();
      int scrollOffsetY = ((Float) driver.executeScript("return window.pageYOffset")).intValue();

      driverUtils.setPageContextToNative();

      WebElement webView = driver.findElement(By.className("android.webkit.WebView"));
      double webViewWidth = webView.getSize().getWidth();
      double webViewHeight = webView.getSize().getHeight();

      // From the WebView coordinates we will calculate the Native view coordinates using the width and height
      double elementXScaled = ((coordinateXWebView - scrollOffsetX) * webViewWidth) / webViewDocWidth;
      double elementYScaled = ((coordinateYWebView - scrollOffsetY) * webViewHeight) / webViewDocHeight;

      return absoluteCoordinatesForWebViewCoordinates(webView, (int) elementXScaled, (int) elementYScaled);
    } finally {
      driver.context(currentContext);
    }
  }

  private static Point convertWebViewLocationToNativeCoordinatesIOS(AppiumDriverUtilities driverUtils, int coordinateXWebView, int coordinateYWebView) {
    AppiumDriver driver = (AppiumDriver) driverUtils.getWebDriver();
    String currentContext = driver.getContext();
    try {
      driverUtils.setPageContextToNative();

      // The scale for WebView to Native coordinates is 1:1 so just need to convert to absolute coordinates
      return absoluteCoordinatesForWebViewCoordinates(driver.findElement(By.className("XCUIElementTypeWebView")), coordinateXWebView, coordinateYWebView);
    } finally {
      driver.context(currentContext);
    }
  }

  /**
   * Calculate the absolute position of the given coordinates.
   * @param webView The WebView that contains the coordinates
   * @param xWebView The x coordinate relative to the WebView
   * @param yWebView The y coordinate relative to the WebView
   * @return The absolute position of the target
   */
  private static Point absoluteCoordinatesForWebViewCoordinates(WebElement webView, int xWebView, int yWebView) {
    // The dimensions are all relative to the WebView, so calculate absolute coordinates
    int webViewX = webView.getLocation().getX();
    int webViewY = webView.getLocation().getY();
    int absoluteX = xWebView + webViewX;
    int absoluteY = yWebView + webViewY;
    return new Point(absoluteX, absoluteY);
  }

}
