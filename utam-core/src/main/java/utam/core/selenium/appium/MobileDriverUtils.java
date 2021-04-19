package utam.core.selenium.appium;

import io.appium.java_client.AppiumDriver;
import io.appium.java_client.TouchAction;
import io.appium.java_client.android.AndroidDriver;
import io.appium.java_client.ios.IOSDriver;
import io.appium.java_client.touch.WaitOptions;
import io.appium.java_client.touch.offset.PointOption;
import java.time.Duration;
import java.util.Set;
import org.openqa.selenium.By;
import org.openqa.selenium.Dimension;
import org.openqa.selenium.Platform;
import org.openqa.selenium.Point;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import utam.core.driver.Driver;
import utam.core.driver.DriverContext;
import utam.core.framework.consumer.UtamError;
import utam.core.framework.context.PlatformType;
import utam.core.framework.context.Profile;

/**
 * helper class for Appium implementation of mobile driver <br>
 *
 * @author r.rajasekaran
 * @since 232
 */
@SuppressWarnings("rawtypes")
abstract class MobileDriverUtils {

  private static final String ERR_SUPPORTED_FOR_MOBILE = "Method is applicable only for iOS/Android";
  private static final Duration DEFAULT_FLICK_ACTION_WAIT_MILLISECONDS = Duration.ofMillis(500);

  static void flickElement(Driver driverWrapper,
      Duration timeout,
      Duration pollingInterval,
      WebElement element,
      int xOffset, int yOffset) {
    AppiumDriver driver = ((MobileDriverAdapter) driverWrapper).getAppiumDriver();
    Point nativeStartPoint = element.getLocation();
    nativeStartPoint = nativeStartPoint.moveBy(
        element.getSize().getWidth() / 2,
        element.getSize().getHeight() / 2
    );
    Point nativeEndPoint = nativeStartPoint.moveBy(xOffset, yOffset);
    String originalContext = driver.getContext();
    try {
      if (!driverWrapper.isNative()) {
        nativeStartPoint = convertWebViewLocationToNativeCoordinates(
            driverWrapper,
            nativeStartPoint.getX(),
            nativeStartPoint.getY()
        );
        nativeEndPoint = convertWebViewLocationToNativeCoordinates(
            driverWrapper,
            nativeEndPoint.getX(),
            nativeEndPoint.getY()
        );
        driverWrapper.setPageContextToNative();
      }
      int startX = wrapXCoordinate(driverWrapper, nativeStartPoint.getX());
      int startY = wrapYCoordinate(driverWrapper, nativeStartPoint.getY());
      int endX = wrapXCoordinate(driverWrapper, nativeEndPoint.getX());
      int endY = wrapYCoordinate(driverWrapper, nativeEndPoint.getY());
      flickAtLocation(driver, startX, startY, endX, endY);
    } finally {
      if (!originalContext.equals(MobileDriverAdapter.NATIVE_CONTEXT_HANDLE)) {
        driverWrapper.setPageContextToWebView(originalContext, timeout, pollingInterval);
      }
    }
  }

  /**
   * Bound the x coordinate within the screen in NATIVE context and within the WebView in WEB
   * context.
   *
   * @param driverWrapper AppiumDriverUtilities instance
   * @param x             the x coordinate to check
   * @return the original x coordinate or the nearest x coordinate within the bounds to the original
   */
  private static int wrapXCoordinate(Driver driverWrapper, int x) {
    AppiumDriver driver = ((MobileDriverAdapter) driverWrapper).getAppiumDriver();
    int windowWidth;
    if (driverWrapper.isNative()) {
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
   * Bound the y coordinate within the screen in NATIVE context and within the WebView in WEB
   * context.
   *
   * @param driverWrapper AppiumDriverUtilities instance
   * @param y             the y coordinate to check
   * @return the original y coordinate or the nearest y coordinate within the bounds to the original
   */
  private static int wrapYCoordinate(Driver driverWrapper, int y) {
    AppiumDriver driver = ((MobileDriverAdapter) driverWrapper).getAppiumDriver();
    int windowHeight;
    if (driverWrapper.isNative()) {
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
   *
   * @param driver AppiumDriver instance
   * @return Dimension of the web view
   */
  private static Dimension getWebViewDocumentSize(AppiumDriver driver) {
    int webViewWidth = ((Long) driver
        .executeScript("return window.innerWidth || document.body.clientWidth")).intValue();
    int webViewHeight = ((Long) driver
        .executeScript("return window.innerHeight || document.body.clientHeight")).intValue();
    return new Dimension(webViewWidth, webViewHeight);
  }

  /**
   * Simulates a flick using touch control
   *
   * @param driver AppiumDriver instance
   * @param startX x-coordinate of starting point
   * @param startY y-coordinate of starting point
   * @param endX   x-coordinate of end point
   * @param endY   y-coordinate of end point
   */
  private static void flickAtLocation(io.appium.java_client.AppiumDriver driver, int startX,
      int startY, int endX,
      int endY) {
    new TouchAction(driver)
        .press(PointOption.point(startX, startY))
        .waitAction(WaitOptions.waitOptions(DEFAULT_FLICK_ACTION_WAIT_MILLISECONDS))
        .moveTo(PointOption.point(endX, endY))
        .release()
        .perform();
  }

  /**
   * Convert the location to native location
   *
   * @param driverWrapper      AppiumDriverUtilities instance
   * @param coordinateXWebView the x-coordinate within the WebView
   * @param coordinateYWebView the y-coordinate within the WebView
   * @return the location of x,y target within the native context
   */
  private static Point convertWebViewLocationToNativeCoordinates(Driver driverWrapper,
      int coordinateXWebView, int coordinateYWebView) {
    AppiumDriver driver = ((MobileDriverAdapter) driverWrapper).getAppiumDriver();
    if (isIOSPlatform(driver)) {
      return convertWebViewLocationToNativeCoordinatesIOS(driverWrapper, coordinateXWebView,
          coordinateYWebView);
    } else if (isAndroidPlatform(driver)) {
      return convertWebViewLocationToNativeCoordinatesAndroid(driverWrapper, coordinateXWebView,
          coordinateYWebView);
    } else {
      throw new UtamError(ERR_SUPPORTED_FOR_MOBILE);
    }
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

  private static boolean isIOSPlatform(WebDriver driver) {
    return getActivePlatformProfile(driver).equals(PlatformType.PLATFORM_IOS);
  }

  private static Point convertWebViewLocationToNativeCoordinatesAndroid(
      Driver driverWrapper, int coordinateXWebView, int coordinateYWebView) {
    AppiumDriver driver = ((MobileDriverAdapter) driverWrapper).getAppiumDriver();
    String currentContext = driver.getContext();
    try {
      // WebView Dimensions
      Dimension webViewDocDimension = getWebViewDocumentSize(driver);
      int webViewDocWidth = webViewDocDimension.getWidth();
      int webViewDocHeight = webViewDocDimension.getHeight();
      int scrollOffsetX = ((Float) driver.executeScript("return window.pageXOffset")).intValue();
      int scrollOffsetY = ((Float) driver.executeScript("return window.pageYOffset")).intValue();

      driverWrapper.setPageContextToNative();

      WebElement webView = driver.findElement(By.className("android.webkit.WebView"));
      double webViewWidth = webView.getSize().getWidth();
      double webViewHeight = webView.getSize().getHeight();

      // From the WebView coordinates we will calculate the Native view coordinates using the width and height
      double elementXScaled =
          ((coordinateXWebView - scrollOffsetX) * webViewWidth) / webViewDocWidth;
      double elementYScaled =
          ((coordinateYWebView - scrollOffsetY) * webViewHeight) / webViewDocHeight;

      return absoluteCoordinatesForWebViewCoordinates(webView, (int) elementXScaled,
          (int) elementYScaled);
    } finally {
      driver.context(currentContext);
    }
  }

  private static Point convertWebViewLocationToNativeCoordinatesIOS(
      Driver driverWrapper, int coordinateXWebView, int coordinateYWebView) {
    AppiumDriver driver = ((MobileDriverAdapter) driverWrapper).getAppiumDriver();
    String currentContext = driver.getContext();
    try {
      driverWrapper.setPageContextToNative();

      // The scale for WebView to Native coordinates is 1:1 so just need to convert to absolute coordinates
      return absoluteCoordinatesForWebViewCoordinates(
          driver.findElement(By.className("XCUIElementTypeWebView")), coordinateXWebView,
          coordinateYWebView);
    } finally {
      driver.context(currentContext);
    }
  }

  /**
   * Calculate the absolute position of the given coordinates.
   *
   * @param webView  The WebView that contains the coordinates
   * @param xWebView The x coordinate relative to the WebView
   * @param yWebView The y coordinate relative to the WebView
   * @return The absolute position of the target
   */
  private static Point absoluteCoordinatesForWebViewCoordinates(WebElement webView, int xWebView,
      int yWebView) {
    // The dimensions are all relative to the WebView, so calculate absolute coordinates
    int webViewX = webView.getLocation().getX();
    int webViewY = webView.getLocation().getY();
    int absoluteX = xWebView + webViewX;
    int absoluteY = yWebView + webViewY;
    return new Point(absoluteX, absoluteY);
  }

  static AppiumDriver switchToWebView(Driver driver, String title) {
    AppiumDriver appiumDriver = ((MobileDriverAdapter) driver).getAppiumDriver();
    Set<String> contextHandles = appiumDriver.getContextHandles();
    for (String contextHandle : contextHandles) {
      if (!contextHandle.equals(MobileDriverAdapter.NATIVE_CONTEXT_HANDLE)) {
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
