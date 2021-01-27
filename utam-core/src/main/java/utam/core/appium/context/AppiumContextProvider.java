package utam.core.appium.context;

import java.time.Duration;

import org.openqa.selenium.WebDriver;

import utam.core.framework.consumer.LocationPolicyType;
import utam.core.framework.consumer.UtamError;
import io.appium.java_client.AppiumDriver;
import utam.core.selenium.context.SeleniumContextProvider;
import utam.core.selenium.expectations.DriverExpectationsUtil;
import utam.core.selenium.expectations.DriverWait;

/**
 * Appium Driver utilities and context
 *
 * @author qren
 * @since 228
 */
@SuppressWarnings("rawtypes")
public class AppiumContextProvider extends SeleniumContextProvider
    implements AppiumDriverUtilities {
  public static final String WEBVIEW_CONTEXT_HANDLE_PREFIX = "WEBVIEW";
  public static final String NATIVE_CONTEXT_HANDLE = "NATIVE_APP";
  static final String ERR_BRIDGE_TITLE_NULL =
      "Bridge application title is null, "
          + "use setBridgeAppTitle from PageObjectsProvider to setup before getting an instance of driver";
  private final String webViewTitle;

  public AppiumContextProvider(AppiumDriver driver) {
    this(driver, null);
  }

  public AppiumContextProvider(AppiumDriver driver, String title) {
    this(driver, DEFAULT_POLLING_TIMEOUT, title);
  }

  public AppiumContextProvider(AppiumDriver driver, Duration timeout, String title) {
    super(driver, LocationPolicyType.CHAIN, timeout);
    this.webViewTitle = title;
  }

  @Override
  public WebDriver setPageContextToNative() {
    AppiumDriver currentDriver = (AppiumDriver) getWebDriverUtils().getWebDriver();
    if (!isNative()) {
      return currentDriver.context(NATIVE_CONTEXT_HANDLE);
    }
    return currentDriver;
  }

  @Override
  public WebDriver setPageContextToWebView() {
    return setPageContextToWebView(webViewTitle);
  }

  @Override
  public WebDriver setPageContextToWebView(String title) {
    if (title == null) {
      throw new UtamError(ERR_BRIDGE_TITLE_NULL);
    }
    AppiumDriver driver = (AppiumDriver) getWebDriverUtils().getWebDriver();
    DriverWait wait = getDriverWait();
    if (!isNative() && driver.getTitle().equalsIgnoreCase(title)) {
      return driver;
    }
    wait.get(DriverExpectationsUtil.isAnyWebViewContextAvailable());
    return wait.get(DriverExpectationsUtil.switchToWebView(title));
  }

  @Override
  public Boolean isNative() {
    return ((AppiumDriver) getWebDriverUtils().getWebDriver())
        .getContext()
        .equals(NATIVE_CONTEXT_HANDLE);
  }

}
