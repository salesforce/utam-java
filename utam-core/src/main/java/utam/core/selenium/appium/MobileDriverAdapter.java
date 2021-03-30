package utam.core.selenium.appium;

import io.appium.java_client.AppiumDriver;
import java.time.Duration;
import java.util.Set;
import utam.core.driver.Driver;
import utam.core.driver.DriverContext;
import utam.core.driver.DriverTimeouts;
import utam.core.framework.element.ExpectationsImpl;
import utam.core.framework.consumer.UtamError;
import utam.core.selenium.element.DriverAdapter;

/**
 * Appium Driver wrapper and context
 *
 * @author qren
 * @since 228
 */
@SuppressWarnings("rawtypes")
public class MobileDriverAdapter extends DriverAdapter implements Driver {

  static final String WEBVIEW_CONTEXT_HANDLE_PREFIX = "WEBVIEW";
  static final String NATIVE_CONTEXT_HANDLE = "NATIVE_APP";
  static final String ERR_BRIDGE_TITLE_NULL =
      "Bridge application title is null, "
          + "use setBridgeAppTitle from PageObjectsProvider to setup before getting an instance of driver";
  static final ExpectationsImpl<AppiumDriver, Boolean> WEBVIEW_CONTEXT = new ExpectationsImpl<>(
      "wait for available web view", (driver, appiumDriver) -> {
    Set<String> contextHandles = appiumDriver.getContextHandles();
    return contextHandles.stream().
        anyMatch(handle -> handle.contains(MobileDriverAdapter.WEBVIEW_CONTEXT_HANDLE_PREFIX));
  });
  static final ExpectationsImpl<String, AppiumDriver> SWITCH_TO_WEB_VIEW = new ExpectationsImpl<>(
      "switch to web view",
      MobileDriverUtils::switchToWebView);
  private final String currentWebViewTitle;

  public MobileDriverAdapter(AppiumDriver driver, DriverContext driverContext) {
    super(driver, driverContext);
    this.currentWebViewTitle = driverContext.getBridgeAppTitle();
  }

  DriverTimeouts getTimeouts() {
    return this.driverContext.getTimeouts();
  }

  @Override
  public void setPageContextToNative() {
    if (!isNative()) {
      getAppiumDriver().context(NATIVE_CONTEXT_HANDLE);
    }
  }

  @Override
  public Driver setPageContextToWebView() {
    return setPageContextToWebView(currentWebViewTitle);
  }

  @Override
  public Driver setPageContextToWebView(String title) {
    if (title == null) {
      throw new UtamError(ERR_BRIDGE_TITLE_NULL);
    }
    if (!isNative() && driver.getTitle().equalsIgnoreCase(title)) {
      return this;
    }
    Duration timeout = driverContext.getTimeouts().getWaitForTimeout();
    Duration interval = driverContext.getTimeouts().getPollingInterval();
    waitFor(timeout, interval, WEBVIEW_CONTEXT, getAppiumDriver());
    AppiumDriver newDriver = waitFor(timeout, interval, SWITCH_TO_WEB_VIEW, title);
    return new MobileDriverAdapter(newDriver, driverContext);
  }

  @Override
  public boolean isNative() {
    return NATIVE_CONTEXT_HANDLE.equals(getAppiumDriver()
        .getContext());
  }

  AppiumDriver getAppiumDriver() {
    return (AppiumDriver) driver;
  }

  @Override
  public boolean isMobile() {
    return true;
  }
}
