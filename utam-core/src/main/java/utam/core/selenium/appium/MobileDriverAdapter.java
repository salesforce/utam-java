package utam.core.selenium.appium;

import io.appium.java_client.AppiumDriver;
import java.time.Duration;
import java.util.Set;
import utam.core.driver.Driver;
import utam.core.driver.DriverContext;
import utam.core.driver.Expectations;
import utam.core.framework.consumer.UtamError;
import utam.core.framework.element.ExpectationsImpl;
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

  static final Expectations<Boolean> WEBVIEW_AVAILABILITY = new ExpectationsImpl<>(
      "wait for available web view", (driver, element) -> {
    Set<String> contextHandles = ((MobileDriverAdapter) driver).getAppiumDriver()
        .getContextHandles();
    return contextHandles.stream().
        anyMatch(handle -> handle.contains(WEBVIEW_CONTEXT_HANDLE_PREFIX));
  });

  public MobileDriverAdapter(AppiumDriver driver) {
    super(driver);
  }

  static Expectations<AppiumDriver> switchToWebView(String title) {
    return new ExpectationsImpl<>(
        "switch to web view",
        (driver, element) -> MobileDriverUtils.switchToWebView(driver, title));
  }

  @Override
  public void setPageContextToNative() {
    if (!isNative()) {
      getAppiumDriver().context(NATIVE_CONTEXT_HANDLE);
    }
  }

  @Override
  public void setPageContextToWebView(String title, Duration timeout, Duration pollingInterval) {
    if (title == null) {
      throw new UtamError(ERR_BRIDGE_TITLE_NULL);
    }
    if (!isNative() && getAppiumDriver().getTitle().equalsIgnoreCase(title)) {
      return;
    }
    waitFor(timeout, pollingInterval, WEBVIEW_AVAILABILITY);
    AppiumDriver newDriver = waitFor(timeout, pollingInterval, switchToWebView(title));
    resetDriver(newDriver);
  }

  @Override
  public boolean isNative() {
    return NATIVE_CONTEXT_HANDLE.equals(getAppiumDriver()
        .getContext());
  }

  AppiumDriver getAppiumDriver() {
    return (AppiumDriver) getSeleniumDriver();
  }

  @Override
  public boolean isMobile() {
    return true;
  }
}
