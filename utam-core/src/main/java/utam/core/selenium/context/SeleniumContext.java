package utam.core.selenium.context;

import utam.core.framework.consumer.LocationPolicy;
import utam.core.selenium.expectations.DriverWait;

import java.time.Duration;

/**
 * wrapper for WebDriver and its settings
 *
 * @author elizaveta.ivanova
 * @since 226
 */
public interface SeleniumContext {

  /**
   * get instance of the web driver utilities
   * @return instance of WebDriver utilities
   */
  WebDriverUtilities getWebDriverUtils();

  /**
   * timeout is used for wait
   *
   * @return timeout duration
   */
  Duration getPollingTimeout();

  /**
   * time between polling intervals for waiting for condition
   *
   * @return the polling interval duration (default is 1000 milliseconds)
   */
  Duration getPollingInterval();

  /**
   * might be set from consumer, for example from core
   *
   * @param pollingInterval polling interval duration
   */
  void setPollingInterval(Duration pollingInterval);

  /**
   * get FluentWait for webDriver
   * @return instance of FluentWait<WebDriver> for user interactions
   */
  DriverWait getDriverWait();

  /**
   * might be set from consumer, for example from core <br>
   * set long timeout in the context
   *
   * @param timeout long timeout duration
   */
  void setPollingTimeout(Duration timeout);

  /**
   * get location policy type
   * @return instance of location policy
   */
  LocationPolicy getLocationPolicy();
}
