package utam.core.selenium.context;

import utam.core.framework.consumer.LocationPolicy;
import utam.core.framework.consumer.UtamTimeouts;
import utam.core.selenium.expectations.DriverWait;


/**
 * wrapper for WebDriver and its settings
 *
 * @author elizaveta.ivanova
 * @since 226
 */
public interface SeleniumContext {

  /**
   * get instance of the web driver utilities
   *
   * @return instance of WebDriver utilities
   */
  WebDriverUtilities getWebDriverUtils();

  /**
   * get configured timeouts
   *
   * @return timeouts
   */
  UtamTimeouts getTimeouts();

  /**
   * get FluentWait for webDriver
   *
   * @return instance of FluentWait<WebDriver> for user interactions
   */
  DriverWait getDriverWait();

  /**
   * get location policy type
   *
   * @return instance of location policy
   */
  LocationPolicy getLocationPolicy();
}
