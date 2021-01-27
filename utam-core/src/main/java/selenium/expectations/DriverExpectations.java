package selenium.expectations;

import org.openqa.selenium.WebDriver;

import java.util.function.Function;

/**
 * expectations for PageObject actions such as url navigation
 *
 * @author elizaveta.ivanova
 * @since 222
 */
public interface DriverExpectations<T> {

  /**
   * do Not use boolean as T because webDriver wait considers false same as null
   *
   * @return T
   */
  Function<WebDriver, T> getter();
}
