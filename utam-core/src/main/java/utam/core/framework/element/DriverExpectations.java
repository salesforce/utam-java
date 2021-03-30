package utam.core.framework.element;

import java.util.function.Function;
import utam.core.driver.Driver;

/**
 * driver expectations
 *
 * @author elizaveta.ivanova
 * @since 234
 */
class DriverExpectations<T> extends ExpectationsImpl<Void, T> {

  DriverExpectations(String logMessage, Function<Driver, T> apply) {
    super(logMessage, (driver, v) -> apply.apply(driver));
  }
}
