/*
 * @Copyright, 1999-2018, salesforce.com
 *  All Rights Reserved
 *  Company Confidential
 *  Project LPOP
 */

package utam.core.driver;

import java.util.function.Function;
import org.openqa.selenium.SearchContext;
import utam.core.selenium.context.WebDriverUtilities;

/**
 * element expectations
 *
 * @author elizaveta.ivanova
 * @since 234
 */
public interface Expectations<S, T> {

  T returnIfNothingFound();

  /**
   * provides log message when expectations are called
   *
   * @return log message to use in logs and in error
   */
  String getLogMessage();

  /**
   * find single element and returns value
   *
   * @return function to apply
   */
  Function<S, T> apply(DriverContext driverContext);
}
