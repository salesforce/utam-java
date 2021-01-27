/*
 * @Copyright, 1999-2018, salesforce.com
 *  All Rights Reserved
 *  Company Confidential
 *  Project LPOP
 */

package selenium.expectations;

import org.openqa.selenium.SearchContext;
import selenium.context.WebDriverUtilities;

import java.util.function.Function;

/**
 * element expectations
 *
 * @author elizaveta.ivanova
 * @since 226
 */
public interface ElementExpectations<T> {

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
  Function<SearchContext, T> apply(WebDriverUtilities utilities);
}
