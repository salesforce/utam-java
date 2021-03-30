/*
 * @Copyright, 1999-2018, salesforce.com
 *  All Rights Reserved
 *  Company Confidential
 *  Project LPOP
 */

package utam.core.framework.element;

import java.util.function.BiFunction;
import utam.core.driver.Driver;
import utam.core.driver.Expectations;

/**
 * expectations act as a function parameter for waits
 *
 * @author elizaveta.ivanova
 * @since 234
 */
public class ExpectationsImpl<T, R> implements Expectations<T, R> {

  private final String logMessage;
  private final BiFunction<Driver, T, R> apply;
  private final R defaultValue;

  public ExpectationsImpl(String logMessage, BiFunction<Driver, T, R> apply, R defaultValue) {
    this.logMessage = logMessage;
    this.apply = apply;
    this.defaultValue = defaultValue;
  }

  public ExpectationsImpl(String logMessage, BiFunction<Driver, T, R> apply) {
    this(logMessage, apply, null);
  }

  @Override
  public R returnIfFalsy() {
    return defaultValue;
  }

  @Override
  public String getLogMessage() {
    return logMessage;
  }

  @Override
  public R apply(Driver driver, T args) {
    R res = apply.apply(driver, args);
    if (res == null || Boolean.FALSE.equals(res)) {
      if (returnIfFalsy() != null) {
        return returnIfFalsy();
      }
    }
    return res;
  }
}
