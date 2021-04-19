/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.element;

import java.util.function.BiFunction;
import java.util.function.Function;
import utam.core.driver.Driver;
import utam.core.driver.Expectations;
import utam.core.element.Element;

/**
 * expectations act as a function parameter for waits
 *
 * @author elizaveta.ivanova
 * @since 234
 */
public class ExpectationsImpl<T> implements Expectations<T> {

  private final String logMessage;
  private final BiFunction<Driver, Element, T> apply;

  public ExpectationsImpl(String logMessage, BiFunction<Driver, Element, T> apply) {
    this.logMessage = logMessage;
    this.apply = apply;
  }

  public ExpectationsImpl(String logMessage, Function<Driver, T> apply) {
    this(logMessage, (driver, element) -> apply.apply(driver));
  }

   @Override
  public String getLogMessage() {
    return logMessage;
  }

  @Override
  public T apply(Driver driver, Element element) {
    if (element != null && element.isNull()) {
      throw new NullPointerException(
          String.format("Can't apply '%s', element is null", logMessage));
    }
    return apply.apply(driver, element);
  }
}
