/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.selenium.expectations;

import org.openqa.selenium.WebElement;
import utam.core.selenium.context.WebDriverUtilities;

import java.util.List;
import java.util.function.BiFunction;
import java.util.function.Function;

/**
 * @author elizaveta.ivanova
 * @since 224
 */
final class AbstractListExpectation<T> implements ElementListExpectations<T> {

  private final String log;
  private final BiFunction<WebDriverUtilities, List<WebElement>, T> apply;
  private final T returns;

  AbstractListExpectation(String log, Function<List<WebElement>, T> apply, T returns) {
    this.log = log;
    this.apply = (wu, l) -> apply.apply(l);
    this.returns = returns;
  }

  AbstractListExpectation(String log, Function<List<WebElement>, T> apply) {
    this(log, apply, null);
  }

  @Override
  public Function<List<WebElement>, T> apply(WebDriverUtilities utilities) {
    return list -> apply.apply(utilities, list);
  }

  @Override
  public String getLogMessage() {
    return log;
  }

  @Override
  public T returnIfNothingFound() {
    return returns;
  }
}
