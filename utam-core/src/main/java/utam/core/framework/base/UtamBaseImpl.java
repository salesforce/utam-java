/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.base;

import static utam.core.element.FindContext.Type.NULLABLE;
import static utam.core.element.FindContext.Type.NULLABLE_IN_SHADOW;

import java.util.function.Supplier;
import utam.core.driver.Driver;
import utam.core.driver.DriverTimeouts;
import utam.core.driver.Expectations;
import utam.core.element.Element;
import utam.core.element.Locator;
import utam.core.framework.UtamLogger;
import utam.core.framework.element.ExpectationsImpl;

/**
 * abstraction base for an element and a page object
 *
 * @author elizaveta.ivanova
 * @since 234
 */
public abstract class UtamBaseImpl implements UtamBase {

  protected UtamBaseImpl() {}

  protected abstract Element getElement();

  protected abstract PageObjectsFactory getFactory();

  protected final Driver getDriver() {
    return getFactory().getDriver();
  }

  protected final DriverTimeouts getDriverTimeouts() {
    return getFactory().getDriverContext().getTimeouts();
  }

  protected final void log(String message) {
    UtamLogger.info(getLogMessage(message));
  }

  String getLogMessage(String message) {
    return message;
  }

  /**
   * wait for condition to return true or not null before the timeout
   *
   * @param condition condition to wait
   * @param <T> return type
   * @return method can only return not null or true
   */
  @Override
  public final <T> T waitFor(Supplier<T> condition) {
    Expectations<T> expectations =
        new ExpectationsImpl<>("wait for condition", (driver) -> {
          try {
            return condition.get();
          } catch (Exception e) {
            return null;
          }
        });
    log(expectations.getLogMessage());
    return getDriver().waitFor(getDriverTimeouts().getWaitForTimeout(), getDriverTimeouts().getPollingInterval(), expectations);
  }

  private <T> T waitFor(Expectations<T> expectations) {
    log(expectations.getLogMessage());
    return getDriver().waitFor(
        getDriverTimeouts().getWaitForTimeout(),
        getDriverTimeouts().getPollingInterval(),
        expectations,
        getElement());
  }

  @Override
  public final void waitForAbsence() {
    Expectations<Boolean> expectations = new ExpectationsImpl<>(
        "wait for absence", (driver, element) -> !element.isExisting());
    waitFor(expectations);
  }

  @Override
  public final void waitForVisible() {
    Expectations<Boolean> expectations = new ExpectationsImpl<>(
        "wait for element visibility",
        (driver, element) -> element.isDisplayed());
    waitFor(expectations);
  }

  @Override
  public final void waitForInvisible() {
    Expectations<Boolean> expectations = new ExpectationsImpl<>(
        "wait for element invisibility",
        (driver, element) -> !element.isDisplayed());
    waitFor(expectations);
  }

  @Override
  public final boolean isVisible() {
    return getElement().isDisplayed();
  }

  @Override
  public final boolean containsElement(Locator locator, boolean isExpandShadow) {
    return
        getElement().findElements(locator, isExpandShadow ? NULLABLE_IN_SHADOW : NULLABLE).size()
            > 0;
  }

  @Override
  public final boolean containsElement(Locator locator) {
    return containsElement(locator, false);
  }

  @Override
  public boolean isPresent() {
    return !getElement().isNull();
  }
}
