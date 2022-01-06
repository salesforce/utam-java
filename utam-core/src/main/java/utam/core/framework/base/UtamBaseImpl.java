/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.base;

import java.util.function.Supplier;
import utam.core.driver.Driver;
import utam.core.element.Element;
import utam.core.element.Locator;
import utam.core.framework.UtamLogger;

/**
 * abstraction base for an element and a page object
 *
 * @author elizaveta.ivanova
 * @since 234
 */
public abstract class UtamBaseImpl implements UtamBase {

  private Driver driver;
  private Element element;

  protected UtamBaseImpl() {
  }

  // not final to allow mocks from tests
  protected Element getElement() {
    return element;
  }

  // not final to allow mocks from tests
  protected Driver getDriver() {
    return driver;
  }

  protected final void setElement(Element element) {
    this.element = element;
  }

  protected final void setDriver(Driver driver) {
    this.driver = driver;
  }

  protected final void log(String message) {
    UtamLogger.info(getLogMessage(message));
  }

  String getLogMessage(String message) {
    return message;
  }

  @Override
  public final <T> T waitFor(Supplier<T> condition) {
    log("wait for condition");
    return getDriver().waitFor(condition, null, null);
  }

  @Override
  public final void waitForAbsence() {
    log("wait for element absence");
    getDriver().waitFor(() -> !getElement().isExisting(), "wait for element absence", null);
  }

  @Override
  public final void waitForVisible() {
    log("wait for element visibility");
    getDriver().waitFor(() -> getElement().isDisplayed(), "wait for element visibility", null);
  }

  @Override
  public final void waitForInvisible() {
    log("wait for element invisibility");
    getDriver().waitFor(() -> !getElement().isDisplayed(), "wait for element invisibility", null);
  }

  @Override
  public final boolean isVisible() {
    log("check element visibility");
    return getElement().isDisplayed();
  }

  @Override
  public final boolean containsElement(Locator locator, boolean isExpandShadow) {
    return getElement().containsElements(locator, isExpandShadow) > 0;
  }

  @Override
  public final boolean containsElement(Locator locator) {
    return containsElement(locator, false);
  }

  @Override
  public boolean isPresent() {
    return getElement().isExisting();
  }
}
