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
import utam.core.element.Element;
import utam.core.element.Locator;
import utam.core.framework.UtamCoreError;
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
    if(!isPresent()) {
      throw new UtamCoreError("Element is not present, can't wait for its visibility");
    }
    getDriver().waitFor(() -> getElement().isDisplayed(), "wait for element visibility", null);
  }

  @Override
  public final void waitForInvisible() {
    log("wait for element invisibility");
    if(!isPresent()) {
      throw new UtamCoreError("Element is not present, can't wait for its invisibility");
    }
    getDriver().waitFor(() -> !getElement().isDisplayed(), "wait for element invisibility", null);
  }

  @Override
  public final boolean isVisible() {
    log("check element visibility");
    if(!isPresent()) {
      throw new UtamCoreError("Element is absent, can't check its visibility");
    }
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
