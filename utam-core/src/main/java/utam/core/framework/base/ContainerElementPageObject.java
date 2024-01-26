/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.base;

import java.util.function.Supplier;
import utam.core.element.Locator;
import utam.core.framework.UtamCoreError;
import utam.core.framework.consumer.ContainerElement;

/**
 * Page Object that acts as a wrapper for a ContainerElement. Only used in compatibility mode.
 *
 * @since 230
 * @deprecated not supported outside Salesforce engineering teams
 */
@Deprecated
public final class ContainerElementPageObject extends ContainerElementImpl
    implements PageObject, ContainerElement {

  static final String ERR_UNSUPPORTED_METHOD =
      "method is not supported for a " + ContainerElementPageObject.class.getSimpleName();

  /**
   * Initializes a new instance of the ContainerElementPageObject class
   *
   * @param container the ContainerElement used for scope in integration with external Page Objects
   */
  ContainerElementPageObject(ContainerElementImpl container) {
    super(container);
  }

  /**
   * Gets the ContainerElement instance
   *
   * @return the ContainerElement used for scope in integration with external Page Objects
   * @deprecated no longer needed, use this very object as container element
   */
  @Deprecated
  public ContainerElementImpl getContainerElement() {
    return this;
  }

  @Override
  public Object load() {
    // instead of throwing, do nothing
    // for compatibility with already existing POs
    return this;
  }

  @Override
  public boolean isPresent() {
    // instead of throwing, return false
    // for compatibility with already existing POs
    return false;
  }

  @Override
  public void waitForAbsence() {
    throw new UtamCoreError(ERR_UNSUPPORTED_METHOD);
  }

  @Override
  public void waitForVisible() {
    throw new UtamCoreError(ERR_UNSUPPORTED_METHOD);
  }

  @Override
  public void waitForInvisible() {
    throw new UtamCoreError(ERR_UNSUPPORTED_METHOD);
  }

  @Override
  public boolean isVisible() {
    throw new UtamCoreError(ERR_UNSUPPORTED_METHOD);
  }

  @Override
  public boolean containsElement(Locator locator, boolean isExpandShadow) {
    throw new UtamCoreError(ERR_UNSUPPORTED_METHOD);
  }

  @Override
  public boolean containsElement(Locator locator) {
    throw new UtamCoreError(ERR_UNSUPPORTED_METHOD);
  }

  @Override
  public <T> T waitFor(Supplier<T> condition) {
    throw new UtamCoreError(ERR_UNSUPPORTED_METHOD);
  }

  @Override
  public <T> T waitFor(Supplier<T> condition, String errorMessage) {
    throw new UtamCoreError(ERR_UNSUPPORTED_METHOD);
  }
}
