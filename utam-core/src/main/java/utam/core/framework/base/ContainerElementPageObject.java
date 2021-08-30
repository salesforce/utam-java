/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.base;

import java.util.List;
import java.util.function.Supplier;
import utam.core.element.ElementLocation;
import utam.core.element.FindContext;
import utam.core.element.Locator;
import utam.core.framework.UtamCoreError;
import utam.core.framework.consumer.Contained;
import utam.core.framework.consumer.ContainerElement;
import utam.core.selenium.element.LocatorBy;

/**
 * Page Object that acts as a wrapper for a ContainerElement. Only used in compatibility mode.
 * @since 230
 */
public final class ContainerElementPageObject implements PageObject, ContainerElement {

  static final String ERR_UNSUPPORTED_METHOD =
      "method is not supported for a " + ContainerElementPageObject.class.getSimpleName();
  private final ContainerElement container;

  /**
   * Initializes a new instance of the ContainerElementPageObject class
   *
   * @param container the ContainerElement used for scope in integration with external Page Objects
   */
  public ContainerElementPageObject(ContainerElement container) {
    this.container = container;
  }

  // for testing
  ElementLocation getRootLocationChain() {
    return ((ContainerElementImpl) container).containerRoot;
  }

  // for testing
  FindContext getFinderContext() {
    return ((ContainerElementImpl) container).finderContext;
  }

  // for testing
  PageObjectsFactory getFactory() {
    return ((ContainerElementImpl) container).factory;
  }

  /**
   * Gets the ContainerElement instance
   *
   * @return the ContainerElement used for scope in integration with external Page Objects
   * @deprecated no longer needed, use this very object as container element
   */
  @Deprecated
  public ContainerElement getContainerElement() {
    return container;
  }

  @Override
  public void load() {
    // instead of throwing, do nothing
    // for compatibility with already existing POs
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
  public void setScope(Contained object) {
    this.container.setScope(object);
  }

  @Override
  public <T extends PageObject> T load(Class<T> utamType, String injectCss) {
    return this.container.load(utamType, injectCss);
  }

  @Override
  public <T extends PageObject> T load(Class<T> utamType, Locator injectSelector) {
    return this.container.load(utamType, injectSelector);
  }

  <T extends PageObject> T test(Class<T> type, String css) {
    return new CustomElementBuilder(getFactory(), getRootLocationChain(), LocatorBy.byCss(css), getFinderContext())
        .test(type);
  }

  @Override
  public <T extends PageObject> List<T> loadList(Class<T> utamType, Locator injectSelector) {
    return this.container.loadList(utamType, injectSelector);
  }
}
