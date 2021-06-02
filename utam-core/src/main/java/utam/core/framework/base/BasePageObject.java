/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
/*
 * @Copyright, 1999-2018, salesforce.com
 *  All Rights Reserved
 *  Company Confidential
 *  Project LPOP
 */

package utam.core.framework.base;

import static utam.core.framework.UtamLogger.info;

import utam.core.driver.Document;
import utam.core.driver.Expectations;
import utam.core.element.Element;
import utam.core.element.ElementLocation;
import utam.core.element.FindContext;
import utam.core.element.Locator;
import utam.core.framework.base.CustomElementBuilder.External;
import utam.core.framework.consumer.ContainerElement;
import utam.core.framework.consumer.UtamError;
import utam.core.framework.element.BasePageElement;
import utam.core.framework.element.DocumentObject;
import utam.core.framework.element.ExpectationsImpl;

import java.util.function.Supplier;

/**
 * base class for any UTAM page object
 *
 * @author elizaveta.ivanova
 * @since 228
 */
public abstract class BasePageObject implements RootPageObject {

  // lazy element injected by factory
  private BasePageElement rootElement;
  // has to be protected as used in "inScope" method from POs
  protected ElementLocation root;
  // lazy factory injected by factory
  private PageObjectsFactory factory;
  // lazy document injected by factory
  private Document document;

  protected BasePageObject() {
  }

  final void setBootstrap(ElementLocation root, PageObjectsFactory factory) {
    this.root = root;
    this.factory = factory;
  }

  protected final Document getDocument() {
    if (document == null) {
      document = new DocumentObject(getFactory());
    }
    return document;
  }

  protected final ElementLocation getRootLocator() {
    if (root == null) {
      throw new UtamError(getLogMessage("root element locator is null"));
    }
    return root;
  }

  protected final BasePageElement getRootElement() {
    if (rootElement == null) {
      Element element = getFactory().findElement(getRootLocator());
      rootElement = element.isNull() ? null : new BasePageElement(getFactory(), element);
    }
    return rootElement;
  }

  private PageObjectsFactory getFactory() {
    return factory;
  }

  private void log(String message) {
    info(getLogMessage(message));
  }

  private String getLogMessage(String message) {
    return String.format("Page Object '%s': %s", getClass().getSimpleName(), message);
  }

  @Override
  public void load() {
    log("find page object root element");
    getRootElement();
    if (rootElement == null) {
      throw new NullPointerException(getLogMessage(String
          .format("root element not found with locator '%s'",
              root.getLocatorChainString())));
    }
  }

  @Override
  public final boolean isPresent() {
    log("check for root element presence inside its scope");
    return getRootLocator().findElements(getFactory().getDriver()).size() > 0;
  }

  @SuppressWarnings("unused")
  // used by generator - scope inside element of the page object
  protected final CustomElementBuilder inScope(
      ElementLocation scopeElement, Locator selector, boolean isNullable,
      boolean isExpandParentShadow) {
    return new CustomElementBuilder(
        getFactory(), scopeElement, selector,
        FindContext.Type.build(isNullable, isExpandParentShadow));
  }

  @SuppressWarnings("unused")
  // used by generator for external page objects only (result is never nullable)
  protected final CustomElementBuilder inScope(ElementLocation scopeElement, Locator selector,
      boolean isExpandParentShadowRoot) {
    return new External(
        getFactory(), scopeElement, selector, isExpandParentShadowRoot);
  }

  protected final ElementBuilder element(ElementLocation element) {
    return new ElementBuilder(getFactory(), element);
  }

  protected final ContainerElement inContainer(ElementLocation element,
      boolean isExpandShadowRoot) {
    return new ContainerElementImpl(getFactory(), element,
        FindContext.Type.build(false, isExpandShadowRoot));
  }

  @SuppressWarnings({"unchecked", "rawtypes"})
  // used by generator - return imperative utility
  protected final <T extends ImperativeProvider> T getUtility(Class<T> type) {
    T utility = ImperativeProvider.build(type);
    utility.setInstance(this);
    return utility;
  }

  /**
   * wait for condition to return true or not null before the timeout
   *
   * @param condition condition to wait
   * @param <T> return type
   * @return method can only return not null or true
   */
  protected final <T> T waitFor(Supplier<T> condition) {
    Expectations<T> expectations =
            new ExpectationsImpl<>("wait for condition", (driver, element) -> condition.get());
    log(expectations.getLogMessage());
    return getFactory()
            .getDriver()
            .waitFor(
                    getFactory().getDriverContext().getTimeouts().getWaitForTimeout(),
                    getFactory().getDriverContext().getTimeouts().getPollingInterval(),
                    expectations);
  }
}
