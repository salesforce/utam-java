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

import utam.core.driver.Document;
import utam.core.element.Element;
import utam.core.element.ElementLocation;
import utam.core.element.FindContext;
import utam.core.element.Locator;
import utam.core.framework.consumer.ContainerElement;
import utam.core.framework.consumer.UtamError;
import utam.core.framework.element.BasePageElement;
import utam.core.framework.element.DocumentObject;

/**
 * base class for any UTAM page object, analogue of the UtamBasePageObject in JS library
 *
 * @author elizaveta.ivanova
 * @since 228
 */
public abstract class BasePageObject extends UtamBaseImpl implements RootPageObject {

  // lazy factory injected by factory
  private PageObjectsFactory factory;

  // lazy element injected in runtime when a Page Object is loaded
  private Element rootFound;
  private BasePageElement rootElement;

  // has to be protected as used in "inScope" method from generated page objects
  protected ElementLocation root;
  // lazy document injected by factory
  private Document document;

  protected BasePageObject() {}

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
      throw new UtamError(getLogMessage("page object root locator is null"));
    }
    return root;
  }

  // this method can be called from generated Page Objects when root element is not public
  protected final BasePageElement getRootElement() {
    if (rootElement == null) {
      rootElement = getElement().isNull() ? null : new BasePageElement(getFactory(), getElement());
    }
    return rootElement;
  }

  @Override
  protected final Element getElement() {
    if(rootFound == null) {
      rootFound = getFactory().findElement(getRootLocator());
    }
    return rootFound;
  }

  @Override
  protected final PageObjectsFactory getFactory() {
    return factory;
  }

  @Override
  protected final String getLogMessage(String message) {
    return String.format("Page Object '%s': %s", getClass().getSimpleName(), message);
  }

  @Override
  public void load() {
    log("find page object root element");
    // find if was not already
    Element element = getElement();
    // then check if it was found
    if (element == null || element.isNull()) {
      throw new NullPointerException(getLogMessage(String
          .format("root element not found with locator '%s'",
              getRootLocator().getLocatorChainString())));
    }
  }

  @Override
  public final boolean isPresent() {
    log("check for page object root element presence inside its scope");
    return getRootLocator().findElements(getDriver()).size() > 0;
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
    return new CustomElementBuilder.External(
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
}
