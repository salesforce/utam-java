/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.base;

import static utam.core.framework.element.BasePageElement.createInstance;

import java.lang.reflect.Proxy;
import utam.core.driver.Document;
import utam.core.element.BasicElement;
import utam.core.element.Element;
import utam.core.element.ElementLocation;
import utam.core.element.FindContext;
import utam.core.element.Locator;
import utam.core.framework.UtamCoreError;
import utam.core.framework.consumer.ContainerElement;
import utam.core.framework.element.BasePageElement;
import utam.core.framework.element.DocumentObject;

/**
 * base class for any UTAM page object, analogue of the UtamBasePageObject in JS library
 *
 * @author elizaveta.ivanova
 * @since 228
 */
public abstract class BasePageObject extends UtamBaseImpl implements PageObject {

  // has to be protected as used in "inScope" method from generated page objects
  protected ElementLocation root;
  // lazy factory injected by factory
  private PageObjectsFactory factory;
  // lazy document injected by factory
  private Document document;
  // lazy element injected in runtime when a Page Object is loaded
  private Element rootFound = null;
  private BasePageElement utamRootElement = null;

  protected BasePageObject() {
  }

  final void setBootstrap(ElementLocation root, PageObjectsFactory factory) {
    this.factory = factory;
    this.document = new DocumentObject(getFactory());
    this.root = root;
  }

  protected final Document getDocument() {
    return document;
  }

  protected final ElementLocation getRootLocator() {
    return root;
  }

  // this method can be called from generated Page Objects when root element is not public
  protected final BasePageElement getRootElement() {
    if (utamRootElement == null) {
      if (getElement().isNull()) {
        utamRootElement = null;
      } else {
        utamRootElement = createInstance(BasePageElement.class, getElement(), getFactory());
      }
    }
    return utamRootElement;
  }

  @Override
  protected final Element getElement() {
    if (rootFound == null) {
      this.rootFound = root.findElement(getFactory().getDriver());
    }
    return rootFound;
  }

  @Override
  protected final PageObjectsFactory getFactory() {
    return factory;
  }

  @Override
  final String getLogMessage(String message) {
    return String.format("Page Object '%s': %s", getClass().getSimpleName(), message);
  }

  final BasePageObject findRootAndCheckNotNull(boolean isThrowIfNotFound) {
    getElement();
    if (isThrowIfNotFound && (rootFound == null || rootFound.isNull())) {
      throw new NullPointerException(getLogMessage(String
          .format("root element not found with locator '%s'", root.getLocatorChainString())));
    }
    getRootElement();
    return this;
  }

  @Override
  public Object load() {
    log("find page object root element");
    return findRootAndCheckNotNull(true);
  }

  @Override
  public final boolean isPresent() {
    log("check for page object root element presence inside its scope");
    return getRootElement() != null && getRootElement().isPresent();
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

  protected final BasicElementBuilder element(ElementLocation element) {
    return new BasicElementBuilder(getFactory(), element);
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
   * create instance of the proxy type that implements custom union type
   *
   * @param element   this element will be used as a proxy
   * @param unionType interface to implement
   * @param <T>       type bound
   * @return instance of the proxy object
   */
  protected final <T extends BasicElement> T proxy(BasePageElement element,
      Class<T> unionType) {
    return (T) Proxy.newProxyInstance(
        this.getClass().getClassLoader(),
        new Class[]{unionType},
        (proxy, method, args) -> {
          try {
            method.setAccessible(true);
            return method.invoke(element, args);
          } catch (Exception e) {
            throw new UtamCoreError(String.format("Unable to invoke method '%s'", method.getName()),
                e);
          }
        }
    );
  }
}
