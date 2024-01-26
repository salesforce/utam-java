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
import utam.core.driver.Navigation;
import utam.core.element.BasicElement;
import utam.core.element.Element;
import utam.core.element.Locator;
import utam.core.framework.UtamCoreError;
import utam.core.framework.consumer.ContainerElement;
import utam.core.framework.element.BasePageElement;

/**
 * base class for any UTAM page object, analogue of the UtamBasePageObject in JS library
 *
 * @author elizaveta.ivanova
 * @since 228
 */
@SuppressWarnings("WeakerAccess")
public abstract class BasePageObject extends UtamBaseImpl implements PageObject {

  PageObjectsFactory factory;
  BasePageElement rootElement;
  private Document document;
  private Navigation navigation;
  private Locator locatorInsideScope;

  /** Initializes a new instance of the BasePageObject class. */
  protected BasePageObject() {}

  /**
   * Get instance of the root element. Not Nullable. Method is not final because overridden for Root
   * page objects.
   *
   * @return instance of the root element, invoked to expose root
   */
  protected BasePageElement getRootElement() {
    if (rootElement == null) {
      if (getElement() == null) {
        throw new NullPointerException("Root element not set, report a bug");
      }
      rootElement = createInstance(getElement(), getDriver());
    }
    return rootElement;
  }

  /**
   * Gets the root locator of the root element. Should not be used outside unit tests.
   *
   * @return a Locator object containing the selector of the root element
   */
  // used in unit tests
  protected final Locator getRootLocator() {
    return locatorInsideScope;
  }

  /**
   * Gets the document object.
   *
   * @return the document object.
   */
  protected final Document getDocument() {
    return document;
  }

  /**
   * Gets the navigation object.
   *
   * @return the navigation object.
   */
  protected final Navigation getNavigation() {
    return navigation;
  }

  /**
   * During bootstrap assign values injected by page objects factory
   *
   * @param factory instance of the factory
   * @param element root element (not null!)
   * @param locator root locator (not null!)
   * @param document singleton instance of the document object
   * @param navigation singleton instance of the navigation object
   */
  final void initialize(
      PageObjectsFactory factory,
      Element element,
      Locator locator,
      Document document,
      Navigation navigation) {
    this.factory = factory;
    this.locatorInsideScope = locator;
    this.document = document;
    this.navigation = navigation;
    setDriver(factory.getDriver());
    setElement(element);
  }

  private PageObjectsFactory getFactory() {
    return factory;
  }

  @Override
  final String getLogMessage(String message) {
    return String.format("Page Object '%s': %s", getClass().getSimpleName(), message);
  }

  @Override
  public Object load() {
    // for non root PO element is already found, for root we override this method
    return this;
  }

  @Override
  public final boolean isPresent() {
    log("check for page object root element presence inside its scope");
    return getElement().isExisting();
  }

  /**
   * scopes custom element in certain location
   *
   * @param scopeElement scope element (can be root)
   * @param location locator and expand/nullable flags
   * @return builder
   */
  protected final CustomElementBuilder custom(BasicElement scopeElement, ElementLocation location) {
    return new CustomElementBuilder(getFactory(), scopeElement, location);
  }

  /**
   * scopes basic element in certain location
   *
   * @param scopeElement scope element (can be root)
   * @param location locator and expand/nullable flags
   * @return builder
   */
  protected final BasicElementBuilder basic(BasicElement scopeElement, ElementLocation location) {
    return new BasicElementBuilder(getFactory(), scopeElement, location);
  }

  /**
   * scopes custom element in certain location
   *
   * @param scopeElement scope element (can be root)
   * @param isExpandShadowRoot expand flag
   * @return builder
   */
  protected final ContainerElement container(
      BasicElement scopeElement, boolean isExpandShadowRoot) {
    return new ContainerElementImpl(getFactory(), scopeElement, isExpandShadowRoot);
  }

  /**
   * Gets the instance of the imperative extensions for this Page Object
   *
   * @param type Class of the class containing the imperative extensions for this Page Object
   * @param <T> type name of the imperative extension class
   * @return the instance of the class containing the imperative extensions for thie Page Object
   */
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
   * @param element this element will be used as a proxy
   * @param unionType interface to implement
   * @param <T> type bound
   * @return instance of the proxy object
   */
  protected final <T extends BasicElement> T getProxy(BasePageElement element, Class<T> unionType) {
    return (T)
        Proxy.newProxyInstance(
            this.getClass().getClassLoader(),
            new Class[] {unionType, HasElementGetter.class},
            (proxy, method, args) -> {
              try {
                // without it reflection throws "object is not an instance of declaring class"
                // because
                if (method.getName().equals("getElement")) {
                  return element.getElement();
                } else {
                  method.setAccessible(true);
                  // NB: DURING DEBUG STEP INSIDE THIS METHOD
                  return method.invoke(element, args);
                }
              } catch (Exception e) {
                throw new UtamCoreError(
                    String.format("Unable to invoke method '%s'", method.getName()), e);
              }
            });
  }

  /**
   * dummy interface whose sole purpose is to have getElement method so that it'd be called for a
   * proxy instance. interface should be public otherwise proxy throws IllegalArgumentException:
   * non-public interfaces from different packages
   */
  @SuppressWarnings("unused")
  public interface HasElementGetter {

    Element getElement();
  }
}
