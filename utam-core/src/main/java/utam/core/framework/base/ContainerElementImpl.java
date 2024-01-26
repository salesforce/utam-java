/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */

package utam.core.framework.base;

import static utam.core.framework.base.BasicElementBuilder.getUnwrappedElement;

import java.util.Collections;
import java.util.List;
import org.openqa.selenium.NoSuchElementException;
import utam.core.element.BasicElement;
import utam.core.element.Element;
import utam.core.element.FindContext;
import utam.core.element.Locator;
import utam.core.framework.consumer.Contained;
import utam.core.framework.consumer.ContainerElement;
import utam.core.selenium.element.ElementAdapter;
import utam.core.selenium.element.LocatorBy;

/**
 * element exposed as a container
 *
 * @author elizaveta.ivanova
 * @since 228
 */
class ContainerElementImpl implements ContainerElement {

  static final String NULL_SCOPE_ERR = "Container scope can't be null";

  private final PageObjectsFactory factory;
  final Element containerScope;
  private final FindContext findContext;

  ContainerElementImpl(
      PageObjectsFactory factory, BasicElement scopeElement, boolean isExpandShadowRoot) {
    this(factory, getUnwrappedElement(scopeElement), isExpandShadowRoot);
  }

  ContainerElementImpl(
      PageObjectsFactory factory, Element scopeElement, boolean isExpandShadowRoot) {
    this.factory = factory;
    this.containerScope = scopeElement;
    this.findContext = FindContext.Type.build(false, isExpandShadowRoot);
    if (containerScope == null) {
      throw new NoSuchElementException(NULL_SCOPE_ERR);
    }
  }

  ContainerElementImpl(ContainerElementImpl containerElement) {
    this(
        containerElement.factory,
        containerElement.containerScope,
        containerElement.findContext.isExpandScopeShadowRoot());
  }

  @Override
  @Deprecated
  public void setScope(Contained externalObjectInsideContainer) {
    externalObjectInsideContainer.setScope(((ElementAdapter) containerScope)::getWebElement);
  }

  @Override
  @Deprecated
  public <T extends PageObject> T load(Class<T> utamType, String injectCss) {
    return load(utamType, LocatorBy.byCss(injectCss));
  }

  private ContainerElementPageObject getContainerElementPageObject(Locator locator) {
    ElementLocation elementLocation = new ElementLocation(locator, findContext);
    // findContext not nullable so can't be null
    Element scopeElement = elementLocation.find(this.containerScope).getFoundElement();
    ContainerElementImpl containerElement =
        new ContainerElementImpl(factory, scopeElement, findContext.isExpandScopeShadowRoot());
    return new ContainerElementPageObject(containerElement);
  }

  private boolean isCompatibilityMode(Class type) {
    return ContainerElementPageObject.class.equals(type);
  }

  @Override
  public <T extends PageObject> T load(Class<T> utamPageObject, Locator rootLocator) {
    if (isCompatibilityMode(utamPageObject)) {
      return (T) getContainerElementPageObject(rootLocator);
    }
    ElementLocation elementLocation = new ElementLocation(rootLocator, findContext);
    return new CustomElementBuilder(factory, containerScope, elementLocation).build(utamPageObject);
  }

  @Override
  public <T extends PageObject> List<T> loadList(Class<T> type, Locator locator) {
    if (isCompatibilityMode(type)) {
      return Collections.singletonList((T) getContainerElementPageObject(locator));
    }
    ElementLocation elementLocation = new ElementLocation(locator, findContext);
    return new CustomElementBuilder(factory, containerScope, elementLocation).buildList(type);
  }
}
