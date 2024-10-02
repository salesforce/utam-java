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

  // package access to use in unit tests
  final PageObjectsFactory factory;
  final Element containerScope;
  boolean isExpandShadowRoot = false;
  boolean isNullable = false;
  FindContext findContext;

  ContainerElementImpl(PageObjectsFactory factory, BasicElement scopeElement) {
    this(factory, getUnwrappedElement(scopeElement));
  }

  ContainerElementImpl(PageObjectsFactory factory, Element scopeElement) {
    this.factory = factory;
    this.containerScope = scopeElement;
    if (containerScope == null) {
      throw new NoSuchElementException(NULL_SCOPE_ERR);
    }
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
    ContainerElement containerElement =
        new ContainerElementImpl(factory, scopeElement)
            .expandShadowRoot(findContext.isExpandScopeShadowRoot())
            .nullable(findContext.isNullable())
            .build();
    return new ContainerElementPageObject((ContainerElementImpl) containerElement);
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

  @Override
  public ContainerElement expandShadowRoot(boolean isExpand) {
    this.isExpandShadowRoot = isExpand;
    return this;
  }

  @Override
  public ContainerElement nullable(boolean isNullable) {
    this.isNullable = isNullable;
    return this;
  }

  @Override
  public ContainerElement build() {
    if (this.findContext == null) {
      this.findContext = FindContext.Type.build(this.isNullable, this.isExpandShadowRoot);
    }
    return this;
  }
}
