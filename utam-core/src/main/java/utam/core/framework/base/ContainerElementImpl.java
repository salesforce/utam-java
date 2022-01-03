/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */

package utam.core.framework.base;

import java.util.Collections;
import java.util.List;
import utam.core.element.ElementLocation;
import utam.core.element.FindContext;
import utam.core.element.Locator;
import utam.core.framework.consumer.Contained;
import utam.core.framework.consumer.ContainerElement;
import utam.core.framework.element.ElementLocationChain;
import utam.core.selenium.element.ElementAdapter;
import utam.core.selenium.element.LocatorBy;

/**
 * element exposed as a container
 *
 * @author elizaveta.ivanova
 * @since 228
 */
class ContainerElementImpl implements ContainerElement {

  final PageObjectsFactory factory;
  final ElementLocation containerRoot;
  final FindContext finderContext;

  ContainerElementImpl(PageObjectsFactory factory, ElementLocation containerRoot,
      FindContext finderContext) {
    this.factory = factory;
    this.containerRoot = containerRoot;
    this.finderContext = finderContext;
  }

  ContainerElementImpl(ContainerElementImpl containerElement) {
    this(containerElement.factory, containerElement.containerRoot, containerElement.finderContext);
  }

  @Override
  public void setScope(Contained pageObject) {
    pageObject.setScope(() -> ((ElementAdapter)containerRoot.findElement(factory.getDriver())).getWebElement());
  }

  @Override
  public <T extends PageObject> T load(Class<T> utamType, String injectCss) {
    return load(utamType, LocatorBy.byCss(injectCss));
  }

  private ContainerElementPageObject getContainerElementPageObject(Locator locator) {
    ElementLocation location =
        containerRoot == null ? new ElementLocationChain(locator, finderContext) :
            containerRoot.scope(locator, finderContext);
    ContainerElementImpl containerElement = new ContainerElementImpl(factory, location, finderContext);
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
    T instance = new CustomElementBuilder(factory, containerRoot, rootLocator, finderContext)
        .build(utamPageObject);
    instance.load();
    return instance;
  }

  @Override
  public <T extends PageObject> List<T> loadList(Class<T> type, Locator locator) {
    if (isCompatibilityMode(type)) {
      return Collections.singletonList((T) getContainerElementPageObject(locator));
    }
    return new CustomElementBuilder(factory, containerRoot, locator, finderContext).buildList(type);
  }
}
