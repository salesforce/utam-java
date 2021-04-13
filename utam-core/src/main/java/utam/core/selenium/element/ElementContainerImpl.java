/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.selenium.element;

import utam.core.framework.base.ContainerElementPageObject;
import utam.core.framework.base.PageObjectBuilderImpl;
import utam.core.framework.base.PageObject;
import utam.core.framework.base.PageObjectsFactory;
import utam.core.framework.consumer.Contained;
import utam.core.framework.consumer.ContainerElement;
import utam.core.framework.consumer.LocationPolicy;

import java.util.Collections;
import java.util.List;

import static utam.core.selenium.element.LocatorUtilities.buildLocator;

/**
 * element exposed as container
 *
 * @author elizaveta.ivanova
 * @since 228
 */
final class ElementContainerImpl extends ElementImpl implements ContainerElement {

  private final PageObjectsFactory factory;
  private final Locator locator;
  private final boolean isExpandShadow;

  ElementContainerImpl(Locator locator, PageObjectsFactory factory, boolean isExpandShadow) {
    super(locator, factory.getSeleniumContext());
    this.factory = factory;
    this.isExpandShadow = isExpandShadow;
    this.locator = locator;
  }

  @Override
  public void setScope(Contained pageObject) {
    pageObject.setScope(() -> find(isExpandShadow));
  }

  @SuppressWarnings("unused")
  @Override
  public <T extends PageObject> T load(Class<T> utamType, String injectCss) {
    return load(utamType, Web.byCss(injectCss));
  }

  @Override
  public <T extends PageObject> T load(Class<T> utamType, Selector selector) {
    if (utamType.equals(ContainerElementPageObject.class)) {
      return (T) (new ContainerElementPageObject(this));
    }
    LocationPolicy policy = factory.getSeleniumContext().getLocationPolicy();
    return new PageObjectBuilderImpl(factory, locator, false, buildLocator(policy, selector, isExpandShadow))
        .build(utamType);
  }

  @Override
  public <T extends PageObject> List<T> loadList(Class<T> utamType, Selector injectSelector) {
    if (utamType.equals(ContainerElementPageObject.class)) {
      return Collections.singletonList((T) (new ContainerElementPageObject(this)));
    }
    LocationPolicy policy = factory.getSeleniumContext().getLocationPolicy();
    return new PageObjectBuilderImpl(factory, locator, false, buildLocator(policy, injectSelector, isExpandShadow))
        .buildList(utamType);
  }
  @Override
  public boolean isExpandScopeShadow() {
    return isExpandShadow;
  }
}
