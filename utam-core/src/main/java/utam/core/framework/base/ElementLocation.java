/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.base;

import java.util.List;
import java.util.stream.Collectors;
import utam.core.element.Element;
import utam.core.element.FindContext;
import utam.core.element.Locator;
import utam.core.selenium.element.ShadowRootElementAdapter;

/**
 * helper class, location of the element inside its scope
 *
 * @author elizaveta.ivanova
 * @since 238
 */
public final class ElementLocation {

  final Locator locator;
  final FindContext findContext;

  ElementLocation(Locator locator, FindContext findContext) {
    this.locator = locator;
    this.findContext = findContext;
  }

  private Element transformScope(Element scope) {
    if (scope == null || !findContext.isExpandScopeShadowRoot()) {
      return scope;
    }
    return new ShadowRootElementAdapter(scope);
  }

  /**
   * apply parameters to locator and find element or return null
   *
   * @param scopeElement search context to find an element
   * @return instance of the found element or null
   */
  ElementFound find(Element scopeElement) {
    // wrap as shadowRoot if need to
    Element transformedScope = transformScope(scopeElement);
    // for nullable element its scope can be null
    if (scopeElement == null && findContext.isNullable()) {
      return null;
    }
    // can return null if element is nullable, otherwise throws
    Element foundElement = transformedScope.findElement(locator, findContext.isNullable());
    // null can be returned for nullable
    return foundElement == null ? null : new ElementFound(locator, foundElement);
  }

  /**
   * apply parameters to locator and find all element or return null
   *
   * @param scopeElement search context to find an element
   * @return list of the found elements or null
   */
  List<ElementFound> findList(Element scopeElement) {
    // wrap as shadowRoot if need to
    Element transformedScope = transformScope(scopeElement);
    // for nullable element its scope can be null
    if (scopeElement == null && findContext.isNullable()) {
      return null;
    }
    // can return null if element is nullable, otherwise throws
    List<Element> foundList = transformedScope.findElements(locator, findContext.isNullable());
    // null only returned for nullable
    if (foundList == null) {
      return null;
    }
    return foundList.stream().map(e -> new ElementFound(locator, e)).collect(Collectors.toList());
  }

  /**
   * apply parameters to locator and return new location
   *
   * @param locatorParameters vararg with locator parameters
   * @return list of the found elements or null
   */
  public ElementLocation setParameters(Object... locatorParameters) {
    Locator locator = this.locator.setParameters(locatorParameters);
    return new ElementLocation(locator, this.findContext);
  }

  /**
   * helper class to store found element and its relative locator
   *
   * @author elizaveta.ivanova
   * @since 238
   */
  static final class ElementFound {

    private final Locator locatorWithParameters;
    private final Element foundElement;

    ElementFound(Locator locatorWithParameters, Element foundElement) {
      this.locatorWithParameters = locatorWithParameters;
      this.foundElement = foundElement;
    }

    /**
     * Get Locator with already applied parameters (because element is found)
     *
     * @return locator instance
     */
    Locator getLocatorWithParameters() {
      return locatorWithParameters;
    }

    /**
     * Get found element, cannot be null
     *
     * @return instance of the found element
     */
    Element getFoundElement() {
      return foundElement;
    }
  }
}
