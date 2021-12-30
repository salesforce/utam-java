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

  /**
   * apply parameters to locator and find element or return null
   *
   * @param scope             search context to find an element
   * @param locatorParameters vararg with locator parameters
   * @return instance of the found element or null
   */
  ElementFound find(Element scope, Object... locatorParameters) {
    Locator locator = this.locator.setParameters(locatorParameters);
    if (findContext.isNullable()
        && scope.containsElements(locator, findContext.isExpandScopeShadowRoot()) <= 0) {
      return null;
    }
    Element foundElement = scope.findElement(locator, findContext);
    return new ElementFound(locator, foundElement);
  }

  /**
   * Get found Element or throw an error if nothing was found
   *
   * @param scope search context to find an element
   * @return instance of the found element
   */
  Element findNotNullElement(Element scope) {
    ElementFound found = find(scope);
    if (found == null) {
      throw new NullPointerException(
          String.format("Can't find element with locator '%s'", locator.getStringValue()));
    }
    return found.getFoundElement();
  }

  /**
   * apply parameters to locator and find all element or return null
   *
   * @param scope             search context to find an element
   * @param locatorParameters vararg with locator parameters
   * @return list of the found elements or null
   */
  List<ElementFound> findList(Element scope, Object... locatorParameters) {
    Locator locator = this.locator.setParameters(locatorParameters);
    if (findContext.isNullable()
        && scope.containsElements(locator, findContext.isExpandScopeShadowRoot()) <= 0) {
      return null;
    }
    List<Element> foundList = scope.findElements(locator, findContext);
    return foundList
        .stream()
        .map(e -> new ElementFound(locator, e))
        .collect(Collectors.toList());
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
