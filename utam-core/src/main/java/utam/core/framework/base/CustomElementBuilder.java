/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.base;

import static utam.core.framework.base.BasicElementBuilder.checkNullScope;
import static utam.core.framework.base.BasicElementBuilder.getUnwrappedElement;

import java.util.List;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import utam.core.element.BasicElement;
import utam.core.element.Element;
import utam.core.framework.base.ElementLocation.ElementFound;

/**
 * builder for a custom element (Page Object) scoped inside a page object
 *
 * @author elizaveta.ivanova
 * @since 224
 */
public class CustomElementBuilder {

  private static final String ERR_CANT_FIND_ELEMENT_WITH_FILTER =
      "can't find element [%s] that matches condition";

  private final PageObjectsFactory factory;
  private final Element scope;
  private final ElementLocation elementLocation;

  CustomElementBuilder(
      PageObjectsFactory factory, BasicElement scopeElement, ElementLocation elementLocation) {
    this(factory, getUnwrappedElement(scopeElement), elementLocation);
  }

  CustomElementBuilder(
      PageObjectsFactory factory, Element scopeElement, ElementLocation elementLocation) {
    this.factory = factory;
    this.scope = scopeElement;
    this.elementLocation = elementLocation;
    checkNullScope(scope, elementLocation);
  }

  static String getFilteredElementNotFoundErr(Class type) {
    return String.format(ERR_CANT_FIND_ELEMENT_WITH_FILTER, type.getSimpleName());
  }

  private <T extends PageObject> T getBootstrappedInstance(Class<T> type, ElementFound foundRoot) {
    T poInstance = factory.getPageContext().getBean(type);
    factory.bootstrap(
        poInstance, foundRoot.getFoundElement(), foundRoot.getLocatorWithParameters());
    return poInstance;
  }

  /**
   * scope page object of the custom type if nothing found and element is not nullable, throws an
   * error <br>
   *
   * @param type custom type
   * @param <T> custom generic type
   * @return instance of the Page Object of given type
   */
  public <T extends PageObject> T build(Class<T> type) {
    ElementLocation.ElementFound element = elementLocation.find(scope);
    // if nothing is found and element is nullable - return null
    if (element == null) {
      return null;
    }
    T poInstance = getBootstrappedInstance(type, element);
    poInstance.load();
    return poInstance;
  }

  /**
   * scope page object of the custom type, then find all and apply filter to find first match <br>
   * if nothing found, throws an error <br>
   * if no match for filter found, throws if not nullable
   *
   * @param type custom type
   * @param <T> custom generic type
   * @param filter filter to apply to the found instances
   * @return instance of the Page Object of given type
   */
  public <T extends PageObject> T build(Class<T> type, Predicate<T> filter) {
    List<ElementLocation.ElementFound> elements = elementLocation.findList(scope);
    // if nothing is found and element is nullable - return null
    if (elements == null) {
      return null;
    }

    for (ElementLocation.ElementFound el : elements) {
      T poInstance = getBootstrappedInstance(type, el);
      if (filter.test(poInstance)) {
        poInstance.load();
        return poInstance;
      }
    }

    // if no match found and element is nullable - return null
    if (elementLocation.findContext.isNullable()) {
      return null;
    }
    throw new NullPointerException(getFilteredElementNotFoundErr(type));
  }

  /**
   * scope page object of the custom type, then find all if nothing found, throws an error <br>
   *
   * @param type custom type
   * @param <T> custom generic type
   * @return all found instances of the Page Object of given type
   */
  public <T extends PageObject> List<T> buildList(Class<T> type) {
    List<ElementLocation.ElementFound> elements = elementLocation.findList(scope);
    // if nothing is found and element is nullable - return null
    if (elements == null) {
      return null;
    }
    return elements.stream()
        .map(
            el -> {
              T poInstance = getBootstrappedInstance(type, el);
              poInstance.load();
              return poInstance;
            })
        .collect(Collectors.toList());
  }

  /**
   * scope page object of the custom type, then find all and apply filter<br>
   * if nothing found, throws <br>
   * if no match for filter found, throws if not nullable otherwise returns empty list
   *
   * @param type custom type
   * @param <T> custom generic type
   * @param filter filter to apply to the found instances
   * @return instance of the Page Object of given type
   */
  public <T extends PageObject> List<T> buildList(Class<T> type, Predicate<T> filter) {
    // if element is not nullable - this throws
    List<T> found = buildList(type);

    // if nothing is found and element is nullable - return null
    if (found == null) {
      return null;
    }
    return found.stream().filter(filter).collect(Collectors.toList());
  }
}
