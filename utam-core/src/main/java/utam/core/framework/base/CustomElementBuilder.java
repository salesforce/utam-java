/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.base;

import static utam.core.element.FindContext.Type.EXISTING;

import java.util.List;
import java.util.function.Predicate;
import java.util.function.Supplier;
import java.util.stream.Collectors;
import org.openqa.selenium.SearchContext;
import utam.core.element.Element;
import utam.core.element.ElementLocation;
import utam.core.element.FindContext;
import utam.core.element.FindContext.Type;
import utam.core.element.Locator;
import utam.core.framework.consumer.Contained;
import utam.core.framework.consumer.Container;
import utam.core.framework.consumer.UtamError;
import utam.core.framework.element.ElementLocationChain;
import utam.core.selenium.element.ElementAdapter;

/**
 * builder for a custom element (Page Object) scoped inside a page object
 *
 * @author elizaveta.ivanova
 * @since 224
 */
public class CustomElementBuilder {

  private static final String ERR_CANT_FIND_ELEMENT_WITH_FILTER = "can't find element [%s] that matches condition";

  final PageObjectsFactory factory;
  final ElementLocation root;

  CustomElementBuilder(PageObjectsFactory factory,
      ElementLocation scope,
      Locator rootLocator,
      FindContext finderContext) {
    this.factory = factory;
    this.root = scope != null ? scope.scope(rootLocator, finderContext)
        : new ElementLocationChain(rootLocator, finderContext);
  }

  CustomElementBuilder(PageObjectsFactory factory, ElementLocation root) {
    this.factory = factory;
    this.root = root;
  }

  static String getFilteredElementNotFoundErr(Class type) {
    return String.format(ERR_CANT_FIND_ELEMENT_WITH_FILTER, type.getSimpleName());
  }

  // overridden for external PO as need to inject its root
  <T extends PageObject> T getRawInstance(Class<T> type) {
    T poInstance = factory.getPageContext().getBean(type);
    if (poInstance instanceof Container || poInstance instanceof Contained) {
      throw new UtamError(
          String.format(
              "wrong builder used to scope Page Object '%s'", poInstance.getClass().getName()));
    }
    return poInstance;
  }

  /**
   * scope page object of the custom type if nothing found and element is not nullable, throws an
   * error <br>
   *
   * @param type custom type
   * @param <T>  custom generic type
   * @return instance of the Page Object of given type
   */
  public <T extends PageObject> T build(Class<T> type) {
    T poInstance = getRawInstance(type);
    factory.bootstrap(poInstance, root);

    BasePageObject pageObject = (BasePageObject) poInstance;

    // if nothing is found and element is nullable - return null
    if (pageObject.getElement().isNull()) {
      return null;
    }
    return poInstance;
  }

  /**
   * scope page object of the custom type, then find all and apply filter to find first match <br>
   * if nothing found, throws an error <br> if no match for filter found, throws if not nullable
   *
   * @param type   custom type
   * @param <T>    custom generic type
   * @param filter filter to apply to the found instances
   * @return instance of the Page Object of given type
   */
  public <T extends PageObject> T build(Class<T> type, Predicate<T> filter) {
    // if element is not nullable - this throws an error
    List<Element> found = factory.findElements(root);

    for (Element el : found) {
      T instance = new CustomElementBuilder(factory, new ElementLocationChain(el)).build(type);
      if (filter.test(instance)) {
        return instance;
      }
    }
    // if nothing is found and element is nullable - return null
    if (root.isNullable()) {
      return null;
    }
    throw new NullPointerException(getFilteredElementNotFoundErr(type));
  }

  /**
   * scope page object of the custom type, then find all if nothing found, throws an error <br>
   *
   * @param type custom type
   * @param <T>  custom generic type
   * @return all found instances of the Page Object of given type
   */
  public <T extends PageObject> List<T> buildList(Class<T> type) {
    // if element is not nullable - this throws an error
    List<Element> found = factory.findElements(root);

    // if nothing is found and element is nullable - return null
    if ((found == null || found.isEmpty()) && root.isNullable()) {
      return null;
    }
    return found.stream()
        .map(el -> new CustomElementBuilder(factory, new ElementLocationChain(el)).build(type))
        .collect(Collectors.toList());
  }

  /**
   * scope page object of the custom type, then find all and apply filter<br> if nothing found,
   * throws <br> if no match for filter found, throws if not nullable otherwise returns empty list
   *
   * @param type   custom type
   * @param <T>    custom generic type
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
    return found.stream()
        .filter(filter)
        .collect(Collectors.toList());
  }

  /**
   * UTAM PO as a parent of external PO <br> it needs to get
   */
  static class External extends CustomElementBuilder {

    External(PageObjectsFactory factory, ElementLocation scopeElement, Locator selector,
        boolean isExpandParentShadowRoot) {
      super(factory, scopeElement, selector,
          isExpandParentShadowRoot ? Type.EXISTING_IN_SHADOW : EXISTING);
    }

    @Override
    <T extends PageObject> T getRawInstance(Class<T> type) {
      T instance = factory.getPageContext().getBean(type);
      if (!(instance instanceof Contained)) {
        throw new UtamError(
            String.format(
                "wrong builder used to scope Page Object '%s' inside UTAM parent",
                instance.getClass().getName()));
      }
      Supplier<SearchContext> rootSupplier = () -> {
        ElementAdapter element = (ElementAdapter) factory.findElement(root);
        return element.getWebElement();
      };
      ((Contained) instance).setRoot(rootSupplier);
      return instance;
    }
  }
}
