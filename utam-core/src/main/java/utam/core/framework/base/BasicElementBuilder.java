/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.base;

import static utam.core.framework.base.CustomElementBuilder.getFilteredElementNotFoundErr;

import java.util.List;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import utam.core.element.BasicElement;
import utam.core.element.Element;
import utam.core.element.ElementLocation;
import utam.core.framework.consumer.UtamError;
import utam.core.framework.element.BasePageElement;

/**
 * builder for a basic page object element
 *
 * @author elizaveta.ivanova
 * @since 232
 */
public class BasicElementBuilder {

  private final PageObjectsFactory factory;
  private final ElementLocation elementFinder;

  BasicElementBuilder(PageObjectsFactory factory, ElementLocation elementFinder) {
    this.factory = factory;
    this.elementFinder = elementFinder;
  }

  /**
   * set parameters in actionable
   *
   * @param type   type of the actionable
   * @param values selector parameters values, can be empty
   * @param <T>    element type
   * @return instance with parameters set in selector
   */
  public <T extends BasicElement, R extends BasePageElement> T build(
      Class<T> type, Class<R> implType, Object... values) {
    ElementLocation elementLocation = this.elementFinder.setParameters(values);

    // if element is not nullable - this throws an error
    Element element = factory.findElement(elementLocation);

    // if nothing is found and element is nullable - return null
    if (element.isNull()) {
      return null;
    }
    return createInstance(implType, element);
  }

  private <T extends BasicElement, R extends BasePageElement> T createInstance(
      Class<R> implType, Element element) {
    try {
      R result = implType.getConstructor().newInstance();
      result.initialize(factory, element);
      return (T) result;
    } catch (ReflectiveOperationException e) {
      throw new UtamError(
          String.format("Error creating instance of type '%s'", implType.getSimpleName()),
          e);
    }
  }


  /**
   * set parameters in actionable, then find it and apply filter to return first match
   *
   * @param type   type of the actionable
   * @param filter to apply to found list
   * @param values selector parameters values, can be empty
   * @param <T>    element type
   * @return instance with parameters set in selector
   */
  public <T extends BasicElement, R extends BasePageElement> T build(
      Class<T> type, Class<R> implType, Predicate<T> filter, Object... values) {
    // if element is not nullable - this throws an error
    List<T> list = buildList(type, implType, values);

    if (list == null) {
      return null;
    }
    for (T t : list) {
      if (filter.test(t)) {
        return t;
      }
    }

    // if none found that match condition - throw
    throw new NullPointerException(getFilteredElementNotFoundErr(type));
  }

  /**
   * set parameters in actionable, then find all elements and return list
   *
   * @param type       type of the actionable
   * @param parameters selector parameters values, can be empty
   * @param <T>        element type
   * @return list of instances with index in selector
   */
  public <T extends BasicElement, R extends BasePageElement> List<T> buildList(
      Class<T> type, Class<R> implType, Object... parameters) {
    ElementLocation elementFinder = this.elementFinder.setParameters(parameters);

    // if element is not nullable - this throws an error
    List<Element> elementsFound = factory.findElements(elementFinder);

    // if nothing is found and element is nullable - return null
    if (elementsFound == null || elementsFound.isEmpty()) {
      return null;
    }

    return elementsFound
        .stream()
        .map(el -> (T) createInstance(implType, el))
        .collect(Collectors.toList());
  }

  /**
   * set parameters in actionable, then find it and apply filter to return all elements that match
   *
   * @param type   type of the actionable
   * @param filter to apply to found list
   * @param values selector parameters values, can be empty
   * @param <T>    element type
   * @return instance with parameters set in selector
   */
  public <T extends BasicElement, R extends BasePageElement> List<T> buildList(
      Class<T> type, Class<R> implType, Predicate<T> filter, Object... values) {
    // if element is not nullable - this throws an error
    List<T> list = buildList(type, implType, values);

    // if nothing is found and element is nullable - return null
    if (list == null || list.isEmpty()) {
      return null;
    }

    return list.stream().filter(filter).collect(Collectors.toList());
  }
}
