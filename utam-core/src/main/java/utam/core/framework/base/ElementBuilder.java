/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.base;

import java.util.List;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import utam.core.element.Actionable;
import utam.core.element.Element;
import utam.core.element.ElementLocation;
import utam.core.framework.consumer.UtamError;
import utam.core.framework.element.BasePageElement;

/**
 * builder for page object element
 *
 * @author elizaveta.ivanova
 * @since 232
 */
public class ElementBuilder {

  private final PageObjectsFactory factory;
  private final ElementLocation elementFinder;

  ElementBuilder(PageObjectsFactory factory, ElementLocation elementFinder) {
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
  public <T extends Actionable> T build(Class<T> type, Object... values) {
    ElementLocation elementLocation = this.elementFinder.setParameters(values);
    Element element = factory.findElement(elementLocation);
    if(element.isNull()) {
      return null;
    }
    return (T) new BasePageElement(factory, element);
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
  public <T extends Actionable> T build(Class<T> type, Predicate<T> filter, Object... values) {
    List<T> list = buildList(type, values);
    if (list == null) {
      return null;
    }
    for (T t : list) {
      if (filter.test(t)) {
        return t;
      }
    }
    throw new UtamError("can't find matching element");
  }

  /**
   * set parameters in actionable, then find all elements and return list
   *
   * @param type       type of the actionable
   * @param parameters selector parameters values, can be empty
   * @param <T>        element type
   * @return list of instances with index in selector
   */
  public <T extends Actionable> List<T> buildList(Class<T> type, Object... parameters) {
    ElementLocation elementFinder = this.elementFinder.setParameters(parameters);
    List<Element> elementsFound = factory.findElements(elementFinder);
    if (elementsFound == null || elementsFound.isEmpty()) {
      return null;
    }
    return (List<T>) elementsFound.stream().map(el -> new BasePageElement(factory, el))
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
  public <T extends Actionable> List<T> buildList(Class<T> type, Predicate<T> filter,
      Object... values) {
    List<T> list = buildList(type, values);
    if (list.isEmpty()) {
      throw new UtamError("can't find matching element");
    }
    return list.stream().filter(filter).collect(Collectors.toList());
  }
}
