/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.base;

import static utam.core.framework.base.CustomElementBuilder.getFilteredElementNotFoundErr;
import static utam.core.framework.element.BasePageElement.createInstance;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.List;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import org.openqa.selenium.NoSuchElementException;
import utam.core.driver.Driver;
import utam.core.element.BasicElement;
import utam.core.element.Element;
import utam.core.element.FrameElement;
import utam.core.framework.UtamCoreError;
import utam.core.framework.element.BasePageElement;

/**
 * builder for a basic page object element
 *
 * @author elizaveta.ivanova
 * @since 232
 */
public class BasicElementBuilder {

  static final String NULL_SCOPE_ERR =
      "Scope of locator '%s' can't be null because element is not marked as nullable";

  private final Driver driver;
  private final Element scope;
  private final ElementLocation elementLocation;

  BasicElementBuilder(
      PageObjectsFactory factory, BasicElement scopeElement, ElementLocation elementLocation) {
    this.driver = factory.getDriver();
    this.scope = getUnwrappedElement(scopeElement);
    this.elementLocation = elementLocation;
    checkNullScope(scope, elementLocation);
  }

  static void checkNullScope(Element scope, ElementLocation elementLocation) {
    if (scope == null && !elementLocation.findContext.isNullable()) {
      throw new NoSuchElementException(
          String.format(NULL_SCOPE_ERR, elementLocation.locator.getStringValue()));
    }
  }

  /**
   * Unwrap element that is used as a scope from basic element or proxy
   *
   * @param basicElement instance of the element
   * @return unwrapped Element
   */
  public static Element getUnwrappedElement(BasicElement basicElement) {
    if (basicElement
        == null) { // sometimes scope can be passed as null if element is inside nullable
      return null;
    }
    if (basicElement instanceof BasePageElement) {
      return ((BasePageElement) basicElement).getElement();
    }
    try {
      Method method = basicElement.getClass().getDeclaredMethod("getElement");
      method.setAccessible(true);
      return (Element) method.invoke(basicElement);
    } catch (NoSuchMethodException | IllegalAccessException | InvocationTargetException e) {
      throw new UtamCoreError("Can't invoke getElement on proxy class", e);
    }
  }

  /**
   * set parameters in actionable
   *
   * @param type interface type of the actionable
   * @param implType implementation type of the actionable
   * @param <T> element type
   * @param <R> implementation type
   * @return instance with parameters set in selector
   */
  public <T extends BasicElement, R extends BasePageElement> T build(
      Class<T> type, Class<R> implType) {
    // if element is not nullable - this throws an error
    ElementLocation.ElementFound element = this.elementLocation.find(scope);

    // if nothing is found and element is nullable - return null
    if (element == null) {
      return null;
    }

    return createInstance(implType, element.getFoundElement(), driver);
  }

  /**
   * set parameters in actionable, then find it and apply filter to return first match
   *
   * @param type interface type of the actionable
   * @param implType implementation type of the actionable
   * @param filter to apply to found list
   * @param <T> element type
   * @param <R> implementation type
   * @return instance with parameters set in selector
   */
  public <T extends BasicElement, R extends BasePageElement> T build(
      Class<T> type, Class<R> implType, Predicate<T> filter) {
    // if element is not nullable - this throws an error
    List<T> list = buildList(type, implType);

    if (list == null) {
      return null;
    }
    for (T t : list) {
      if (filter.test(t)) {
        return t;
      }
    }
    // if no match found and element is nullable - return null
    if (elementLocation.findContext.isNullable()) {
      return null;
    }
    // if none found that match condition - throw
    throw new NullPointerException(getFilteredElementNotFoundErr(type));
  }

  /**
   * set parameters in actionable, then find all elements and return list
   *
   * @param type interface type of the actionable
   * @param implType implementation type of the actionable
   * @param <T> element type
   * @param <R> implementation type
   * @return list of instances with index in selector
   */
  public <T extends BasicElement, R extends BasePageElement> List<T> buildList(
      Class<T> type, Class<R> implType) {
    // if element is not nullable - this throws an error
    List<ElementLocation.ElementFound> elementsFound = elementLocation.findList(scope);

    // if nothing is found and element is nullable - return null
    if (elementsFound == null || elementsFound.isEmpty()) {
      return null;
    }

    return elementsFound.stream()
        .map(el -> (T) createInstance(implType, el.getFoundElement(), driver))
        .collect(Collectors.toList());
  }

  /**
   * set parameters in actionable, then find it and apply filter to return all elements that match
   *
   * @param type type of the actionable
   * @param implType implementation type of the actionable
   * @param filter to apply to found list
   * @param <T> element type
   * @param <R> implementation type
   * @return instance with parameters set in selector
   */
  public <T extends BasicElement, R extends BasePageElement> List<T> buildList(
      Class<T> type, Class<R> implType, Predicate<T> filter) {
    // if element is not nullable - this throws an error
    List<T> list = buildList(type, implType);

    // if nothing is found and element is nullable - return null
    if (list == null || list.isEmpty()) {
      return null;
    }

    return list.stream().filter(filter).collect(Collectors.toList());
  }

  /**
   * same as build(FrameElement.class, FrameElementImpl.class)
   *
   * @return frame element instance
   */
  public FrameElement buildFrame() {
    return build(FrameElement.class, FrameElementImpl.class);
  }
}
