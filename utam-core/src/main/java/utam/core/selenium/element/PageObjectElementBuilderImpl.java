/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.selenium.element;

import utam.core.framework.base.PageObjectElementBuilder;
import utam.core.framework.base.PageObjectsFactory;
import utam.core.framework.consumer.ContainerElement;
import utam.core.framework.consumer.UtamError;
import org.openqa.selenium.WebElement;
import utam.core.selenium.context.SeleniumContext;
import utam.core.selenium.context.WebDriverUtilities;
import utam.core.selenium.expectations.ElementListExpectations;

import java.util.List;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

/**
 * builder class is used by Page Object to apply element parameters or make it a list <br>
 * moved to a separate class to remove such API from Actionable
 *
 * @author elizaveta.ivanova
 * @since 232
 */
public class PageObjectElementBuilderImpl implements PageObjectElementBuilder {

  private final SeleniumContext seleniumContext;
  // can be null if used to build Container element
  private final BaseElement self;
  private final boolean isNullable;

  public PageObjectElementBuilderImpl(PageObjectsFactory pageObjectsFactory, BaseElement element, boolean isNullable) {
    this.seleniumContext = pageObjectsFactory.getSeleniumContext();
    this.self = element;
    this.isNullable = isNullable;
  }

  @SuppressWarnings("unused")
  // constructor for container element
  public PageObjectElementBuilderImpl(PageObjectsFactory pageObjectsFactory) {
    this(pageObjectsFactory, null, false);
  }

  static ElementListExpectations<Integer> countElements(boolean isNullable) {
    return new ElementListExpectations<>() {
      @Override
      public Integer returnIfNothingFound() {
        return isNullable ? 0 : null;
      }

      @Override
      public String getLogMessage() {
        return "get elements count";
      }

      @Override
      public Function<List<WebElement>, Integer> apply(WebDriverUtilities utilities) {
        return List::size;
      }
    };
  }

  private static boolean isParametersEmpty(Object... values) {
    return values == null || values.length == 0;
  }

  /**
   * used from page object to set parameters in container element
   *
   * @param factory page objects factory
   * @param element container self element
   * @param values parameters in selector to set
   * @return instance of the element with parameters set in selector
   */
  public static ContainerElement setContainerParameters(
      PageObjectsFactory factory, ContainerElement element, Object... values) {
    if (isParametersEmpty(values)) {
      return element;
    }
    Locator.Parameters parameters = new LocatorParameters(values);
    Locator locator = ((ElementContainerImpl) element).getLocator();
    return LocatorUtilities.getContainer(
        locator.setParameters(parameters), element.isExpandScopeShadow(), factory);
  }

  @SuppressWarnings("unchecked")
  @Override
  public <T extends Actionable> T build(Class<T> type, Object... values) {
    if (isParametersEmpty(values)) {
      return (T) self;
    }
    Locator.Parameters parameters = new LocatorParameters(values);
    Locator locator = LocatorUtilities.getElementLocator(self);
    return (T) LocatorUtilities.getElement(locator.setParameters(parameters), seleniumContext);
  }

  @Override
  public <T extends Actionable> T build(Class<T> type, Predicate<T> filter, Object... values) {
    List<T> list = buildList(type, values);
    if (list.isEmpty()) {
      if (isNullable) {
        return null;
      }
      throw new UtamError("can't find matching element");
    }
    for (T t : list) {
      if (filter.test(t)) {
        return t;
      }
    }
    throw new UtamError("can't find matching element");
  }

  @SuppressWarnings("unchecked")
  @Override
  public <T extends Actionable> List<T> buildList(Class<T> type, Object... values) {
    Locator locatorWithoutParameters = LocatorUtilities.getElementLocator(this.self);
    Locator locator;
    if (isParametersEmpty(values)) {
      locator = locatorWithoutParameters;
    } else {
      Locator.Parameters parameters = new LocatorParameters(values);
      locator = locatorWithoutParameters.setParameters(parameters);
    }
    int count = count(isNullable, locator);
    return IntStream.range(0, count)
        .mapToObj(
            index -> (T) LocatorUtilities.getElement(locator.setIndex(index), seleniumContext))
        .collect(Collectors.toList());
  }

  @Override
  public <T extends Actionable> List<T> buildList(
      Class<T> type, Predicate<T> filter, Object... values) {
    List<T> list = buildList(type, values);
    if (list.isEmpty()) {
      if (isNullable) {
        return null;
      }
      throw new UtamError("can't find matching element");
    }
    return list.stream().filter(filter).collect(Collectors.toList());
  }

  final int count(boolean isNullable, Locator elementLocator) {
    ElementListExpectations<Integer> expectation = countElements(isNullable);
    return new ElementWaitImpl(expectation.getLogMessage(), elementLocator, seleniumContext)
        .wait(expectation);
  }
}
