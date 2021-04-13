/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.selenium.element;

import utam.core.framework.consumer.UtamError;

import org.openqa.selenium.*;

import utam.core.selenium.context.WebDriverUtilities;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import static utam.core.selenium.element.LocatorNodeImpl.ERR_ELEMENT_SELECTED_BY;
import static utam.core.selenium.element.ShadowRootWebElement.getElementsWithFirefoxWorkaround;

/**
 * base class for locator
 *
 * @author elizaveta.ivanova
 * @since 224
 */
class LocatorImpl implements Locator {

  static final String JAVASCRIPT_STRING_NOT_SUPPORTED =
      "building javascript is not supported for chained locators policy";
  static final String LOCATOR_CHAIN_SEPARATOR = "|";
  private static final String LOCATOR_CHAIN_SHADOW_BOUNDARY_SEPARATOR = "|*|";
  private static final String ERR_CANT_FIND_ELEMENTS = "can't find elements";
  private static final List<WebElement> NOTHING_FOUND = new ArrayList<>();
  final LocatorNodeImpl rootElement;

  LocatorImpl(LocatorNode rootElement) {
    this.rootElement = (LocatorNodeImpl) rootElement;
  }

  // used in tests
  LocatorImpl(LocatorNode... elements) {
    this.rootElement = (LocatorNodeImpl) elements[0];
    LocatorNodeImpl current = rootElement;
    for (int i = 1; i < elements.length; i++) {
      current.setNext(elements[i]);
      current = (LocatorNodeImpl) elements[i];
    }
  }

  @Override
  public final LocatorImpl setIndex(int index) {
    if (index == 0) {
      return this;
    }
    LocatorImpl clone = getCopy();
    clone.getLeaf().setIndex(index);
    return clone;
  }

  @Override
  public final LocatorImpl add(LocatorNode leaf) {
    LocatorImpl copy = getCopy();
    copy.getLeaf().setNext(((LocatorNodeImpl) leaf).getCopy());
    return copy;
  }

  @Override
  public final LocatorImpl scope(Locator next) {
    LocatorImpl copy = getCopy();
    LocatorNodeImpl copyLeaf = copy.getLeaf();
    LocatorNode currentScoped = ((LocatorImpl) next).getRoot();
    while (currentScoped != null) {
      copyLeaf.setNext(((LocatorNodeImpl) currentScoped).getCopy());
      currentScoped = currentScoped.getNext();
    }
    return copy;
  }

  final LocatorNodeImpl getRoot() {
    return rootElement;
  }

  final LocatorNodeImpl getLeaf() {
    LocatorNode current = getRoot();
    while (current.getNext() != null) {
      current = current.getNext();
    }
    return (LocatorNodeImpl) current;
  }

  // overridden in JS locator class
  String getSelectorString() {
    StringBuilder res = new StringBuilder();
    LocatorNodeImpl current = this.getRoot();
    while (current != null) {
      if (res.length() > 2) {
        String separator =
            current.getScopeTransformer() == ShadowBoundary.EXPAND_SHADOW_ROOT
                ? LOCATOR_CHAIN_SHADOW_BOUNDARY_SEPARATOR
                : LOCATOR_CHAIN_SEPARATOR;
        res.append(separator);
      }
      res.append(current.getSelectorString());
      res.append(current.getFilter().getFilterString());
      current = (LocatorNodeImpl) current.getNext();
    }
    return res.toString().trim();
  }

  final String getErrorPrefix() {
    return String.format(ERR_ELEMENT_SELECTED_BY, getSelectorString());
  }

  // overridden in JS locator class
  List<WebElement> find(WebDriverUtilities utilities, LocatorUtilities.Find find) {
    SearchContext context = utilities.getWebDriver();
    LocatorNodeImpl currentScope = null;
    LocatorNodeImpl current = getRoot();
    // for intermittent elements find type is always same
    LocatorUtilities.Find howToFind = find.getFindIntermittentElementType();
    while (current != null && current.getNext() != null) {
      List<WebElement> found = current.findElements(context, currentScope, utilities, howToFind);
      found = current.applyFilter(found, howToFind);
      // exception is already thrown by now if nothing is found
      if (found.isEmpty()) {
        return NOTHING_FOUND;
      }
      context = found.iterator().next();
      currentScope = current;
      current = (LocatorNodeImpl) current.getNext();
    }
    // for last element find type is always as requested from caller
    List<WebElement> found = current.findElements(context, currentScope, utilities, find);
    return current.applyFilter(found, find);
  }

  // overridden in JS locator class
  LocatorImpl getSelfCopy() {
    return new LocatorImpl(rootElement.getCopy());
  }

  final LocatorImpl getCopy() {
    LocatorImpl copy = getSelfCopy();
    LocatorNodeImpl currentTarget = copy.getRoot();
    LocatorNodeImpl currentSource = rootElement;
    while (currentSource.getNext() != null) {
      currentSource = (LocatorNodeImpl) currentSource.getNext();
      LocatorNodeImpl element = currentSource.getCopy();
      currentTarget.setNext(element);
      currentTarget = element;
    }
    return copy;
  }

  @Override
  public final boolean equals(Object obj) {
    if (!(obj instanceof LocatorImpl)) {
      return false;
    }
    return ((LocatorImpl) obj).getSelectorString().equals(getSelectorString());
  }

  @Override public int hashCode() {
    return Objects.hash(getSelectorString());
  }

  @Override
  public final Locator setParameters(Locator.Parameters parameters) {
    if (parameters == null || parameters.isEmpty()) {
      return this;
    }
    ((LocatorParameters) parameters).start();
    LocatorImpl copy = getCopy();
    LocatorNode current = copy.getRoot();
    while (current != null) {
      current.setParameters(parameters);
      current = current.getNext();
    }
    ((LocatorParameters) parameters).end(this);
    return copy;
  }

  // overridden in JS locator class
  String getJavascriptForConsole(LocatorUtilities.Find find) {
    throw new UtamError(JAVASCRIPT_STRING_NOT_SUPPORTED);
  }

  static final class Javascript extends LocatorImpl {

    Javascript(LocatorNode rootElement) {
      super(rootElement);
    }

    // used in tests
    Javascript(LocatorNode... elements) {
      super(elements);
    }

    @Override
    final LocatorImpl getSelfCopy() {
      return new LocatorImpl.Javascript(rootElement.getCopy());
    }

    @Override
    List<WebElement> find(WebDriverUtilities utilities, LocatorUtilities.Find find) {
      LocatorUtilities.Find findFirst =
          getRoot().getNext() == null ? find : LocatorUtilities.Find.FILTERED_LIST;
      // first step is to find root element, can't use javascript here
      List<WebElement> found =
          getRoot().findElements(utilities.getWebDriver(), null, utilities, findFirst);
      found = getRoot().applyFilter(found, findFirst);
      // exception is already thrown by now if nothing is found
      if (getRoot().getNext() == null || found.isEmpty()) {
        return found;
      }
      StringBuilder builder = new StringBuilder("arguments[0]");
      // first element should be skipped - we found it already
      ((LocatorNodeImpl.Javascript) getRoot().getNext()).setJavascriptBuilderAll(builder, find);
      String javascriptString = "return " + builder.toString();
      try {
        found = getElementsWithFirefoxWorkaround(
                utilities.getExecutor().executeScript(javascriptString, found.iterator().next()));
      } catch (WebDriverException executorError) {
        throw new NotFoundException(getErrorPrefix() + ERR_CANT_FIND_ELEMENTS, executorError);
      }
      if (found.isEmpty() && !find.isCanBeEmpty()) {
        throw new NotFoundException(getErrorPrefix() + ERR_CANT_FIND_ELEMENTS);
      }
      return getLeaf().applyFilter(found, find);
    }

    @Override
    String getJavascriptForConsole(LocatorUtilities.Find find) {
      StringBuilder builder = new StringBuilder();
      LocatorUtilities.Find findFirst = getRoot().getNext() == null ? find : LocatorUtilities.Find.FILTERED_ELEMENT;
      ((LocatorNodeImpl.Javascript) getRoot())
          .setJavascriptBuilder(builder, findFirst, LocatorUtilities.QueryType.ROOT_JQUERY);
      if (getRoot().getNext() != null) {
        ((LocatorNodeImpl.Javascript) getRoot().getNext()).setJavascriptBuilderAll(builder, find);
      }
      return builder.toString();
    }

    @Override
    String getSelectorString() {
      StringBuilder builder = new StringBuilder("arguments[0]");
      LocatorUtilities.Find findFirst =
          getRoot().getNext() == null
              ? LocatorUtilities.Find.FILTERED_LIST
              : LocatorUtilities.Find.FILTERED_ELEMENT;
      ((LocatorNodeImpl.Javascript) getRoot())
          .setJavascriptBuilder(builder, findFirst, LocatorUtilities.QueryType.ROOT_JS);
      if (getRoot().getNext() != null) {
        ((LocatorNodeImpl.Javascript) getRoot().getNext())
            .setJavascriptBuilderAll(builder, LocatorUtilities.Find.FILTERED_LIST);
      }
      return "return " + builder.toString();
    }
  }
}
