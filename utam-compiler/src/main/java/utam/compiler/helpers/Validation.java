/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.helpers;

import utam.core.framework.consumer.UtamError;
import utam.core.element.Locator;

import java.util.*;

import static utam.compiler.helpers.ValidationExclusions.isExceptionAllowed;

/**
 * static guardrails
 *
 * @author elizaveta.ivanova
 * @since 226
 */
class Validation {

  static final String ERR_FORMAT_LOCAL = "for elements '%s' and '%s' in Page Object '%s': ";
  static final String ERR_FORMAT_GLOBAL =
      "for element '%s' in Page Object '%s' and element '%s' in Page Object '%s': ";
  private static final String ERR_PREFIX = "Validation failure ";
  private static final String ERR_FORMAT_LABEL_HARDCODED = "on '%s' Page Object, selector for '%s' ";
  private static final Map<String, Collection<ElementContext>> global = Collections.synchronizedMap(new HashMap<>());
  private static final List<String> SELECTORS_TO_MATCH = List.of(
          "[value",
          "[title");

  private final String pageObjectURI;
  private final Map<String, ElementContext> elementContextMap;

  Validation(String pageObjectURI, Map<String, ElementContext> elementContextMap) {
    this.pageObjectURI = pageObjectURI;
    this.elementContextMap = elementContextMap;
  }

  static boolean isSameSelector(Locator underTest, Locator testAgainst) {
    if (underTest == null || testAgainst == null) {
      // root can have null as selector
      return false;
    }
    if (underTest.getStringValue().isEmpty() || testAgainst.getStringValue().isEmpty()) { // for container
      return false;
    }
    return testAgainst.equals(underTest);
  }

  static boolean isLabelHardcoded(Locator underTest) {
    if(underTest == null) {
      return false;
    }
    String selector = underTest.getStringValue();
    if (SELECTORS_TO_MATCH.stream().anyMatch(selector::contains)) {
      return !selector.contains("%");
    }
    return false;
  }

  // it is a separate method to be able to test
  static synchronized void testGlobalDuplicates(
          String pageObjectURI,
          ElementContext element,
          Map<String, Collection<ElementContext>> allElements) {
    for (String pageObjectName : allElements.keySet()) {
      if (pageObjectName.equals(pageObjectURI)) {
        continue; // we already tested local duplicates
      }
      for (ElementContext testAgainst : allElements.get(pageObjectName)) {
        ErrorType errType = element.validate(testAgainst);
        if (!errType.isError()) {
          continue;
        }
        // depending on order validation, error is thrown in first PO that violated
        // so we have to check for exclusion for both underTest and testAgainst
        if (isExceptionAllowed(pageObjectURI, element.getName(), errType)
                || isExceptionAllowed(pageObjectName, testAgainst.getName(), errType)) {
          continue;
        }
        if (errType.isLabelHardcodedError()) {
          throw new UtamError(errType.getValidationError(pageObjectURI, element.getName()));
        }
        throw new UtamError(errType.getValidationError(element.getName(), pageObjectURI, testAgainst.getName(), pageObjectName));
      }
    }
    // if this PO was never tested before - add container for its elements to map
    if (!allElements.containsKey(pageObjectURI)) {
      allElements.put(pageObjectURI, Collections.synchronizedList(new ArrayList<>()));
    }
    allElements.get(pageObjectURI).add(element);
  }

  /**
   * scan elements of same PO
   *
   * @param element element to be added
   */
  void testLocalDuplicates(ElementContext element) {
    elementContextMap.forEach(
        (key, value) -> {
          ErrorType err = element.validate(value);
          if (err.isLabelHardcodedError()) {
            throw new UtamError(err.getValidationError(pageObjectURI, element.getName()));
          }
          if (err.isError()) {
            throw new UtamError(err.getValidationError(pageObjectURI, element.getName(), key));
          }
        });
  }

  synchronized void testGlobalDuplicates(ElementContext element) {
    testGlobalDuplicates(pageObjectURI, element, global);
  }

  enum ErrorType {
    COMPONENTS_WITH_SAME_SELECTOR_BUT_DIFFERENT_TYPES(
        "components have same selector but different types"),
    COMPONENT_AND_ELEMENT_DUPLICATE_SELECTOR("component and element can't have same selector"),
    DUPLICATE_WITH_ROOT_SELECTOR("selector is duplicate of the root selector"),
    LABEL_HARDCODED("contains hardcoded label"),
    NONE("");

    final String err;

    ErrorType(String err) {
      this.err = err;
    }

    boolean isError() {
      return this != NONE;
    }

    boolean isLabelHardcodedError() {
      return this.equals(LABEL_HARDCODED);
    }

    String getValidationError(
        String firstElement,
        String firstPageObject,
        String secondElement,
        String secondPageObject) {
      return ERR_PREFIX
          + String.format(
              ERR_FORMAT_GLOBAL, firstElement, firstPageObject, secondElement, secondPageObject)
          + err;
    }

    String getValidationError(String pageObject, String firstElement, String secondElement) {
      return ERR_PREFIX
          + String.format(ERR_FORMAT_LOCAL, firstElement, secondElement, pageObject)
          + err;
    }

    String getValidationError(String pageObject, String element) {
      return ERR_PREFIX
              + String.format(ERR_FORMAT_LABEL_HARDCODED, pageObject, element)
              + err;
    }
  }
}
