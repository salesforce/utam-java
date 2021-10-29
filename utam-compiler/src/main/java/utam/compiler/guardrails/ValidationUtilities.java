/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.guardrails;

import java.util.List;
import utam.compiler.helpers.ElementContext;
import utam.compiler.helpers.ElementContext.Basic;
import utam.compiler.helpers.ElementContext.ElementType;
import utam.compiler.helpers.ElementContext.Root;
import utam.core.declarative.representation.TypeProvider;
import utam.core.element.Locator;

/**
 * utilities for guardrails validations
 *
 * @author elizaveta.ivanova
 * @since 236
 */
abstract class ValidationUtilities {

  private static final List<String> PROHIBITED_SELECTORS = List.of(
      "[value",
      "[title");

  static boolean isSameSelector(Locator underTest, Locator testAgainst) {
    // root can have null as selector
    if (underTest == null || testAgainst == null) {
      return false;
    }
    if (underTest.getStringValue().isEmpty() || testAgainst.getStringValue()
        .isEmpty()) { // for container
      return false;
    }
    return testAgainst.equals(underTest);
  }

  static boolean hasHardcodedText(Locator underTest) {
    // root can have null as selector
    if (underTest == null) {
      return false;
    }
    String selector = underTest.getStringValue();
    if (PROHIBITED_SELECTORS.stream().anyMatch(selector::contains)) {
      return !selector.contains("%");
    }
    return false;
  }

  static ValidationError getValidationError(ElementContext first, ElementContext second) {
    ElementContext.ElementType elementType = first.getElementNodeType();
    if (elementType == ElementType.SELF
        || elementType == ElementType.DOCUMENT
        || elementType == ElementType.CONTAINER) {
      return null;
    }
    if (elementType == ElementType.ROOT) {
      return validateRootElement((ElementContext.Root) first, second);
    }
    if (elementType == ElementType.CUSTOM) {
      return validateCustomElement(first, second);
    }
    return validateBasicElement(first, second);
  }

  private static ValidationError validateCustomElement(ElementContext customElement,
      ElementContext element) {
    ElementContext.ElementType elementType = element.getElementNodeType();
    Locator customElementLocator = customElement.getSelector();
    // this statement should be before next because declared root element is also HTML element
    if (elementType == ElementType.ROOT) {
      if (customElement.getType().isSameType(((Root) element).getEnclosingPageObjectType())) {
        return null;
      }
      if (isSameSelector(customElementLocator, element.getSelector())) {
        return ValidationError.DUPLICATE_WITH_ROOT_SELECTOR;
      }
      return null;
    }
    if (element instanceof Basic && isSameSelector(customElementLocator, element.getSelector())) {
      return ValidationError.COMPONENT_AND_ELEMENT_DUPLICATE_SELECTOR;
    }
    // if selector same but type is different - it's error
    if (elementType == ElementType.CUSTOM
        && !customElement.getType().isSameType(element.getType())
        && isSameSelector(customElementLocator, element.getSelector())) {
      return ValidationError.COMPONENTS_WITH_SAME_SELECTOR_BUT_DIFFERENT_TYPES;
    }
    return null;
  }

  private static ValidationError validateBasicElement(ElementContext basicElement, ElementContext element) {
    ElementContext.ElementType elementType = element.getElementNodeType();
    Locator basicElementLocator = basicElement.getSelector();
    if (elementType == ElementType.ROOT && isSameSelector(basicElementLocator, element.getSelector())) {
      return ValidationError.DUPLICATE_WITH_ROOT_SELECTOR;
    }
    if (elementType == ElementType.CUSTOM && isSameSelector(basicElementLocator, element.getSelector())) {
      return ValidationError.COMPONENT_AND_ELEMENT_DUPLICATE_SELECTOR;
    }
    return null;
  }

  private static boolean isSameEnclosingType(TypeProvider enclosingPageObjectType,
      ElementContext element) {
    ElementContext.ElementType elementType = element.getElementNodeType();
    if (elementType == ElementType.CUSTOM && element.getType().isSameType(enclosingPageObjectType)) {
      return true;
    }
    if (elementType == ElementType.ROOT) {
      return enclosingPageObjectType.isSameType(((Root) element).getEnclosingPageObjectType());
    }
    return false;
  }

  private static ValidationError validateRootElement(ElementContext.Root rootElement,
      ElementContext element) {
    TypeProvider enclosingPageObjectType = rootElement.getEnclosingPageObjectType();
    Locator rootElementLocator = rootElement.getSelector();
    if (isSameEnclosingType(enclosingPageObjectType, element)) {
      return null;
    }
    if (isSameSelector(rootElementLocator, element.getSelector())) {
      return ValidationError.DUPLICATE_WITH_ROOT_SELECTOR;
    }
    return null;
  }
}
