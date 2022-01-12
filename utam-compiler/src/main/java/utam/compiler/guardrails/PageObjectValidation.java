/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.guardrails;

import static utam.compiler.guardrails.ValidationExclusions.isViolationAllowed;
import static utam.compiler.guardrails.ValidationUtilities.getValidationError;
import static utam.compiler.guardrails.ValidationUtilities.hasHardcodedText;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import utam.compiler.helpers.ElementContext;
import utam.core.declarative.translator.GuardrailsMode;
import utam.core.framework.UtamLogger;

/**
 * Guardrails that validate elements inside one Page Objects
 *
 * @author elizaveta.ivanova
 * @since 226
 */
public class PageObjectValidation {

  private static final String ERR_SELECTOR_HARDCODED = "Validation failure for Page Object '%s', "
      + "element '%s': selector '%s' contains hardcoded text";
  private static final String ERR_FORMAT_LOCAL = "Validation failure for elements '%s' and '%s' in the Page Object '%s': ";
  private final String pageObjectURI;
  private final Collection<ElementContext> elements;
  private final GuardrailsMode guardrailsMode;

  /**
   * Initializes a new instance of the PageObjectValidation class
   *
   * @param guardrailsMode the guard rails mode to use for validation
   * @param pageObjectURI  the Page object URI
   * @param elements       the elements to validate
   */
  public PageObjectValidation(GuardrailsMode guardrailsMode, String pageObjectURI,
      Collection<ElementContext> elements) {
    this.pageObjectURI = pageObjectURI;
    this.elements = elements;
    this.guardrailsMode = guardrailsMode;
  }

  String getErrorPrefix(ElementContext firstElement, ElementContext secondElement) {
    return String.format(ERR_FORMAT_LOCAL, firstElement.getName(), secondElement.getName(), pageObjectURI);
  }

  void validateSelector(ElementContext elementContext) {
    if (!hasHardcodedText(elementContext.getSelector())) {
      return;
    }
    String errorMessage = getHardcodedTextInSelectorError(elementContext);
    if (guardrailsMode.isInterruptWithError()) {
      throw new UtamValidationError(errorMessage);
    } else {
      UtamLogger.warning(errorMessage);
    }
  }

  String getHardcodedTextInSelectorError(ElementContext elementContext) {
    return String
        .format(ERR_SELECTOR_HARDCODED, pageObjectURI, elementContext.getName(),
            elementContext.getSelector().getStringValue());
  }

  /**
   * validate all elements inside a page object
   */
  public void validate() {
    List<ElementContext> elements = new ArrayList<>(this.elements);
    for (int i = 0; i < elements.size(); i++) {
      ElementContext element = elements.get(i);
      // first validate selector
      validateSelector(element);
      // then iterate through remaining elements
      for (int j = i + 1; j < elements.size(); j++) {
        ElementContext testAgainst = elements.get(j);
        ValidationError errType = getValidationError(element, testAgainst);
        if (errType == null) {
          return;
        }
        String errorMessage = getErrorPrefix(element, testAgainst) + errType.getErrMessage();
        // for exception or in a warning mode - print warning in console
        if (isViolationAllowed(pageObjectURI, element.getName(), errType)
            || !guardrailsMode.isInterruptWithError()) {
          UtamLogger.warning(errorMessage);
        } else {
          throw new UtamValidationError(errorMessage);
        }
      }
    }
  }
}
