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

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import utam.compiler.helpers.ElementContext;
import utam.core.declarative.translator.GuardrailsMode;
import utam.core.framework.UtamLogger;

/**
 * Static guardrails that validate all Page Objects known to a runner
 *
 * @author elizaveta.ivanova
 * @since 236
 */
public class GlobalValidation {

  private static final String ERR_FORMAT_GLOBAL =
      "Validation failure for element '%s' in Page Object '%s' and element '%s' in Page Object '%s': ";
  private final Map<String, Collection<ElementContext>> allElementsAccumulated = Collections
      .synchronizedMap(new HashMap<>());
  private final GuardrailsMode guardrailsMode;

  /**
   * Initializes a new instance of the GlobalValidation class
   *
   * @param guardrailsMode the mode to use for the guardrails
   */
  public GlobalValidation(GuardrailsMode guardrailsMode) {
    this.guardrailsMode = guardrailsMode;
  }

  static String getErrorPrefix(ValidationSubject first, ValidationSubject second) {
    return String.format(ERR_FORMAT_GLOBAL,
        first.getElementName(),
        first.pageObjectName,
        second.getElementName(),
        second.pageObjectName);
  }

  /**
   * Adds the page object elements to the validation list
   * @param pageObjectURI the URI of the Page Object
   * @param elements      the list of elements to add
   */
  public void setPageObjectElements(String pageObjectURI, Collection<ElementContext> elements) {
    this.allElementsAccumulated.put(pageObjectURI, elements);
  }

  /**
   * iterate through all Page Objects and all their elements
   */
  public void validate() {
    for (String firstPageObject : allElementsAccumulated.keySet()) {
      for (ElementContext firstElementContext : allElementsAccumulated.get(firstPageObject)) {
        ValidationSubject first = new ValidationSubject(firstPageObject, firstElementContext);
        for (String secondPageObject : allElementsAccumulated.keySet()) {
          if (secondPageObject.equals(firstPageObject)) {
            continue; // we already tested inside same PO
          }
          for (ElementContext secondElementContext : allElementsAccumulated.get(secondPageObject)) {
            ValidationSubject second = new ValidationSubject(secondPageObject, secondElementContext);
            checkViolations(first, second);
          }
        }
      }
    }
  }

  private void checkViolations(ValidationSubject first, ValidationSubject second) {
    ValidationError errType = getValidationError(first.elementContext, second.elementContext);
    if (errType == null) {
      return;
    }
    String errorMessage = getErrorPrefix(first, second) + errType.getErrMessage();
    // depending on order validation, error is thrown in first PO that violated
    // so we have to check for exclusion for both underTest and testAgainst
    if (isViolationAllowed(first.pageObjectName, first.getElementName(), errType)
        || isViolationAllowed(second.pageObjectName, second.getElementName(), errType)
        || !guardrailsMode.isInterruptWithError()) {
      UtamLogger.warning(errorMessage);
    } else {
      throw new UtamValidationError(errorMessage);
    }
  }

  static class ValidationSubject {

    private final String pageObjectName;
    private final ElementContext elementContext;

    ValidationSubject(String pageObjectName, ElementContext elementContext) {
      this.pageObjectName = pageObjectName;
      this.elementContext = elementContext;
    }

    String getElementName() {
      return elementContext.getName();
    }
  }
}
