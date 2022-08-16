/*
 * Copyright (c) 2022, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.helpers;

import com.fasterxml.jackson.databind.JsonNode;
import utam.compiler.UtamCompilationError;

/**
 * Validation utilities
 *
 * @author elizaveta.ivanova
 * @since 242
 */
public class ValidationUtilities {

  private final TranslationContext context;

  public ValidationUtilities(TranslationContext context) {
    this.context = context;
  }

  public void validateNotNullOrEmptyString(JsonNode node, String value, String prefix, String propertyName) {
    if(value == null || value.isEmpty()) {
      throw new UtamCompilationError(node, context.getErrorMessage(10, prefix, propertyName));
    }
  }

  public void validateNullOrNotEmptyString(JsonNode node, String value, String prefix, String propertyName) {
    if(value != null && value.isEmpty()) {
      throw new UtamCompilationError(node, context.getErrorMessage(10, prefix, propertyName));
    }
  }
}
