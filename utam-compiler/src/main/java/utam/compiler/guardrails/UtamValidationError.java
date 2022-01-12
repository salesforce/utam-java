/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.guardrails;

import utam.core.framework.consumer.UtamError;

/**
 * Error thrown in runtime from UTAM guardrails
 *
 * @author elizaveta.ivanova
 * @since 234
 */
public class UtamValidationError extends UtamError {

  /**
   * Default serial version ID for serializable object
   */
  private static final long serialVersionUID = 1L;

  /**
   * Initializes a new instance of the UtamValidationError class
   *
   * @param message the validation error message
   */
  public UtamValidationError(String message) {
    super(message);
  }
}
