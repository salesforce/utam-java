/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.lint;

import utam.core.framework.consumer.UtamError;

/**
 * Error thrown by linting
 *
 * @author elizaveta.ivanova
 * @since 242
 */
class UtamLintingError extends UtamError {

  /** Default serial version ID for serializable object */
  private static final long serialVersionUID = 1L;

  /**
   * Initializes a new instance of the UtamValidationError class
   *
   * @param message the validation error message
   */
  UtamLintingError(String message) {
    super(message);
  }

  UtamLintingError(String message, Throwable t) {
    super(message, t);
  }
}
