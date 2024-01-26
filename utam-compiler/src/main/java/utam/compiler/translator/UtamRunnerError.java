/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.translator;

import utam.core.framework.consumer.UtamError;

/**
 * Error thrown in runtime from runner, not compilation error in page object
 *
 * @author elizaveta.ivanova
 * @since 240
 */
class UtamRunnerError extends UtamError {

  /** Default serial version ID for serializable object */
  private static final long serialVersionUID = 1L;

  /**
   * Initializes a new instance
   *
   * @param message the message of the error
   */
  UtamRunnerError(String message) {
    super(message);
  }

  /**
   * Initializes a new instance
   *
   * @param message the message of the error
   * @param e the inner exception wrapped by the error
   */
  UtamRunnerError(String message, Throwable e) {
    super(message, e);
  }
}
