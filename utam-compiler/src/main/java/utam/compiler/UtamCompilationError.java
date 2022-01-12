/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler;

import utam.core.framework.consumer.UtamError;

/**
 * Error thrown in runtime from UTAM compiler
 *
 * @author elizaveta.ivanova
 * @since 234
 */
public class UtamCompilationError extends UtamError {

  /**
   * Default serial version ID for serializable object
   */
  private static final long serialVersionUID = 1L;

  /**
   * Initializes a new instance of the UtamCompilationError class
   *
   * @param message the message of the error
   */
  public UtamCompilationError(String message) {
    super(message);
  }

  /**
   * Initializes a new instance of the UtamCompilationError class
   *
   * @param message the message of the error
   * @param e the inner exception wrapped by the error
   */
  public UtamCompilationError(String message, Exception e) {
    super(message, e);
  }

  /**
   * Initializes a new instance of the UtamCompilationError class
   *
   * @param e the inner exception wrapped by the error
   */
  public UtamCompilationError(Exception e) {
    super(e);
  }
}
