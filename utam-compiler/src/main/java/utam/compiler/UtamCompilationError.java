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

  public UtamCompilationError(String message) {
    super(message);
  }

  public UtamCompilationError(String message, Exception e) {
    super(message, e);
  }

  public UtamCompilationError(Exception e) {
    super(e);
  }
}
