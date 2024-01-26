/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.consumer;

/**
 * TEMPORARY LOCATION TO BE REFACTORED
 *
 * @author elizaveta.ivanova
 * @since 228
 */
public class UtamError extends RuntimeException {

  /** Default serial version ID for serializable object */
  private static final long serialVersionUID = 1L;

  /**
   * Initializes a new instance of the UtamError class
   *
   * @param message the error message
   */
  public UtamError(String message) {
    super(message);
  }

  /**
   * Initializes a new instance of the UtamError class
   *
   * @param message the error message
   * @param cause the underlying wrapped exception
   */
  public UtamError(String message, Throwable cause) {
    super(message, cause);
  }

  /**
   * Initializes a new instance of the UtamError class
   *
   * @param e the underlying wrapped exception
   */
  public UtamError(Exception e) {
    super(e);
  }
}
