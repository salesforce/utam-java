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

  /**
   * Default serial version ID for serializable object
   */
  private static final long serialVersionUID = 1L;

  public UtamError(String message) {
    super(message);
  }

  public UtamError(String message, Throwable cause) {
    super(message, cause);
  }

  public UtamError(Exception e) {
    super(e);
  }
}
