/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework;

import utam.core.framework.consumer.UtamError;

/**
 * Exception thrown in runtime from UTAM core library
 *
 * @author elizaveta.ivanova
 * @since 234
 */
public class UtamCoreError extends UtamError {

  /**
   * Default serial version ID for serializable object
   */
  private static final long serialVersionUID = 1L;

  public UtamCoreError(String message) {
    super(message);
  }

  public UtamCoreError(String message, Exception e) {
    super(message, e);
  }
}
