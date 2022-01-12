/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.declarative.translator;

/**
 * type of the Guardrails validations configured for the runner
 *
 * @author elizaveta.ivanova
 * @since 236
 */
public enum GuardrailsMode {

  /**
   * Guardrails violations emit a compiler warning
   */
  WARNING,

  /**
   * Guardrails violations emit a compiler error
   */
  ERROR;

  /**
   * Gets a value indicating whether to interrupt on an error
   *
   * @return true if the compiler should interrupt with an error; otherwise, false
   */
  public boolean isInterruptWithError() {
    return this == ERROR;
  }
}
