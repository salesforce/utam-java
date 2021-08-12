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

  warning,
  error;

  public boolean isInterruptWithError() {
    return this == error;
  }
}
