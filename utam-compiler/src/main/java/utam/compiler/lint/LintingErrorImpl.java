/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.lint;

import static utam.compiler.diagnostics.ValidationUtilities.VALIDATION;

import utam.compiler.lint.LintingRule.ViolationType;
import utam.core.declarative.lint.LintingError;

/**
 * Linting error implementation
 *
 * @author elizaveta.ivanova
 * @since 242
 */
class LintingErrorImpl implements LintingError {

  private final String errorMessage;
  private final ViolationType ruleType;
  private final boolean isReport;

  LintingErrorImpl(ViolationType ruleType, boolean isReport, Integer errorCode, String... args) {
    this.ruleType = ruleType;
    String prefix = isReport? ruleType.name() : "disabled";
    this.errorMessage = String.format("linting %s %d: %s", prefix, errorCode,
        VALIDATION.getLintingMessage(errorCode, args));
    this.isReport = isReport;
  }

  @Override
  public String getMessage() {
    return errorMessage;
  }

  @Override
  public boolean isThrowError() {
    return isReport() && ruleType == ViolationType.error;
  }

  @Override
  public boolean isReport() {
    return isReport;
  }
}
