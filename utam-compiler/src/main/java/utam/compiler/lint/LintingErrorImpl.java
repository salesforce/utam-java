/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.lint;

import static utam.compiler.diagnostics.ValidationUtilities.VALIDATION;

import utam.core.declarative.lint.LintingError;
import utam.core.declarative.lint.PageObjectLinting;

/**
 * Linting error implementation
 *
 * @author elizaveta.ivanova
 * @since 242
 */
class LintingErrorImpl implements LintingError {

  private final String errorMessage;
  private final String fullErrorMessage;
  private final String id;
  private final String ruleId;
  private final ViolationLevel level;
  private final String pageObjectPath;

  LintingErrorImpl(String ruleId, ViolationLevel level, PageObjectLinting pageObject,
      Integer errorCode,
      String... args) {
    this.ruleId = ruleId;
    this.level = level;
    this.id = String.valueOf(errorCode);
    this.errorMessage = VALIDATION.getLintingMessage(errorCode, args);
    this.pageObjectPath = pageObject.getJsonFilePath();
    this.fullErrorMessage = buildFullErrorMessage(pageObject.getName(), this, this.errorMessage);
  }

  /**
   * Build full error message that includes ruleId and page object name to print in UTAM logs. Not
   * private because used in the unit tests.
   *
   * @param pageObjectName URI of the page page object
   * @param error          linting error object
   * @param shortMessage   short message used by SARIF
   * @return string
   */
  static String buildFullErrorMessage(String pageObjectName, LintingError error,
      String shortMessage) {
    LintingErrorImpl err = (LintingErrorImpl) error;
    return String.format("lint %s %s in page object %s: %s", err.ruleId,
        err.level.name(), pageObjectName, shortMessage);
  }

  @Override
  public String getMessage() {
    return errorMessage;
  }

  @Override
  public String getFullMessage() {
    return fullErrorMessage;
  }

  @Override
  public ViolationLevel getLevel() {
    return level;
  }

  @Override
  public String getId() {
    return id;
  }

  @Override
  public String getRuleId() {
    return ruleId;
  }

  @Override
  public String getSourceFilePath() {
    return pageObjectPath;
  }
}
