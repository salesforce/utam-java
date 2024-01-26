/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.lint;

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
  private final String fixSuggesion;
  private final int sourceLine;

  LintingErrorImpl(
      String ruleId,
      ViolationLevel level,
      String fixSuggestion,
      PageObjectLinting pageObject,
      int sourceLine,
      Integer errorCode,
      String errorMessage) {
    this.ruleId = ruleId;
    this.level = level;
    this.id = String.valueOf(errorCode);
    this.errorMessage = errorMessage;
    this.pageObjectPath = pageObject.getJsonFilePath();
    this.fullErrorMessage =
        String.format(
            "lint rule %s failure in page object %s: %s; %s",
            ruleId, pageObject.getName(), this.errorMessage, fixSuggestion);
    this.fixSuggesion = fixSuggestion;
    this.sourceLine = sourceLine;
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

  @Override
  public String getFixSuggestion() {
    return fixSuggesion;
  }

  @Override
  public int getSourceLine() {
    return sourceLine;
  }
}
