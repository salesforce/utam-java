/*
 * Copyright (c) 2022, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.declarative.lint;

/**
 * An error created by linting process
 *
 * @author elizaveta.ivanova
 * @since 242
 */
public interface LintingError {

  /**
   * Get unique error ID for SARIF JSON
   *
   * @return error id
   */
  String getId();

  /**
   * Get rule Id that was violated
   *
   * @return string with rule Id
   */
  String getRuleId();

  /**
   * Get an error message for SARIF JSON
   *
   * @return preconfigured via errors config short message
   */
  String getMessage();

  /**
   * Full message includes information about error code and page object name to print to console log
   *
   * @return string
   */
  String getFullMessage();

  /**
   * Get violation level, error or warning
   *
   * @return violation type
   */
  ViolationLevel getLevel();

  /**
   * Full JSON source file path for SARIF JSON
   *
   * @return string
   */
  String getSourceFilePath();

  /**
   * Suggestion how to fix the error
   *
   * @return string
   */
  String getFixSuggestion();

  int getSourceLine();

  /**
   * Type of the linting violation
   *
   * @author elizaveta.ivanova
   * @since 242
   */
  enum ViolationLevel {
    // lower case for deserialization
    error,
    warning,
    disabled
  }
}
