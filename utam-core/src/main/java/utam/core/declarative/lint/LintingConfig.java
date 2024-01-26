/*
 * Copyright (c) 2022, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.declarative.lint;

/**
 * Configuration for linting
 *
 * @author elizaveta.ivanova
 * @since 242
 */
public interface LintingConfig {

  /**
   * Start linting process by creating its context
   *
   * @return new linting context
   */
  LintingContext start();

  /**
   * Run linting to collect errors for a particular page object
   *
   * @param pageObjectLintingContext particular page object context
   */
  void lint(LintingContext context, PageObjectLinting pageObjectLintingContext);

  /**
   * Run linting to collect errors across all page objects. Executed after applying linting for
   * every page object and then reports results to SARIF and UTAM logs
   *
   * @param context linting context
   */
  void finish(LintingContext context);

  /**
   * Run linting to collect errors across all page objects. Executed after applying linting for
   * every page object and then reports results to SARIF and UTAM logs
   *
   * @param context linting context
   * @param pathToReport directory where to put report
   */
  void writeReport(LintingContext context, String pathToReport);
}
