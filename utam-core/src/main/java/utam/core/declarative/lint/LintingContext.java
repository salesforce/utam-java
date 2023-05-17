/*
 * Copyright (c) 2022, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.declarative.lint;

import java.util.List;

/**
 * Configuration for linting
 *
 * @author elizaveta.ivanova
 * @since 242
 */
public interface LintingContext {

  /**
   * Get all compiled page objects from the run
   *
   * @return list of string
   */
  List<PageObjectLinting> getAllPageObjects();

  /**
   * Add a page object information to the linting context
   *
   * @param pageObject result of the page object linting
   */
  void addPageObject(PageObjectLinting pageObject);

  /**
   * Get linting errors after linting process is complete
   *
   * @return lit of errors
   */
  List<LintingError> getErrors();
}
