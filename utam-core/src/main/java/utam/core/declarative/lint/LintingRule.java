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
 * Linting rule
 *
 * @author elizaveta.ivanova
 * @since 242
 */
public interface LintingRule {

  /**
   * Apply rule to a single page object after it's generated
   *
   * @param errors container to report errors
   * @param pageObject page object
   */
  void validate(List<LintingError> errors, PageObjectLinting pageObject);

  /**
   * Apply rule to all page objects to cross check them
   *
   * @param first first page object (already linted)
   * @param second second page object (already linted)
   * @param context all page objects under linting
   */
  void validate(PageObjectLinting first, PageObjectLinting second, LintingContext context);

  /**
   * Get unique rule ID in the format ULR(number)
   *
   * @return rule id
   */
  String getId();

  /**
   * Get rule name
   *
   * @return string to use for SARIF JSON
   */
  String getName();

  /**
   * Get short description of the rule
   *
   * @return string to use for SARIF JSON
   */
  String getDescription();
}
