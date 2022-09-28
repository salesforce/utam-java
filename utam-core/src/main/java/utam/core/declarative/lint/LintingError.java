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
   * Get an error message
   *
   * @return string
   */
  String getMessage();

  /**
   * If set to true, error should throw an exception and interrupt compilation
   *
   * @return boolean
   */
  boolean isThrowError();

  /**
   * Exclusions should not be reported
   *
   * @return boolean
   */
  boolean isReport();
}
