/*
 * Copyright (c) 2022, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.declarative.errors;

/**
 * Configuration decides how to handle compiler error - interrupt runner or collect a report
 *
 * @author elizaveta.ivanova
 * @since 244
 */
public interface CompilerErrorsConfig {

  /**
   * Return true if runner should stop at first error
   *
   * @return boolean
   */
  default boolean isInterrupt() {
    return true;
  }

  /**
   * Creates new context at the beginning of the runner execution
   *
   * @return context instance
   */
  CompilerErrorsContext newContext();

  /**
   * Write all errors to the file and to the console exception, if configured
   *
   * @param context context of the runner errors
   * @return string with all compilation errors or null
   */
  default String report(CompilerErrorsContext context) {
    return null;
  }
}
