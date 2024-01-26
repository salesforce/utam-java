/*
 * Copyright (c) 2022, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.declarative.errors;

import java.util.List;

/**
 * Errors context is created when compiler starts and stores all compiler errors
 *
 * @author elizaveta.ivanova
 * @since 244
 */
public interface CompilerErrorsContext {

  /**
   * Add compiler error to report
   *
   * @param error instance of the error
   */
  default void setError(CompilerError error) {}

  /**
   * Get report with errors for all page objects. By default null because first error throws.
   *
   * @return list of errors or null
   */
  default List<CompilerError> getCompilerReport() {
    return null;
  }

  /**
   * Instance of the compiler error. Currently only needs toString method
   *
   * @author elizaveta.ivanova
   * @since 244
   */
  interface CompilerError {}
}
