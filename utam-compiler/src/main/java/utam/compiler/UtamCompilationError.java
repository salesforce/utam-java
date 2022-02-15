/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler;

import static utam.compiler.UtamCompilerIntermediateError.getErrorMessageWithJsonCode;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.databind.JsonNode;
import utam.core.framework.consumer.UtamError;

/**
 * Error thrown in runtime from UTAM compiler
 *
 * @author elizaveta.ivanova
 * @since 234
 */
public class UtamCompilationError extends UtamError {

  /**
   * Default serial version ID for serializable object
   */
  private static final long serialVersionUID = 1L;

  /**
   * Initializes a new instance of the UtamCompilationError class
   *
   * @param message the message of the error
   */
  public UtamCompilationError(String message) {
    super(message);
  }

  /**
   * add json code to message
   *
   * @param parser  json parser
   * @param message original message
   */
  public UtamCompilationError(JsonParser parser, String message) {
    super(getErrorMessageWithJsonCode(parser, message));
  }

  /**
   * add json code to message
   *
   * @param parser  json parser
   * @param message original message
   * @param t       original exception
   */
  public UtamCompilationError(JsonParser parser, String message, Throwable t) {
    super(getErrorMessageWithJsonCode(parser, message), t);
  }

  /**
   * add json code to message
   *
   * @param node    json node
   * @param message original message
   */
  public UtamCompilationError(JsonNode node, String message) {
    super(getErrorMessageWithJsonCode(node, message));
  }

  /**
   * Initializes a new instance of the UtamCompilationError class
   *
   * @param message the message of the error
   * @param e the inner exception wrapped by the error
   */
  public UtamCompilationError(String message, Throwable e) {
    super(message, e);
  }
}
