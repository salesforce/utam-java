/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler;

import static utam.compiler.diagnostics.ValidationUtilities.VALIDATION;
import static utam.compiler.grammar.JsonDeserializer.isEmptyNode;

import com.fasterxml.jackson.core.JsonLocation;
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import utam.core.framework.consumer.UtamError;

/**
 * Error thrown during compilation
 *
 * @author elizaveta.ivanova
 * @since 234
 */
public class UtamCompilationError extends UtamError {

  private static final String JSON_SOURCE_SUBSTRING = "[Source:";

  /** Default serial version ID for serializable object */
  private static final long serialVersionUID = 1L;

  private final boolean hasJsonSource;

  /**
   * Initializes error thrown outside parser and without context
   *
   * @param message the message of the error
   */
  public UtamCompilationError(String message) {
    super(message);
    hasJsonSource = false;
  }

  /**
   * Instantiate, add JSON source to message
   *
   * @param node json node
   * @param message original message
   */
  public UtamCompilationError(JsonNode node, String message) {
    super(getErrorMessageWithJsonCode(node, message));
    hasJsonSource = true;
  }

  /**
   * Initializes a new instance of the UtamCompilationError class
   *
   * @param message the message of the error
   * @param e the inner exception wrapped by the error
   */
  public UtamCompilationError(String message, Throwable e) {
    super(message, e);
    hasJsonSource = false;
  }

  /**
   * Transform error thrown by mapper
   *
   * @param node JSON source
   * @param error original exception
   * @param errorMessage error message passed by processor, e.i. "error 500: incorrect format of
   *     matcher"
   * @return exception object
   */
  public static ErrorSupplier processMapperError(
      JsonNode node, Exception error, String errorMessage) {
    String message;
    Throwable cause;
    // there could be error from child objects
    UtamCompilationError compilationError = unwrapCompilerError(error);
    if (compilationError != null) {
      message =
          compilationError.hasJsonSource
              ? compilationError.getMessage()
              : getErrorMessageWithJsonCode(node, compilationError.getMessage());
      cause = compilationError.getCause();
    } else {
      // merge original message with processor message
      message = String.format("%s \n%s", errorMessage, error.getMessage());
      cause = error.getCause();
      // add JSON source
      message = getErrorMessageWithJsonCode(node, message);
    }
    return new ErrorSupplier(cause, message);
  }

  /**
   * Transform error thrown by parser
   *
   * @param parser JSON source
   * @param error original exception
   * @param pageObjectName name of the page object to add as prefix
   * @return exception object
   */
  public static ErrorSupplier processParserError(
      JsonParser parser, Exception error, String pageObjectName) {
    String errorMessage;
    Throwable errorCause;
    boolean hasJsonSource;
    // thrown from inside UtamPageObjects validation
    UtamCompilationError compilationError = unwrapCompilerError(error);
    if (compilationError != null) {
      errorMessage = compilationError.getMessage();
      errorCause = compilationError.getCause();
      hasJsonSource = compilationError.hasJsonSource;
    } else if (error instanceof JsonProcessingException) { // malformed JSON
      String errSplitMarker = "problem:";
      String message = error.getMessage();
      int index =
          message.contains(errSplitMarker)
              ? message.indexOf(errSplitMarker) + errSplitMarker.length()
              : 0;
      // empty string added because JS needs additional parameter for parser message
      errorMessage = VALIDATION.getErrorMessage(900, "") + message.substring(index);
      errorCause = error.getCause();
      hasJsonSource = true;
    } else {
      // empty string added because JS needs additional parameter for parser message
      errorMessage = VALIDATION.getErrorMessage(900, "") + error.getMessage();
      errorCause = error.getCause();
      hasJsonSource = false;
    }
    // add JSON source to message
    String messageWithSource =
        hasJsonSource ? errorMessage : getErrorMessageWithJsonCode(parser, errorMessage);
    // add PO name prefix
    String finalMessage =
        messageWithSource.contains(pageObjectName)
            ? messageWithSource
            : String.format("page object '%s' \n%s", pageObjectName, messageWithSource);
    return new ErrorSupplier(errorCause, finalMessage);
  }

  private static String getErrorMessageWithJsonCode(JsonParser parser, String message) {
    if (message.contains(JSON_SOURCE_SUBSTRING) || parser == null) {
      return message;
    }
    JsonLocation location = parser.getCurrentLocation();
    String prefix =
        String.format("line: %d, column: %d", location.getLineNr(), location.getColumnNr());
    String source = location.toString();
    return message + String.format("\nat %s %s", prefix, source);
  }

  private static String getErrorMessageWithJsonCode(JsonNode node, String message) {
    if (message.contains(JSON_SOURCE_SUBSTRING) || isEmptyNode(node)) {
      return message;
    }
    return message + String.format("\n[Source: %s]", node.toPrettyString());
  }

  private static UtamCompilationError unwrapCompilerError(Throwable error) {
    if (error instanceof NullPointerException) {
      return new UtamCompilationError("Error", error);
    }
    if (error instanceof UtamCompilationError) {
      return (UtamCompilationError) error;
    }
    Throwable e = error.getCause();
    while (e != null) {
      if (e instanceof UtamCompilationError) {
        return (UtamCompilationError) e;
      }
      e = e.getCause();
    }
    return null;
  }

  public static class ErrorSupplier {

    private final Throwable cause;
    private final String message;

    ErrorSupplier(Throwable cause, String message) {
      this.cause = cause;
      this.message = message;
    }

    public Throwable getCause() {
      return cause;
    }

    public String getMessage() {
      return message;
    }
  }
}
