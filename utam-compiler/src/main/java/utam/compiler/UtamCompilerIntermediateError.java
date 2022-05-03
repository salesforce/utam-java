/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler;

import com.fasterxml.jackson.core.JsonLocation;
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.databind.JsonNode;
import java.util.function.Supplier;
import utam.compiler.helpers.TranslationContext;

/**
 * This exception is thrown from deserializer when we do not know the context. Then we catch it and
 * process
 *
 * @author elizaveta.ivanova
 * @since 238
 */
public class UtamCompilerIntermediateError extends RuntimeException {

  private static final String JSON_SOURCE_SUBSTRING = "[Source:";

  private final Integer errCode;
  private final String[] args;
  private final JsonNode currentNode;
  private final Throwable nullableCause;

  /**
   * Initialize exception that will be used to produce meaningful error
   *
   * @param cause       original error
   * @param currentNode json node to print out source
   * @param errCode     code
   * @param args        replacement for part of the messages that are context dependent
   */
  public UtamCompilerIntermediateError(Throwable cause, JsonNode currentNode, Integer errCode,
      String... args) {
    this.nullableCause = cause;
    this.currentNode = currentNode;
    this.errCode = errCode;
    this.args = args;
  }

  /**
   * Initialize exception that will be used to produce meaningful error
   *
   * @param currentNode json node to print out source
   * @param errCode     code
   * @param args        replacement for part of the messages that are context dependent
   */
  public UtamCompilerIntermediateError(JsonNode currentNode, Integer errCode, String... args) {
    this(null, currentNode, errCode, args);
  }

  /**
   * Initialize exception that will be used to produce meaningful error
   *
   * @param errCode code
   * @param args    replacement for part of the messages that are context dependent
   */
  public UtamCompilerIntermediateError(Integer errCode, String... args) {
    this(null, null, errCode, args);
  }

  /**
   * get string with json source to add to error: [Source: (String)"{"public": true}"; line: 1,
   * column: 19]
   *
   * @param currentNode current node, can be null
   * @return string with json source
   */
  private static String getJsonSource(JsonNode currentNode) {
    if (currentNode == null || currentNode.isNull()) {
      return "";
    }
    return String.format("\n[Source: %s]", currentNode.toPrettyString());
  }

  /**
   * get string with json source to add to error: [Source: (String)"{"public": true}"; line: 1,
   * column: 19]
   *
   * @param parser parser passed to page object deserializer
   * @return string with json source
   */
  private static String getJsonSource(JsonParser parser) {
    if (parser != null) { // in tests
      JsonLocation location = parser.getCurrentLocation();
      String prefix = String
          .format("line: %d, column: %d", location.getLineNr(), location.getColumnNr());
      String source = location.toString();
      return String.format("\nat %s %s", prefix, source);
    }
    return "\nJSON source";
  }

  /**
   * add json source to an error message
   *
   * @param parser  json parser
   * @param message original message
   * @return string
   */
  static String getErrorMessageWithJsonCode(JsonParser parser, String message) {
    if (message.contains(JSON_SOURCE_SUBSTRING)) {
      return message;
    }
    return message + getJsonSource(parser);
  }

  /**
   * add json source to an error message
   *
   * @param node    json node
   * @param message original message
   * @return string
   */
  static String getErrorMessageWithJsonCode(JsonNode node, String message) {
    if (message.contains(JSON_SOURCE_SUBSTRING)) {
      return message;
    }
    return message + getJsonSource(node);
  }

  /**
   * get json node type as a string
   *
   * @param node json node
   * @return string
   */
  public static String getJsonNodeType(JsonNode node) {
    return node.getNodeType().name().toLowerCase();
  }

  /**
   * original exception might have thrown from constructor, so has no node content
   *
   * @param currentNode node passed from parser
   * @return error supplier
   */
  public Supplier<UtamCompilerIntermediateError> addJsonSource(JsonNode currentNode) {
    if (this.currentNode != null) {
      return () -> this;
    }
    return () -> new UtamCompilerIntermediateError(nullableCause, currentNode, errCode, args);
  }

  /**
   * get error message with page object name prefix
   *
   * @param context translation context
   * @param parser  in case error did not get source
   * @return string with message
   */
  private String getErrorMessage(TranslationContext context, JsonParser parser) {
    String message = context.getErrorMessage(errCode, args);
    if (message.contains(JSON_SOURCE_SUBSTRING)) {
      // exception from the parser might already contain source
      return message;
    }
    String jsonSource = currentNode != null ? getJsonSource(currentNode) : getJsonSource(parser);
    return message + jsonSource;
  }

  /**
   * get compilation exception
   *
   * @param context translation context
   * @param parser  json parser
   * @param cause   original error
   * @return error supplier
   */
  public Supplier<UtamCompilationError> getCompilationError(TranslationContext context,
      JsonParser parser,
      Throwable cause) {
    Throwable error = nullableCause == null ? cause : nullableCause;
    if (error == null) {
      return () -> new UtamCompilationError(getErrorMessage(context, parser));
    }
    return () -> new UtamCompilationError(getErrorMessage(context, parser), error);
  }
}
