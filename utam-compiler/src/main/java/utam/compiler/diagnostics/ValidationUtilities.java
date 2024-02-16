/*
 * Copyright (c) 2022, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.diagnostics;

import static utam.compiler.diagnostics.JsonErrorsConfig.getErrorsConfig;

import com.fasterxml.jackson.databind.JsonNode;
import java.util.List;
import utam.compiler.UtamCompilationError;
import utam.core.declarative.lint.LintingError.ViolationLevel;

/**
 * Static validation utilities. JSON Parser does not have access to TranslationContext and compiler
 * needs to throw correct error before accessing context. Class has utility methods for common
 * validations.
 *
 * @author elizaveta.ivanova
 * @since 242
 */
public class ValidationUtilities {

  /** Global validation object for parsing without context */
  public static final ValidationUtilities VALIDATION = new ValidationUtilities();

  private final JsonErrorsConfig errorsConfig;

  private ValidationUtilities() {
    this.errorsConfig = getErrorsConfig();
  }

  private static String getJsonNodeType(JsonNode node) {
    return node.getNodeType().name().toLowerCase();
  }

  /**
   * Get error message with the given code
   *
   * @param code error code
   * @param args optional args
   * @return error message
   */
  public String getErrorMessage(Integer code, String... args) {
    return errorsConfig.getErrorMessage(code, args);
  }

  /**
   * Get linting message with the given code
   *
   * @param violationLevel type of the violation
   * @param code error code
   * @param args optional args
   * @return error message
   */
  public String getLintingMessage(ViolationLevel violationLevel, Integer code, String... args) {
    return errorsConfig.getLintingMessage(violationLevel, code, args);
  }

  private String getEmptyStringError(String prefix, String propertyName, String actualValue) {
    return getErrorMessage(10, prefix, propertyName, "a non empty string", actualValue);
  }

  /**
   * Validate that string is not null or empty
   *
   * @param node JSON node
   * @param prefix validation context
   * @param propertyName name of the JSON property
   * @return JSON node text value as string
   */
  public String validateNotNullOrEmptyString(JsonNode node, String prefix, String propertyName) {
    if (node == null || node.isNull()) {
      throw new UtamCompilationError(node, getEmptyStringError(prefix, propertyName, "null"));
    }
    return validateNotEmptyString(node, prefix, propertyName);
  }

  /**
   * Validate that string is not null or empty
   *
   * @param value string value
   * @param prefix validation context
   * @param propertyName name of the JSON property
   */
  public void validateNotNullOrEmptyString(String value, String prefix, String propertyName) {
    if (value == null) {
      throw new UtamCompilationError(getEmptyStringError(prefix, propertyName, "null"));
    }
    validateNotEmptyString(value, prefix, propertyName);
  }

  /**
   * Validate that string is not empty. It can be null
   *
   * @param node JSON node for error
   * @param prefix validation context
   * @param propertyName name of the JSON property
   * @return JSON node text value as string
   */
  public String validateNotEmptyString(JsonNode node, String prefix, String propertyName) {
    if (node == null || node.isNull()) {
      return null;
    }
    if (!node.isTextual()) {
      throw new UtamCompilationError(
          node, getEmptyStringError(prefix, propertyName, getJsonNodeType(node)));
    }
    if (node.textValue().isEmpty()) {
      throw new UtamCompilationError(node, getEmptyStringError(prefix, propertyName, "empty"));
    }
    return node.textValue();
  }

  /**
   * Validate that string is not empty. It can be null
   *
   * @param value string value
   * @param prefix validation context
   * @param propertyName name of the JSON property
   */
  public void validateNotEmptyString(String value, String prefix, String propertyName) {
    if (value == null) {
      return;
    }
    if (value.isEmpty()) {
      throw new UtamCompilationError(getEmptyStringError(prefix, propertyName, "empty"));
    }
  }

  private String getEmptyArrayError(String prefix, String propertyName) {
    return getErrorMessage(12, prefix, propertyName);
  }

  /**
   * Validate that array is not null or empty
   *
   * @param arrayNode JSON node
   * @param prefix validation context
   * @param propertyName name of the JSON property
   */
  public void validateNotEmptyArray(JsonNode arrayNode, String prefix, String propertyName) {
    if (arrayNode == null || arrayNode.isNull()) {
      throw new UtamCompilationError(arrayNode, getEmptyArrayError(prefix, propertyName));
    }
    if (!arrayNode.isArray() || arrayNode.isEmpty()) {
      throw new UtamCompilationError(arrayNode, getEmptyArrayError(prefix, propertyName));
    }
  }

  /**
   * Validate that array is not empty. Property is optional.
   *
   * @param arrayNode JSON node
   * @param prefix validation context
   * @param propertyName name of the JSON property
   */
  public void validateOptionalNotEmptyArray(
      JsonNode arrayNode, String prefix, String propertyName) {
    if (arrayNode == null || arrayNode.isNull()) {
      return;
    }
    if (!arrayNode.isArray() || arrayNode.isEmpty()) {
      throw new UtamCompilationError(arrayNode, getEmptyArrayError(prefix, propertyName));
    }
  }

  /**
   * Validate that node is an object.
   *
   * @param node JSON node
   * @param prefix validation context
   * @param propertyName name of the JSON property
   */
  public void validateNotNullObject(JsonNode node, String prefix, String propertyName) {
    if (node == null || node.isNull() || !node.isObject()) {
      throw new UtamCompilationError(node, getErrorMessage(13, prefix, propertyName));
    }
  }

  /**
   * Validate that node is an object. can be null.
   *
   * @param node JSON node
   * @param prefix validation context
   * @param propertyName name of the JSON property
   */
  public void validateIsObject(JsonNode node, String prefix, String propertyName) {
    if (node == null || node.isNull()) {
      return;
    }
    if (!node.isObject()) {
      throw new UtamCompilationError(node, getErrorMessage(13, prefix, propertyName));
    }
  }

  /**
   * Validate that array contains not empty strings
   *
   * @param arrayNode JSON node
   * @param prefix validation context
   */
  public void validateArrayOfStrings(JsonNode arrayNode, String prefix) {
    for (JsonNode valueNode : arrayNode) {
      if (!valueNode.isTextual() || valueNode.textValue().isEmpty()) {
        throw new UtamCompilationError(
            valueNode, VALIDATION.getErrorMessage(11, prefix, getJsonNodeType(valueNode)));
      }
    }
  }

  /**
   * Validate that property is not null
   *
   * @param object property value
   * @param prefix validation context
   * @param propertyName name of the JSON property
   */
  public void validateRequiredProperty(Object object, String prefix, String propertyName) {
    if (object == null) {
      throw new UtamCompilationError(getErrorMessage(9, prefix, propertyName));
    }
  }

  /**
   * Validate that property is null
   *
   * @param object property value
   * @param prefix validation context
   * @param propertyName name of the JSON property
   * @param supported supported values
   */
  public void validateUnsupportedProperty(
      Object object, String prefix, String propertyName, String supported) {
    if (object != null) {
      throw new UtamCompilationError(getErrorMessage(8, prefix, propertyName, supported));
    }
  }

  /**
   * Validate that property is null
   *
   * @param list property value
   * @param prefix validation context
   * @param propertyName name of the JSON property
   * @param supported supported values
   */
  public void validateUnsupportedProperty(
      List<?> list, String prefix, String propertyName, String supported) {
    if (list != null && !list.isEmpty()) {
      throw new UtamCompilationError(getErrorMessage(8, prefix, propertyName, supported));
    }
  }
}
