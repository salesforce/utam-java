/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.diagnostics;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.type.CollectionType;
import java.net.URL;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.MissingFormatArgumentException;
import utam.core.declarative.lint.LintingError.ViolationLevel;

/**
 * Mapping for pre-configured compiler error codes
 *
 * @author elizaveta.ivanova
 * @since 238
 */
public class JsonErrorsConfig {

  static final String ERR_FINDING_ERROR_CONFIG = "can't find error codes config '%s'";
  static final String ERR_READING_ERROR_CONFIG = "error while reading error codes config '%s': ";
  static final String ERR_CODE_NOT_CONFIGURED = "error code %d is not configured";
  // map between error code and details
  private final Map<Number, ErrorDetails> errorDetailsMap = new HashMap<>();

  /**
   * reads config with default name errors.config.json
   *
   * @return de-serialized object
   */
  public static JsonErrorsConfig getErrorsConfig() {
    return getErrorsConfig("errors.config.json");
  }

  /**
   * reads config with default name
   *
   * @param configName name of the file in resources
   * @return de-serialized object
   */
  static JsonErrorsConfig getErrorsConfig(String configName) {
    URL config = JsonErrorsConfig.class.getClassLoader().getResource(configName);
    if (config == null) {
      throw new IllegalArgumentException(String.format(ERR_FINDING_ERROR_CONFIG, configName));
    }
    ObjectMapper mapper = new ObjectMapper();
    try {
      CollectionType javaType =
          mapper.getTypeFactory().constructCollectionType(List.class, ErrorDetails.class);
      List<ErrorDetails> errors = mapper.readValue(config, javaType);
      JsonErrorsConfig jsonErrorsConfig = new JsonErrorsConfig();
      errors.forEach(
          errorDetails -> jsonErrorsConfig.errorDetailsMap.put(errorDetails.code, errorDetails));
      return jsonErrorsConfig;
    } catch (Exception e) {
      throw new IllegalStateException(String.format(ERR_READING_ERROR_CONFIG, configName), e);
    }
  }

  /**
   * read error message by code and replace %s by args if any
   *
   * @param code string code
   * @param args replacement for part of the messages that are context dependent
   * @return string with message or throws an error
   */
  public String getErrorMessage(Integer code, String... args) {
    if (!errorDetailsMap.containsKey(code)) {
      throw new IllegalArgumentException(String.format(ERR_CODE_NOT_CONFIGURED, code));
    }
    ErrorDetails errorDetails = errorDetailsMap.get(code);
    String message = errorDetails.getStringMessage(args);
    return String.format("%s %s: %s", errorDetails.errorCategory.name(), code, message);
  }

  /**
   * read error message by code and replace %s by args if any
   *
   * @param violationLevel linting violation level
   * @param code string code
   * @param args replacement for part of the messages that are context dependent
   * @return string with message or throws an error
   */
  public String getLintingMessage(ViolationLevel violationLevel, Integer code, String... args) {
    if (!errorDetailsMap.containsKey(code)) {
      throw new IllegalArgumentException(String.format(ERR_CODE_NOT_CONFIGURED, code));
    }
    ErrorDetails errorDetails = errorDetailsMap.get(code);
    String message = errorDetails.getStringMessage(args);
    return String.format("%s %s: %s", violationLevel.name(), code, message);
  }

  /** error code can be one of 2 types */
  enum ErrorCategory {
    error,
    warning
  }

  /**
   * Details of one configured error
   *
   * @author elizaveta.ivanova
   * @since 238
   */
  static class ErrorDetails {

    private final int code;
    private final String message;
    private final String docs;
    private final String tip;
    private final ErrorCategory errorCategory;

    @JsonCreator
    ErrorDetails(
        @JsonProperty(value = "code", required = true) Integer code,
        @JsonProperty(value = "message", required = true) String message,
        @JsonProperty(value = "docs") String docs,
        @JsonProperty(value = "tip") String tip,
        @JsonProperty(value = "category") ErrorCategory category) {
      this.message = message;
      this.docs = docs;
      this.errorCategory = category == null ? ErrorCategory.error : category;
      this.tip = tip;
      this.code = code;
    }

    /**
     * construct full error message
     *
     * @param args replacement for part of the message that is context dependent
     * @return string with message
     */
    String getStringMessage(String... args) {
      String res;
      try {
        res = String.format(message, args);
      } catch (MissingFormatArgumentException e) {
        throw new MissingFormatArgumentException(
            String.format("Incorrect args %s for message \"%s\"", Arrays.toString(args), message));
      }
      if (docs != null) {
        res = res.concat(String.format("; \nsee documentation %s", docs));
      }
      if (tip != null) {
        res = res.concat(String.format("; \ntip: %s", tip));
      }
      return res;
    }
  }
}
