/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.translator;

import static com.fasterxml.jackson.core.JsonParser.Feature.ALLOW_COMMENTS;
import static com.fasterxml.jackson.core.JsonParser.Feature.STRICT_DUPLICATE_DETECTION;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonDeserializer;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import java.io.IOException;
import java.net.URL;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.MissingFormatArgumentException;
import utam.compiler.translator.JsonErrorsConfig.Deserializer;

/**
 * Mapping for pre-configured compiler error codes
 *
 * @author elizaveta.ivanova
 * @since 238
 */
@JsonDeserialize(using = Deserializer.class)
class JsonErrorsConfig {

  static final String ERR_FINDING_ERROR_CONFIG = "can't find error codes config '%s'";
  static final String ERR_READING_ERROR_CONFIG = "error while reading error codes config '%s': ";
  static final String ERR_CODE_NOT_CONFIGURED = "error code '%s' is not configured";
  // map between error code and details
  private final Map<String, ErrorDetails> errorDetailsMap = new HashMap<>();

  /**
   * reads config with default name errors.config.json
   *
   * @return de-serialized object
   */
  static JsonErrorsConfig getErrorsConfigWithDefaultName() {
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
    mapper.enable(ALLOW_COMMENTS);
    mapper.enable(STRICT_DUPLICATE_DETECTION);
    try {
      return mapper.readValue(config, JsonErrorsConfig.class);
    } catch (IOException e) {
      throw new IllegalStateException(
          String.format(ERR_READING_ERROR_CONFIG, configName) + e.getMessage());
    }
  }

  /**
   * read error message by code and replace %s by args if any
   *
   * @param code string code
   * @param args replacement for part of the messages that are context dependent
   * @return string with message or throws an error
   */
  String getErrorMessage(String code, String... args) {
    if (!errorDetailsMap.containsKey(code)) {
      throw new IllegalArgumentException(String.format(ERR_CODE_NOT_CONFIGURED, code));
    }
    ErrorDetails errorDetails = errorDetailsMap.get(code);
    String message = errorDetails.getStringMessage(args);
    return String.format("%s %s: %s", errorDetails.errorCodeType.name(), code, message);
  }

  /**
   * error code can be one of 2 types
   */
  enum ErrorCodeType {
    error, warning
  }

  /**
   * de-serializer for error config file
   *
   * @author elizaveta.ivanova
   * @since 238
   */
  static class Deserializer extends JsonDeserializer<JsonErrorsConfig> {

    @Override
    public JsonErrorsConfig deserialize(JsonParser jp, DeserializationContext ctxt)
        throws IOException {
      JsonNode node = jp.getCodec().readTree(jp);
      Iterator<String> errors = node.fieldNames();
      ObjectMapper objectMapper = new ObjectMapper();
      JsonErrorsConfig jsonErrorsConfig = new JsonErrorsConfig();
      while (errors.hasNext()) {
        String errorCode = errors.next();
        JsonNode errorDetailsNode = node.get(errorCode);
        ErrorDetails errorDetails = objectMapper.treeToValue(errorDetailsNode, ErrorDetails.class);
        jsonErrorsConfig.errorDetailsMap.put(errorCode, errorDetails);
      }
      return jsonErrorsConfig;
    }
  }

  /**
   * Details of one configured error
   *
   * @author elizaveta.ivanova
   * @since 238
   */
  static class ErrorDetails {

    private final String message;
    private final String docs;
    private final String tip;
    private final ErrorCodeType errorCodeType;

    @JsonCreator
    ErrorDetails(
        @JsonProperty(value = "message", required = true) String message,
        @JsonProperty(value = "docs") String docs,
        @JsonProperty(value = "tip") String tip,
        @JsonProperty(value = "type") ErrorCodeType type
    ) {
      this.message = message;
      this.docs = docs;
      this.errorCodeType = type == null? ErrorCodeType.error : type;
      this.tip = tip;
    }

    /**
     * construct full error message
     *
     * @param args replacement for part of the message that is context dependent
     * @return string with message
     */
    String getStringMessage(String...args) {
      String res;
      try {
        res = String.format(message, args);
      } catch (MissingFormatArgumentException e) {
        throw new MissingFormatArgumentException(String.format("Incorrect args %s for message \"%s\"",
            Arrays.toString(args), message));
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
