/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import utam.compiler.UtamCompilationError;

/**
 * Description at the level of the page object root, added as JavaDoc for generated page object
 *
 * @author elizaveta.ivanova
 * @since 238
 */
class UtamRootDescription {

  final static String ERR_FORMAT_ERROR =
      "format of the root comment can be either: \n1. \"description\" : \"string\" "
          + "or "
          + "\n2. \"description\" : { \"text\" : [\"array of strings\"], \"author\" : \"my team\" }";

  private final List<String> text = new ArrayList<>();
  private final String author;

  /**
   * Initialize object
   *
   * @param text   text of the page object description
   * @param author name of the team or person who owns a page object, by default UTAM
   */
  @JsonCreator
  UtamRootDescription(
      @JsonProperty(value = "text", required = true) List<String> text,
      @JsonProperty("author") String author) {
    if (text != null) {
      this.text.addAll(text);
    }
    this.author = author;
  }

  /**
   * Process/deserialize description node at the root of the page object
   *
   * @param descriptionNode Json node
   * @return object with description
   */
  static UtamRootDescription processRootDescriptionNode(JsonNode descriptionNode) {
    if (descriptionNode == null || descriptionNode.isNull()) {
      return new UtamRootDescription(null, null);
    }
    if (descriptionNode.isTextual()) {
      String value = descriptionNode.textValue();
      return new UtamRootDescription(Collections.singletonList(value), null);
    }
    if (descriptionNode.isObject()) {
      try {
        return new ObjectMapper().readValue(descriptionNode.toString(), UtamRootDescription.class);
      } catch (IOException e) {
        throw new UtamCompilationError(ERR_FORMAT_ERROR, e);
      }
    }
    throw new UtamCompilationError(ERR_FORMAT_ERROR);
  }

  /**
   * Get description as list of strings to wrap into javadoc format
   *
   * @return list of strings
   */
  List<String> getDescription() {
    List<String> res = new ArrayList<>(text);
    res.add(String.format("@author %s", (author == null ? "UTAM" : author)));
    res.add(String.format("@version %s",
        LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"))));
    return res;
  }

}
