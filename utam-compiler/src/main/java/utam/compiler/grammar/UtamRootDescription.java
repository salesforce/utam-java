/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import static utam.compiler.grammar.JsonDeserializer.readNode;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.JsonNode;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;
import utam.compiler.UtamCompilerIntermediateError;

/**
 * Description at the level of the page object root, added as JavaDoc for generated page object
 *
 * @author elizaveta.ivanova
 * @since 238
 */
class UtamRootDescription {

  final static String VERSION_TAG = "@version";
  private final List<String> text = new ArrayList<>();
  private final String author;
  private final String deprecated;

  /**
   * Initialize object
   *
   * @param text   text of the page object description
   * @param author name of the team or person who owns a page object, by default UTAM
   * @param deprecated if page object is deprecated, text explains why
   */
  @JsonCreator
  UtamRootDescription(
      @JsonProperty(value = "text", required = true) List<String> text,
      @JsonProperty("author") String author,
      @JsonProperty("deprecated") String deprecated) {
    if (text != null) {
      this.text.addAll(text);
    }
    this.author = author;
    this.deprecated = deprecated;
  }

  /**
   * Process/deserialize description node at the root of the page object
   *
   * @param descriptionNode Json node
   * @return object with description
   */
  static UtamRootDescription processRootDescriptionNode(JsonNode descriptionNode) {
    if (descriptionNode == null || descriptionNode.isNull()) {
      return new UtamRootDescription(null, null, null);
    }
    // "description" : "text"
    if (descriptionNode.isTextual()) {
      String value = descriptionNode.textValue();
      return new UtamRootDescription(Collections.singletonList(value), null, null);
    }
    Function<Exception, RuntimeException> parserErrorWrapper = causeErr -> new UtamCompilerIntermediateError(
        causeErr, descriptionNode, 906, causeErr.getMessage());
    return readNode(descriptionNode, UtamRootDescription.class, parserErrorWrapper);
  }

  /**
   * Get description as list of strings to wrap into javadoc format
   *
   * @param version                page objects with version number
   * @param sourceFileRelativePath relative path with Json
   * @return list of strings
   */
  List<String> getDescription(String version, String sourceFileRelativePath) {
    List<String> descriptionLines = new ArrayList<>(text);
    if(sourceFileRelativePath != null) {
      // On Windows, sourceFileRelativePath may contain backslashes ("\"), which will
      // be misinterpreted in Javadoc comments by the Java source code formatter.
      // Replacing them with forward slashes ("/") ensures consistent generation of
      // Java files cross-platform.
      String addComma = text.isEmpty()? "" : ", ";
      descriptionLines.add(
          String.format("%screated from JSON %s", addComma,
              sourceFileRelativePath.replace("\\", "/")));
    }
    // add line @author team_name
    descriptionLines.add(String.format("@author %s", (author == null ? "UTAM" : author)));
    // add line @version with timestamp
    descriptionLines.add(String.format("%s %s", VERSION_TAG, version));
    // add line @deprecated
    if(isDeprecated()) {
      descriptionLines.add(String.format("@deprecated %s", this.deprecated));
    }
    // replace any invalid HTML characters with their appropriate encoded entities.
    // special note: iOS class chains contain "*/" in their paths, so this must be
    // escaped for Javadoc generation.
    return descriptionLines.stream()
        .map((s) -> s
            .replace("&", "&amp;")
            .replace("<", "&lt;")
            .replace(">", "&gt;")
            .replace("*/", "*&#47;"))
        .collect(Collectors.toList());
  }

  /**
   * check if page object was marked as deprecated
   *
   * @return boolean
   */
  boolean isDeprecated() {
    return this.deprecated != null;
  }
}
