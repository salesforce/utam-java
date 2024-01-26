/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import static utam.compiler.diagnostics.ValidationUtilities.VALIDATION;
import static utam.compiler.grammar.JsonDeserializer.readNode;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.JsonNode;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import utam.compiler.helpers.TranslationContext;
import utam.compiler.representation.JavadocObject;
import utam.compiler.representation.JavadocObject.PageObjectJavadoc;

/**
 * Description at the level of the page object root, added as JavaDoc for generated page object
 *
 * @author elizaveta.ivanova
 * @since 238
 */
class UtamRootDescription {

  private final List<String> text;
  private final String author;
  private final String deprecated;

  /**
   * Initialize object
   *
   * @param text text of the page object description
   * @param author name of the team or person who owns a page object, by default UTAM
   * @param deprecated if page object is deprecated, text explains why
   */
  @JsonCreator
  private UtamRootDescription(
      @JsonProperty(value = "text", required = true) List<String> text,
      @JsonProperty("author") String author,
      @JsonProperty("deprecated") String deprecated) {
    this.text = text;
    this.author = author;
    this.deprecated = deprecated;
  }

  /**
   * Process/deserialize description node at the root of the page object
   *
   * @param node Json node
   * @return object with description
   */
  static UtamRootDescription processRootDescriptionNode(JsonNode node) {
    String parserContext = "page object description";
    UtamRootDescription object;
    if (node == null || node.isNull()) {
      object = new UtamEmptyRootDescription();
    } else if (node.isTextual()) {
      String value = node.textValue();
      VALIDATION.validateNotNullOrEmptyString(node, parserContext, "text");
      object = new UtamRootDescription(Collections.singletonList(value), null, null);
    } else {
      object = readNode(node, UtamRootDescription.class, VALIDATION.getErrorMessage(905));
      VALIDATION.validateNotEmptyArray(node.get("text"), parserContext, "text");
      for (JsonNode textNode : node.get("text")) {
        VALIDATION.validateNotNullOrEmptyString(textNode, parserContext, "text");
      }
      VALIDATION.validateNotEmptyString(node.get("author"), parserContext, "author");
      VALIDATION.validateNotEmptyString(node.get("deprecated"), parserContext, "deprecated");
    }
    return object;
  }

  JavadocObject getDescription(TranslationContext context) {
    return new PageObjectJavadoc(context, text, author, deprecated);
  }

  /**
   * Used for linting to check if description is set
   *
   * @return boolean
   */
  boolean isEmpty() {
    return false;
  }

  /**
   * Used for linting to check if author is set
   *
   * @return boolean
   */
  boolean hasAuthor() {
    return author != null;
  }

  /**
   * Helper class for an empty root description
   *
   * @author elizaveta.ivanova
   * @since 242
   */
  private static class UtamEmptyRootDescription extends UtamRootDescription {

    private UtamEmptyRootDescription() {
      super(new ArrayList<>(), null, null);
    }

    @Override
    boolean isEmpty() {
      return true;
    }
  }
}
