/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import static utam.compiler.diagnostics.ValidationUtilities.VALIDATION;
import static utam.compiler.grammar.JsonDeserializer.isEmptyNode;
import static utam.compiler.grammar.JsonDeserializer.readNode;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.JsonNode;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Description at the level of the page object method (compose or getter), added as JavaDoc.
 *
 * @author elizaveta.ivanova
 * @since 238
 */
public class UtamMethodDescription {

  private final List<String> text = new ArrayList<>();
  private final String returnStr;
  private final String throwsStr;
  private final String deprecatedStr;

  /**
   * Initialize object
   *
   * @param text text describing what method does
   * @param returnStr describes meaning of the returned value. by default will be type of the
   *     returned value
   * @param throwsStr if method throws an exception, can add description for it
   * @param deprecatedStr if page object is deprecated, text explains why
   */
  @JsonCreator
  UtamMethodDescription(
      @JsonProperty(value = "text", required = true) List<String> text,
      @JsonProperty("return") String returnStr,
      @JsonProperty("throws") String throwsStr,
      @JsonProperty("deprecated") String deprecatedStr) {
    this.text.addAll(text);
    this.throwsStr = throwsStr;
    this.returnStr = returnStr;
    this.deprecatedStr = deprecatedStr;
  }

  /**
   * Initialize simple version of method description
   *
   * @param text text describing what method does
   */
  public UtamMethodDescription(String text) {
    this(List.of(text), null, null, null);
  }

  /**
   * Process/deserialize description node at the method or element level
   *
   * @param node Json node
   * @param validationContext element or method name with prefix
   * @return object with description
   */
  static UtamMethodDescription processMethodDescriptionNode(
      JsonNode node, String validationContext) {
    if (isEmptyNode(node)) {
      return new UtamEmptyMethodDescription();
    }
    String parserContext = validationContext + " description";
    if (node.isTextual()) {
      String value = node.textValue();
      VALIDATION.validateNotNullOrEmptyString(node, parserContext, "text");
      return new UtamMethodDescription(Collections.singletonList(value), null, null, null);
    }
    UtamMethodDescription object =
        readNode(node, UtamMethodDescription.class, VALIDATION.getErrorMessage(700, parserContext));
    VALIDATION.validateNotEmptyArray(node.get("text"), parserContext, "text");
    for (JsonNode textNode : node.get("text")) {
      VALIDATION.validateNotNullOrEmptyString(textNode, parserContext, "text");
    }
    VALIDATION.validateNotEmptyString(node.get("deprecated"), parserContext, "deprecated");
    VALIDATION.validateNotEmptyString(node.get("return"), parserContext, "return");
    VALIDATION.validateNotEmptyString(node.get("throws"), parserContext, "throws");
    return object;
  }

  /**
   * Used in linting to check if method has description
   *
   * @return boolean
   */
  boolean isEmpty() {
    return false;
  }

  /**
   * Get description text. Public method to access from compiled method
   *
   * @return list of strings
   */
  public List<String> getText() {
    return text;
  }

  /**
   * Get return description. Public method to access from compiled method
   *
   * @return string
   */
  public String getReturnStr() {
    return returnStr;
  }

  /**
   * Get throw message. Public method to access from compiled method
   *
   * @return string
   */
  public String getThrowsStr() {
    return throwsStr;
  }

  /**
   * Get deprecated message. Public method to access from compiled method
   *
   * @return string
   */
  public String getDeprecatedStr() {
    return deprecatedStr;
  }

  /**
   * Helper class for an empty method description
   *
   * @author elizaveta.ivanova
   * @since 242
   */
  public static class UtamEmptyMethodDescription extends UtamMethodDescription {

    public UtamEmptyMethodDescription() {
      super(new ArrayList<>(), null, null, null);
    }

    @Override
    boolean isEmpty() {
      return true;
    }
  }
}
