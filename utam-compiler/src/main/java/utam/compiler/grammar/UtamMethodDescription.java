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
import static utam.compiler.helpers.TypeUtilities.VOID;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.JsonNode;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import utam.core.declarative.representation.MethodDeclaration;
import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.TypeProvider;

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
   * @param text          text describing what method does
   * @param returnStr     describes meaning of the returned value. by default will be type of the
   *                      returned value
   * @param throwsStr     if method throws an exception, can add description for it
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

  public UtamMethodDescription() {
    this(new ArrayList<>(), null, null, null);
  }

  /**
   * Process/deserialize description node at the method or element level
   *
   * @param node              Json node
   * @param validationContext element or method name with prefix
   * @return object with description
   */
  static UtamMethodDescription processMethodDescriptionNode(JsonNode node,
      String validationContext) {
    if (isEmptyNode(node)) {
      return new UtamMethodDescription();
    }
    String parserContext = validationContext + " description";
    if (node.isTextual()) {
      String value = node.textValue();
      VALIDATION.validateNotNullOrEmptyString(node, parserContext, "text");
      return new UtamMethodDescription(Collections.singletonList(value), null, null, null);
    }
    UtamMethodDescription object = readNode(node, UtamMethodDescription.class, VALIDATION.getErrorMessage(700, parserContext));
    VALIDATION.validateNotEmptyArray(node.get("text"), parserContext, "text");
    for(JsonNode textNode : node.get("text")) {
      VALIDATION.validateNotNullOrEmptyString(textNode, parserContext, "text");
    }
    VALIDATION.validateNotEmptyString(node.get("deprecated"), parserContext, "deprecated");
    VALIDATION.validateNotEmptyString(node.get("return"), parserContext, "return");
    VALIDATION.validateNotEmptyString(node.get("throws"), parserContext, "throws");
    return object;
  }

  public MethodDescription getDescription(MethodDeclaration method) {
    return new MethodDescription(method, text, returnStr, throwsStr, deprecatedStr);
  }

  /**
   * Helper class to process description in context and return javadoc
   *
   * @author elizaveta.ivanova
   * @since 242
   */
  public static class MethodDescription {

    private final List<String> javadoc;
    private final boolean isDeprecated;

    private MethodDescription(MethodDeclaration method, List<String> text,
        String returnStr,
        String throwsStr,
        String deprecatedStr) {
      this(method.getName(), method.getReturnType(), method.getParameters(), text, returnStr, throwsStr, deprecatedStr);
    }

    private MethodDescription(
        String methodName,
        TypeProvider returnType,
        List<MethodParameter> parameters,
        List<String> text,
        String returnStr,
        String throwsStr,
        String deprecatedStr) {
      javadoc = new ArrayList<>();
      if (text.isEmpty()) {
        javadoc.add("method " + methodName);
      } else {
        javadoc.addAll(text);
      }
      if (returnStr != null) {
        javadoc.add(String.format("@return %s", returnStr));
      } else if (!returnType.isSameType(VOID)) {
        javadoc
            .add(String.format("@return %s", returnType.getSimpleName()));
      }
      parameters.stream()
          .filter(p -> !p.isLiteral())
          .forEach(parameter -> {
            final String parameterDescription =
                parameter.getDescription() == null ? parameter.getType().getSimpleName()
                    : parameter.getDescription();
            javadoc
                .add(String.format("@param %s %s", parameter.getValue(), parameterDescription));
          });
      if (throwsStr != null) {
        javadoc.add(String.format("@throws %s", throwsStr));
      }
      this.isDeprecated = deprecatedStr != null;
      if (isDeprecated) {
        javadoc.add(String.format("@deprecated %s", deprecatedStr));
      }
    }

    public List<String> getJavadoc() {
      return javadoc;
    }

    public boolean isDeprecated() {
      return isDeprecated;
    }
  }
}
