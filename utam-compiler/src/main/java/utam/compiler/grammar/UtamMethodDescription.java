/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import static utam.compiler.grammar.JsonDeserializer.readNode;
import static utam.compiler.helpers.TypeUtilities.VOID;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.JsonNode;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;
import utam.compiler.UtamCompilerIntermediateError;
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
    if (text != null) {
      this.text.addAll(text);
    }
    this.throwsStr = throwsStr;
    this.returnStr = returnStr;
    this.deprecatedStr = deprecatedStr;
  }

  /**
   * Process/deserialize description node at the method or element level
   *
   * @param descriptionNode Json node
   * @param parserContext   element or method name with prefix
   * @return object with description
   */
  static UtamMethodDescription processMethodDescriptionNode(JsonNode descriptionNode, String parserContext) {
    if (descriptionNode == null || descriptionNode.isNull()) {
      return null;
    }
    if (descriptionNode.isTextual()) {
      String value = descriptionNode.textValue();
      return new UtamMethodDescription(Collections.singletonList(value), null, null, null);
    }
    Function<Exception, RuntimeException> parserErrorWrapper = causeErr -> new UtamCompilerIntermediateError(
        causeErr, descriptionNode, 700, parserContext, causeErr.getMessage());
    return readNode(descriptionNode, UtamMethodDescription.class,
        parserErrorWrapper);
  }

  /**
   * if parameter is not literal, add line for javadoc
   *
   * @param parameter   passed argument
   * @param description list with results
   */
  private static void setParameterText(MethodParameter parameter, List<String> description) {
    if (!parameter.isLiteral()) {
      final String pattern = String.format("@param %s", parameter.getValue()) + " %s";
      final String javadocStr = parameter.getDescription() == null ?
          String.format(pattern, parameter.getType().getSimpleName())
          : String.format(pattern, parameter.getDescription());
      description.add(javadocStr);
    }
  }

  /**
   * add javadoc line with method description, if none set, use method name
   *
   * @param methodName  string with method name
   * @param object      description object, can be null
   * @param description list with results
   */
  private static void setText(String methodName, UtamMethodDescription object,
      List<String> description) {
    List<String> text = object == null ? new ArrayList<>() : object.text;
    if (text.isEmpty()) {
      description.add("method " + methodName);
    } else {
      description.addAll(text);
    }
  }

  /**
   * set return javadoc string
   *
   * @param returnType  method return type
   * @param object      description object, can be null
   * @param description list with results
   */
  private static void setReturnText(TypeProvider returnType, UtamMethodDescription object,
      List<String> description) {
    String returnStr = object == null ? null : object.returnStr;
    if (returnStr != null) {
      description.add(String.format("@return %s", returnStr));
    } else if (!returnType.isSameType(VOID)) {
      description.add(String.format("@return %s", returnType.getSimpleName()));
    }
  }

  /**
   * Get description as list of strings to wrap into javadoc format
   *
   * @param declaration method declaration
   * @param description object with description or null
   * @return list of strings
   */
  public static List<String> getDescription(MethodDeclaration declaration,
      UtamMethodDescription description) {
    List<String> descriptionLines = new ArrayList<>();
    setText(declaration.getName(), description, descriptionLines);
    setReturnText(declaration.getReturnType(), description, descriptionLines);
    for (MethodParameter parameter : declaration.getParameters()) {
      setParameterText(parameter, descriptionLines);
    }
    if (description != null && description.throwsStr != null) {
      descriptionLines.add(String.format("@throws %s", description.throwsStr));
    }
    if (description != null && description.deprecatedStr != null) {
      descriptionLines.add(String.format("@deprecated %s", description.deprecatedStr));
    }
    // replace any invalid HTML characters with their appropriate encoded entities.
    // special note: iOS class chains contain "*/" in their paths, so this must be
    // escaped for Javadoc generation.
    return descriptionLines.stream()
        .map((s) -> s
            .replace("&", "&amp;")
            .replace("<", "&lt;")
            .replace(">", "&gt;")
            .replace("*/", "*&#47"))
        .collect(Collectors.toList());
  }

  /**
   * check if description has deprecated marker
   * @param description object with description or null
   * @return boolean
   */
  public static boolean isDeprecated(UtamMethodDescription description) {
    return description != null && description.deprecatedStr != null;
  }
}
