/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import static utam.compiler.helpers.TypeUtilities.VOID;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import utam.compiler.UtamCompilationError;
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

  final static String ERR_FORMAT_ERROR =
      "format of the root comment can be either: \n1. \"description\" : \"string\" "
          + "or "
          + "\n2. \"description\" : { \"text\" : [\"array of strings\"], \"return\" : \"what method returns\", \"throws\" : \"what method throws\" }";

  private final List<String> text = new ArrayList<>();
  private final String returnStr;
  private final String throwsStr;

  /**
   * Initialize object
   *
   * @param text      text describing what method does
   * @param returnStr describes meaning of the returned value. by default will be type of the
   *                  returned value
   * @param throwsStr if method throws an exception, can add description for it
   */
  @JsonCreator
  UtamMethodDescription(
      @JsonProperty(value = "text", required = true) List<String> text,
      @JsonProperty("return") String returnStr,
      @JsonProperty("throws") String throwsStr
  ) {
    if (text != null) {
      this.text.addAll(text);
    }
    this.throwsStr = throwsStr;
    this.returnStr = returnStr;
  }

  /**
   * Initialize empty description object
   */
  public UtamMethodDescription() {
    this(null, null, null);
  }

  /**
   * Process/deserialize description node at the method level
   *
   * @param descriptionNode Json node
   * @return object with description
   */
  static UtamMethodDescription processMethodDescriptionNode(JsonNode descriptionNode) {
    if (descriptionNode == null || descriptionNode.isNull()) {
      return new UtamMethodDescription();
    }
    if (descriptionNode.isTextual()) {
      String value = descriptionNode.textValue();
      return new UtamMethodDescription(Collections.singletonList(value), null, null);
    }
    if (descriptionNode.isObject()) {
      try {
        return new ObjectMapper()
            .readValue(descriptionNode.toString(), UtamMethodDescription.class);
      } catch (IOException e) {
        throw new UtamCompilationError(ERR_FORMAT_ERROR, e);
      }
    }
    throw new UtamCompilationError(ERR_FORMAT_ERROR);
  }

  private static String getParameterText(MethodParameter parameter) {
    if (parameter.isLiteral()) {
      return null;
    }
    final String pattern = String.format("@param %s", parameter.getValue()) + " %s";
    return parameter.getDescription() == null?
        String.format(pattern, parameter.getType().getSimpleName())
        : String.format(pattern, parameter.getDescription());
  }

  private List<String> getText(String methodName) {
    return this.text.isEmpty() ? Collections.singletonList("method " + methodName) : this.text;
  }

  private String getReturnText(TypeProvider returnType) {
    if (this.returnStr != null) {
      return String.format("@return %s", returnStr);
    }
    return returnType.isSameType(VOID) ? null : String.format("@return %s", returnType.getSimpleName());
  }

  /**
   * Get description as list of strings to wrap into javadoc format
   *
   * @param declaration declaration
   * @return list of strings
   */
  public List<String> getDescription(MethodDeclaration declaration) {
    List<String> res = new ArrayList<>(getText(declaration.getName()));
    String returnText = getReturnText(declaration.getReturnType());
    if (returnText != null) {
      res.add(returnText);
    }
    for (MethodParameter parameter : declaration.getParameters()) {
      String parameterText = getParameterText(parameter);
      if (parameterText != null) {
        res.add(parameterText);
      }
    }
    if (throwsStr != null) {
      res.add(String.format("@throws %s", throwsStr));
    }
    return res;
  }
}
