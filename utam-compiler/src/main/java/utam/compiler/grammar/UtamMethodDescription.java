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
      "format of the root comment can be either: \n"
          + "1. \"description\" : \"string\" "
          + "or \n"
          + "2. \"description\" : { \"text\" : [\"array of strings\"], \"return\" : \"what method returns\", \"throws\" : \"what method throws\" }";

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
   * Process/deserialize description node at the method level
   *
   * @param descriptionNode Json node
   * @return object with description
   */
  static UtamMethodDescription processMethodDescriptionNode(JsonNode descriptionNode) {
    if (descriptionNode == null || descriptionNode.isNull()) {
      return null;
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
    List<String> res = new ArrayList<>();
    setText(declaration.getName(), description, res);
    setReturnText(declaration.getReturnType(), description, res);
    for (MethodParameter parameter : declaration.getParameters()) {
      setParameterText(parameter, res);
    }
    if (description != null && description.throwsStr != null) {
      res.add(String.format("@throws %s", description.throwsStr));
    }
    return res;
  }
}
