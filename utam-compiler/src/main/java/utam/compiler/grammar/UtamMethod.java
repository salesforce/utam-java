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
import static utam.compiler.grammar.UtamArgument.processArgsNode;
import static utam.compiler.grammar.UtamMethodDescription.processMethodDescriptionNode;

import com.fasterxml.jackson.databind.JsonNode;
import java.util.ArrayList;
import java.util.List;
import utam.compiler.helpers.MethodContext;
import utam.compiler.helpers.ParametersContext;
import utam.compiler.helpers.TranslationContext;
import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.PageObjectMethod;

/**
 * base class for public method declared at PO level (interface or compose)
 *
 * @author elizaveta.ivanova
 * @since 228
 */
abstract class UtamMethod {

  final String name;
  final UtamMethodDescription description;
  private final List<UtamArgument> arguments;

  UtamMethod(String name, JsonNode descriptionNode, JsonNode argsNode) {
    this.name = name;
    VALIDATION.validateNotEmptyString(name, "method", "name");
    String parserContext = String.format("method \"%s\"", name);
    this.description = processMethodDescriptionNode(descriptionNode, parserContext);
    this.arguments =
        processArgsNode(
            argsNode, parserContext, UtamArgument.ArgsValidationMode.LITERAL_NOT_ALLOWED);
  }

  /**
   * Constructor is used to add custom compose method, for example waitForElement
   *
   * @param name name of the method
   * @param description description text
   */
  UtamMethod(String name, UtamMethodDescription description) {
    this.name = name;
    this.description = description;
    this.arguments = new ArrayList<>();
  }

  /**
   * process json node "methods"
   *
   * @param methodsNode json node
   * @param isAbstract true if page object is an interface
   * @return list of declared methods
   */
  static List<UtamMethod> processMethodsNode(JsonNode methodsNode, boolean isAbstract) {
    List<UtamMethod> methods = new ArrayList<>();
    VALIDATION.validateOptionalNotEmptyArray(methodsNode, "page object root", "methods");
    if (isEmptyNode(methodsNode)) {
      return methods;
    }
    Class<? extends UtamMethod> methodType =
        isAbstract ? UtamInterfaceMethod.class : UtamComposeMethod.class;
    Integer errCode = isAbstract ? 400 : 500;
    for (JsonNode methodNode : methodsNode) {
      String name =
          VALIDATION.validateNotNullOrEmptyString(methodNode.get("name"), "method", "name");
      UtamMethod method =
          readNode(methodNode, methodType, VALIDATION.getErrorMessage(errCode, name));
      methods.add(method);
    }
    return methods;
  }

  final boolean hasMethodLevelArgs() {
    return !arguments.isEmpty();
  }

  /**
   * Information for linting to check if description is set
   *
   * @return boolean
   */
  final boolean hasDescription() {
    return !description.isEmpty();
  }

  /**
   * compile declared method
   *
   * @param context translation context
   * @return method instance
   */
  abstract PageObjectMethod getMethod(TranslationContext context);

  final void setMethodLevelParameters(TranslationContext context, MethodContext methodContext) {
    ParametersContext parametersContext = methodContext.getParametersContext();
    arguments.forEach(
        arg -> {
          MethodParameter parameter = arg.asParameter(context, methodContext, parametersContext);
          parametersContext.setParameter(parameter);
        });
  }
}
