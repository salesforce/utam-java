/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import static utam.compiler.grammar.JsonDeserializer.readNode;
import static utam.compiler.grammar.UtamArgument.processArgsNode;
import static utam.compiler.grammar.UtamMethodDescription.processMethodDescriptionNode;

import com.fasterxml.jackson.databind.JsonNode;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;
import utam.compiler.UtamCompilerIntermediateError;
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
  final JsonNode argsNode;

  UtamMethod(String name, JsonNode descriptionNode, JsonNode argsNode) {
    this.name = name;
    String validationContext = String.format("method \"%s\"", name);
    this.description = processMethodDescriptionNode(descriptionNode, validationContext);
    this.argsNode = argsNode;
  }

  /**
   * process json node "methods"
   *
   * @param methodsNode json node
   * @param isAbstract  true if page object is an interface
   * @return list of declared methods
   */
  static List<UtamMethod> processMethodsNode(JsonNode methodsNode, boolean isAbstract) {
    List<UtamMethod> methods = new ArrayList<>();
    if (methodsNode == null || methodsNode.isNull()) {
      return methods;
    }
    if (!methodsNode.isArray()) {
      throw new UtamCompilerIntermediateError(methodsNode, 12, "page object root", "methods");
    }
    Class<? extends UtamMethod> methodType =
        isAbstract ? UtamInterfaceMethod.class : UtamComposeMethod.class;
    Integer errCode = isAbstract? 400 : 500;
    Function<Exception, RuntimeException> parserErrorWrapper = causeErr -> new UtamCompilerIntermediateError(
        causeErr, methodsNode, errCode, causeErr.getMessage());
    for (JsonNode methodNode : methodsNode) {
      UtamMethod method = readNode(methodNode, methodType, parserErrorWrapper);
      methods.add(method);
    }
    return methods;
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
    String parserContext = String.format("method \"%s\"", name);
    List<UtamArgument> arguments = processArgsNode(argsNode, parserContext, false);
    arguments.forEach(arg -> {
      MethodParameter parameter = arg.asParameter(context, methodContext, parametersContext);
      parametersContext.setParameter(parameter);
    });
  }
}
