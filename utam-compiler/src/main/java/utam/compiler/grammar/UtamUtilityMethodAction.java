/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import static utam.compiler.diagnostics.ValidationUtilities.VALIDATION;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.JsonNode;
import java.util.List;
import utam.compiler.grammar.UtamMethodAction.ArgumentsProvider;
import utam.compiler.helpers.MethodContext;
import utam.compiler.helpers.ParametersContext;
import utam.compiler.helpers.ParametersContext.StatementParametersContext;
import utam.compiler.helpers.TranslationContext;
import utam.core.declarative.representation.MethodParameter;

/**
 * Imperative extension compose statement mapping. This class creates an entity from a JSON object
 * that represents a utility in a compose statement. It's a 1 to 1 mapping so every utility declared
 * in a compose method will create one UtamUtilityMethodAction object.
 *
 * @author olivier.martin
 * @since 234
 */
class UtamUtilityMethodAction {

  private final String externalClassPath;
  private final String methodName;
  private final JsonNode argsNode;

  /**
   * Creates a utility method action object by deserializing JSON utility compose statements. This
   * object holds the information necessary to generate the executable compose statement once
   * compiled.
   *
   * @param externalClassPath class path of the imperative extension
   * @param methodName name of the static method declared in the class that needs to be run
   * @param argsNode arguments that needs to be passed to the method specified in methodName
   */
  @JsonCreator
  UtamUtilityMethodAction(
      @JsonProperty(value = "type", required = true) String externalClassPath,
      @JsonProperty(value = "invoke", required = true) String methodName,
      @JsonProperty(value = "args") JsonNode argsNode) {
    this.externalClassPath = externalClassPath;
    this.methodName = methodName;
    this.argsNode = argsNode;
  }

  /**
   * get declared parameters from utility invocation
   *
   * @param context translation context
   * @param methodContext method context
   * @return list of parameters
   */
  List<MethodParameter> getParameters(TranslationContext context, MethodContext methodContext) {
    String parserContext = String.format("method \"%s\"", methodContext.getName());
    VALIDATION.validateNotEmptyString(methodName, parserContext, "name");
    VALIDATION.validateNotEmptyString(externalClassPath, parserContext, "type");
    ParametersContext parametersContext =
        new StatementParametersContext(parserContext, context, methodContext);
    ArgumentsProvider argumentsProvider = new ArgumentsProvider(argsNode, parserContext);
    List<UtamArgument> arguments =
        argumentsProvider.getArguments(UtamArgument.ArgsValidationMode.LITERAL_ALLOWED);
    arguments.stream()
        .map(arg -> arg.asParameter(context, methodContext, parametersContext))
        .forEach(parametersContext::setParameter);
    return parametersContext.getParameters();
  }

  /**
   * @return the utility static method name to run
   */
  String getMethodName() {
    return methodName;
  }

  /**
   * @return the utitility class path
   */
  String getExternalClassPath() {
    return externalClassPath;
  }
}
