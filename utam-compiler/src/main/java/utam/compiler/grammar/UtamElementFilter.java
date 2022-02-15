/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import static utam.compiler.helpers.BasicElementActionType.getActionType;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.JsonNode;
import java.util.List;
import java.util.function.Function;
import utam.compiler.UtamCompilationError;
import utam.compiler.UtamCompilerIntermediateError;
import utam.compiler.grammar.UtamMethodAction.ArgumentsProvider;
import utam.compiler.helpers.ActionType;
import utam.compiler.helpers.ParametersContext;
import utam.compiler.helpers.ParametersContext.StatementParametersContext;
import utam.compiler.helpers.TranslationContext;
import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.TypeProvider;

/**
 * @author elizaveta.ivanova
 * @since 228
 */
final class UtamElementFilter {

  final String applyMethod;
  private final JsonNode argsNode;
  private final Function<String, UtamMatcher> matcherProvider;
  private final boolean isFindFirst;
  private List<MethodParameter> matcherParameters;
  private List<MethodParameter> applyMethodParameters;

  @JsonCreator
  UtamElementFilter(
      @JsonProperty(value = "apply", required = true) String apply,
      @JsonProperty(value = "args") JsonNode argsNode,
      @JsonProperty(value = "matcher", required = true) JsonNode matcherNode,
      @JsonProperty(value = "findFirst", defaultValue = "true") boolean isFindFirst) {
    this.argsNode = argsNode;
    this.matcherProvider = str -> UtamMatcher.processMatcherNode(matcherNode, str);
    this.applyMethod = apply;
    this.isFindFirst = isFindFirst;
  }

  /**
   * process node
   *
   * @param node        json node
   * @param elementName element for context
   * @return object of selector
   */
  static UtamElementFilter processFilterNode(JsonNode node, String elementName) {
    return JsonDeserializer.readNode(node,
        UtamElementFilter.class,
        cause -> new UtamCompilerIntermediateError(cause, node, "UEF000", elementName,
            cause.getMessage()));
  }

  UtamMatcher setElementFilter(TranslationContext context, UtamElement.Type elementNodeType,
      TypeProvider elementType, String elementName) {
    String parserContext = String.format("element '%s' filter", elementName);
    ArgumentsProvider provider = new ArgumentsProvider(argsNode, parserContext);
    ParametersContext parametersContext = new StatementParametersContext(parserContext, context,
        argsNode, null);
    List<UtamArgument> arguments = provider.getArguments(true);
    arguments
        .stream()
        .map(arg -> arg.asParameter(context, null, parametersContext))
        .forEach(parametersContext::setParameter);
    UtamMatcher matcher = matcherProvider.apply(parserContext);
    if (elementNodeType == UtamElement.Type.BASIC) {
      ActionType actionType = getActionType(this.applyMethod, elementType);
      if (actionType == null) {
        String message = context.getErrorMessage("UEF001", elementName, this.applyMethod);
        throw new UtamCompilationError(message);
      }
      matcher.getMatcherType().checkOperandForMatcher(actionType.getReturnType(), parserContext);
      List<TypeProvider> expectedArgsTypes = actionType
          .getParametersTypes(parserContext, arguments.size());
      this.applyMethodParameters = parametersContext.getParameters(expectedArgsTypes);
    } else {
      this.applyMethodParameters = parametersContext.getParameters();
    }
    this.matcherParameters = matcher.getParameters(context, elementName);
    return matcher;
  }

  List<MethodParameter> getApplyMethodParameters() {
    return this.applyMethodParameters;
  }

  List<MethodParameter> getMatcherParameters() {
    return this.matcherParameters;
  }

  boolean getFindFirst() {
    return this.isFindFirst;
  }
}
