/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.JsonNode;
import java.util.List;
import utam.compiler.UtamCompilerIntermediateError;
import utam.compiler.grammar.UtamMethodAction.ArgumentsProvider;
import utam.compiler.helpers.MatcherType;
import utam.compiler.helpers.MethodContext;
import utam.compiler.helpers.ParametersContext;
import utam.compiler.helpers.ParametersContext.StatementParametersContext;
import utam.compiler.helpers.TranslationContext;
import utam.core.declarative.representation.MethodParameter;

/**
 * result matcher is used in compose statements or in element filter
 *
 * @author elizaveta.ivanova
 * @since 232
 */
class UtamMatcher {

  private final JsonNode argsNode;
  private final MatcherType matcherType;

  @JsonCreator
  UtamMatcher(
      @JsonProperty(value = "type", required = true) MatcherType matcherType,
      @JsonProperty(value = "args") JsonNode args) {
    this.argsNode = args;
    this.matcherType = matcherType;
  }

  /**
   * process node
   *
   * @param node          json node
   * @param parserContext parser context
   * @return object of selector
   */
  static UtamMatcher processMatcherNode(JsonNode node, String parserContext) {
    return JsonDeserializer.readNode(node,
        UtamMatcher.class,
        cause -> new UtamCompilerIntermediateError(cause, node, "UEF000", parserContext,
            cause.getMessage()));
  }

  /**
   * get parameters for a matcher inside element's filter
   *
   * @param context     translation context
   * @param elementName element name
   * @return list of parameters
   */
  List<MethodParameter> getParameters(TranslationContext context, String elementName) {
    String parserContext = String.format("element \"%s\" matcher", elementName);
    ArgumentsProvider provider = new ArgumentsProvider(argsNode, parserContext);
    ParametersContext parametersContext = new StatementParametersContext(parserContext, context,
        argsNode, null);
    List<UtamArgument> arguments = provider.getArguments(true);
    arguments
        .stream()
        .map(arg -> arg.asParameter(context, null, parametersContext))
        .forEach(parametersContext::setParameter);
    return parametersContext.getParameters(getMatcherType().getExpectedParametersTypes());
  }

  /**
   * get parameters for a matcher inside element's filter
   *
   * @param context       get parameters for a matcher inside method statement
   * @param methodContext method context
   * @return list of parameters
   */
  List<MethodParameter> getParameters(TranslationContext context, MethodContext methodContext) {
    String parserContext = String
        .format("method \"%s\" statement matcher", methodContext.getName());
    ArgumentsProvider provider = new ArgumentsProvider(argsNode, parserContext);
    ParametersContext parametersContext = new StatementParametersContext(parserContext, context,
        argsNode, methodContext);
    List<UtamArgument> arguments = provider.getArguments(true);
    arguments
        .stream()
        .map(arg -> arg.asParameter(context, methodContext, parametersContext))
        .forEach(parametersContext::setParameter);
    return parametersContext.getParameters(getMatcherType().getExpectedParametersTypes());
  }

  /**
   * get matcher type
   *
   * @return enum
   */
  MatcherType getMatcherType() {
    return this.matcherType;
  }
}
