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
import java.util.Objects;
import utam.compiler.grammar.UtamMethodAction.ArgumentsProvider;
import utam.compiler.helpers.MatcherType;
import utam.compiler.helpers.MethodContext;
import utam.compiler.helpers.ParametersContext;
import utam.compiler.helpers.ParametersContext.StatementParametersContext;
import utam.compiler.helpers.TranslationContext;
import utam.compiler.representation.MatcherObject;
import utam.core.declarative.representation.MethodParameter;

/**
 * Matcher is used in compose statements or in element filter
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
   * Translates JsonNode to UtamMatcher and then to MatcherObject
   *
   * @author elizaveta.ivanova
   * @since 240
   */
  static abstract class MatcherProvider {

    final JsonNode matcherNode;

    MatcherProvider(JsonNode node) {
      this.matcherNode = node;
    }

    /**
     * get matcher object
     *
     * @param context context is used for error messages
     * @return matcher object (type and parameters)
     */
    abstract MatcherObject getMatcherObject(TranslationContext context);
  }

  /**
   * Matcher provider for an element filter
   *
   * @author elizaveta.ivanova
   * @since 240
   */
  static class ElementFilterMatcherProvider extends MatcherProvider {

    private final String elementName;

    ElementFilterMatcherProvider(JsonNode matcherNode, String elementName) {
      super(matcherNode);
      this.elementName = elementName;
    }

    @Override
    MatcherObject getMatcherObject(TranslationContext context) {
      UtamMatcher matcher = Objects.requireNonNull(JsonDeserializer
          .readNode(matcherNode, UtamMatcher.class, VALIDATION.getErrorMessage(300, elementName)));
      String parserContext = String.format("element \"%s\" matcher", elementName);
      ArgumentsProvider provider = new ArgumentsProvider(matcher.argsNode, parserContext);
      ParametersContext parametersContext = new StatementParametersContext(parserContext, context, null);
      List<UtamArgument> arguments = provider.getArguments(true);
      arguments
          .stream()
          .map(arg -> arg.asParameter(context, null, parametersContext))
          .forEach(parametersContext::setParameter);
      List<MethodParameter> parameters = parametersContext
          .getParameters(matcher.matcherType.getExpectedParametersTypes());
      return new MatcherObject(matcher.matcherType, parameters, 203, elementName);
    }
  }

  /**
   * Matcher provider for compose statement
   *
   * @author elizaveta.ivanova
   * @since 240
   */
  static class ComposeStatementMatcherProvider extends MatcherProvider {

    private final MethodContext methodContext;

    ComposeStatementMatcherProvider(JsonNode matcherNode, MethodContext methodContext) {
      super(matcherNode);
      this.methodContext = methodContext;
    }

    @Override
    MatcherObject getMatcherObject(TranslationContext context) {
      UtamMatcher matcher = JsonDeserializer
          .readNode(matcherNode, UtamMatcher.class, VALIDATION.getErrorMessage(303, methodContext.getName()));
      String parserContext = String
          .format("method \"%s\" statement matcher", methodContext.getName());
      ArgumentsProvider provider = new ArgumentsProvider(matcher.argsNode, parserContext);
      ParametersContext parametersContext = new StatementParametersContext(parserContext, context, methodContext);
      List<UtamArgument> arguments = provider.getArguments(true);
      arguments
          .stream()
          .map(arg -> arg.asParameter(context, methodContext, parametersContext))
          .forEach(parametersContext::setParameter);
      List<MethodParameter> parameters = parametersContext
          .getParameters(matcher.matcherType.getExpectedParametersTypes());
      return new MatcherObject(matcher.matcherType, parameters, 614, methodContext.getName());
    }
  }
}
