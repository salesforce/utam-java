/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import static utam.compiler.diagnostics.ValidationUtilities.VALIDATION;
import static utam.compiler.grammar.JsonDeserializer.readNode;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.JsonNode;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import utam.compiler.UtamCompilationError;
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

  private static final String SUPPORTED_MATCHER_VALUES =
      Stream.of(MatcherType.values()).map(Enum::name).collect(Collectors.joining(", "));
  private final JsonNode argsNode;
  private final String matcherType;

  @JsonCreator
  UtamMatcher(
      @JsonProperty(value = "type") String matcherType,
      @JsonProperty(value = "args") JsonNode args) {
    this.argsNode = args;
    this.matcherType = matcherType;
  }

  private MatcherType getMatcherType(JsonNode node, String parserContext) {
    try {
      return MatcherType.valueOf(matcherType);
    } catch (Exception e) {
      throw new UtamCompilationError(
          node,
          VALIDATION.getErrorMessage(1201, parserContext, matcherType, SUPPORTED_MATCHER_VALUES));
    }
  }

  /**
   * Translates JsonNode to UtamMatcher and then to MatcherObject
   *
   * @author elizaveta.ivanova
   * @since 240
   */
  abstract static class MatcherProvider {

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
      String filterContext = String.format("element \"%s\" filter", elementName);
      UtamMatcher matcher =
          readNode(matcherNode, UtamMatcher.class, VALIDATION.getErrorMessage(1200, filterContext));
      MatcherType matcherType = matcher.getMatcherType(matcherNode, filterContext);
      String matcherContext = String.format("element \"%s\" filter matcher", elementName);
      ArgumentsProvider provider = new ArgumentsProvider(matcher.argsNode, matcherContext);
      ParametersContext parametersContext =
          new StatementParametersContext(matcherContext, context, null);
      List<UtamArgument> arguments =
          provider.getArguments(UtamArgument.ArgsValidationMode.LITERAL_ALLOWED);
      arguments.stream()
          .map(arg -> arg.asParameter(context, null, parametersContext))
          .forEach(parametersContext::setParameter);
      List<MethodParameter> parameters =
          parametersContext.getParameters(matcherType.getExpectedParametersTypes());
      return new MatcherObject(matcherType, parameters, filterContext);
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
      String statementContext = String.format("method \"%s\" statement", methodContext.getName());
      UtamMatcher matcher =
          readNode(
              matcherNode, UtamMatcher.class, VALIDATION.getErrorMessage(1200, statementContext));
      MatcherType matcherType = matcher.getMatcherType(matcherNode, statementContext);
      String matcherContext =
          String.format("method \"%s\" statement matcher", methodContext.getName());
      ArgumentsProvider provider = new ArgumentsProvider(matcher.argsNode, matcherContext);
      ParametersContext parametersContext =
          new StatementParametersContext(matcherContext, context, methodContext);
      List<UtamArgument> arguments =
          provider.getArguments(UtamArgument.ArgsValidationMode.LITERAL_ALLOWED);
      arguments.stream()
          .map(arg -> arg.asParameter(context, methodContext, parametersContext))
          .forEach(parametersContext::setParameter);
      List<MethodParameter> parameters =
          parametersContext.getParameters(matcherType.getExpectedParametersTypes());
      return new MatcherObject(matcherType, parameters, statementContext);
    }
  }
}
