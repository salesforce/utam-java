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
import static utam.compiler.helpers.BasicElementActionType.getActionType;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.JsonNode;
import java.util.List;
import utam.compiler.grammar.UtamMatcher.ElementFilterMatcherProvider;
import utam.compiler.grammar.UtamMethodAction.ArgumentsProvider;
import utam.compiler.helpers.ActionType;
import utam.compiler.helpers.ParametersContext;
import utam.compiler.helpers.ParametersContext.StatementParametersContext;
import utam.compiler.helpers.TranslationContext;
import utam.compiler.representation.MatcherObject;
import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.TypeProvider;

/**
 * @author elizaveta.ivanova
 * @since 228
 */
final class UtamElementFilter {

  final String applyMethod;
  private final JsonNode argsNode;
  private final JsonNode matcherNode;
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
    this.matcherNode = matcherNode;
    this.applyMethod = apply;
    this.isFindFirst = isFindFirst;
  }

  /**
   * process node
   *
   * @param node json node
   * @param elementName element for context
   * @return object of selector
   */
  static UtamElementFilter processFilterNode(JsonNode node, String elementName) {
    return readNode(node, UtamElementFilter.class, VALIDATION.getErrorMessage(300, elementName));
  }

  MatcherObject setElementFilter(
      TranslationContext context,
      TypeProvider elementType,
      String elementName,
      boolean isBasicElement) {
    String parserContext = String.format("element '%s' filter", elementName);
    VALIDATION.validateNotEmptyString(this.applyMethod, parserContext, "apply");
    ArgumentsProvider provider = new ArgumentsProvider(argsNode, parserContext);
    ParametersContext parametersContext =
        new StatementParametersContext(parserContext, context, null);
    List<UtamArgument> arguments =
        provider.getArguments(UtamArgument.ArgsValidationMode.LITERAL_ALLOWED);
    arguments.stream()
        .map(arg -> arg.asParameter(context, null, parametersContext))
        .forEach(parametersContext::setParameter);
    VALIDATION.validateNotNullObject(matcherNode, parserContext, "matcher");
    MatcherObject matcher =
        new ElementFilterMatcherProvider(matcherNode, elementName).getMatcherObject(context);
    if (isBasicElement) {
      ActionType actionType = getActionType(this.applyMethod, elementType, elementName, null);
      matcher.checkMatcherOperand(actionType.getReturnType());
      List<TypeProvider> expectedArgsTypes =
          actionType.getParametersTypes(parserContext, arguments.size());
      this.applyMethodParameters = parametersContext.getParameters(expectedArgsTypes);
    } else {
      this.applyMethodParameters = parametersContext.getParameters();
    }
    this.matcherParameters = matcher.getParameters();
    return matcher;
  }

  List<MethodParameter> getApplyMethodParameters() {
    return this.applyMethodParameters;
  }

  List<MethodParameter> getMatcherParameters() {
    return this.matcherParameters;
  }

  boolean isFindFirst() {
    return this.isFindFirst;
  }
}
