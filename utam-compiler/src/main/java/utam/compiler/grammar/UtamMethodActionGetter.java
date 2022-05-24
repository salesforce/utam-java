/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import static utam.compiler.translator.TranslationUtilities.getElementGetterMethodName;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import java.util.List;
import java.util.stream.Collectors;
import utam.compiler.UtamCompilationError;
import utam.compiler.grammar.UtamMethodActionApply.ApplyOperation;
import utam.compiler.helpers.ActionType;
import utam.compiler.helpers.ElementContext;
import utam.compiler.helpers.ElementContext.ElementType;
import utam.compiler.helpers.MethodContext;
import utam.compiler.helpers.ParametersContext;
import utam.compiler.helpers.ParametersContext.StatementParametersContext;
import utam.compiler.helpers.PrimitiveType;
import utam.compiler.helpers.StatementContext;
import utam.compiler.helpers.TranslationContext;
import utam.compiler.representation.ComposeMethodStatement;
import utam.compiler.representation.ComposeMethodStatement.Operand;
import utam.compiler.representation.MatcherObject;
import utam.core.declarative.representation.MethodDeclaration;
import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.TypeProvider;

/**
 * compose statement with element getter invocation, uses default deserializer
 *
 * @author elizaveta.ivanova
 * @since 236
 */
@JsonDeserialize
class UtamMethodActionGetter extends UtamMethodAction {

  @JsonCreator
  UtamMethodActionGetter(
      @JsonProperty(value = "element") String elementName,
      @JsonProperty(value = "args") JsonNode argsNode,
      @JsonProperty(value = "matcher") JsonNode matcherNode,
      @JsonProperty(value = "returnType") JsonNode returnType,
      @JsonProperty(value = "returnAll") Boolean isReturnList,
      @JsonProperty(value = "chain", defaultValue = "false") boolean isChain) {
    super(elementName, argsNode, matcherNode, returnType, isReturnList, isChain);
  }

  private List<MethodParameter> getForeignElementGetterParameters(TranslationContext context, MethodContext methodContext) {
    String parserContext = String.format("method \"%s\"", methodContext.getName());
    ParametersContext parametersContext = new StatementParametersContext(parserContext,
        context,
        argsNode,
        methodContext);
    ArgumentsProvider provider = new ArgumentsProvider(argsNode, parserContext);
    List<UtamArgument> arguments = provider.getArguments(true);
    arguments
        .stream()
        .map(arg -> arg.asParameter(context, methodContext, parametersContext))
        .forEach(parametersContext::setParameter);
    return parametersContext.getParameters();
  }


  private GetterInvocation getForeignElementGetter(TranslationContext context,
      MethodContext methodContext,
      StatementContext statementContext) {
    String elementGetterMethodName = getElementGetterMethodName(elementName, true);
    List<MethodParameter> parameters = getForeignElementGetterParameters(context, methodContext);
    TypeProvider returnType = statementContext
        .getDeclaredReturnOrDefault(context, methodContext.getDeclaredReturnType(), null);
    if (returnType == null) {
      throw new UtamCompilationError(
          context.getErrorMessage(605, methodContext.getName(), elementName));
    }
    MatcherObject matcher = matcherProvider.apply(context, methodContext);
    return new GetterInvocation(elementGetterMethodName, returnType, parameters, matcher);
  }

  private List<MethodParameter> getElementGetterFromContextParameters(TranslationContext context,
      MethodContext methodContext,
      MethodDeclaration elementGetter,
      List<UtamArgument> arguments) {
    String parserContext = String.format("method \"%s\"", methodContext.getName());
    ParametersContext parametersContext = new StatementParametersContext(parserContext,
        context,
        argsNode,
        methodContext);
    List<MethodParameter> parameters;
    if (!arguments.isEmpty()) {
      arguments
          .stream()
          .map(arg -> arg.asParameter(context, methodContext, parametersContext))
          .forEach(parametersContext::setParameter);
      List<TypeProvider> expectedElementArgsTypes = elementGetter
          .getParameters()
          .stream()
          .map(MethodParameter::getType)
          .collect(Collectors.toList());
      parameters = parametersContext.getParameters(expectedElementArgsTypes);
    } else if (methodContext.getElementUsageTracker().isReusedElement(elementName)) {
      parameters = elementGetter.getParameters();
    } else {
      elementGetter.getParameters().forEach(parametersContext::setParameter);
      parameters = parametersContext.getParameters();
    }
    return parameters;
  }

  private GetterInvocation getElementGetterFromContext(TranslationContext context,
      MethodContext methodContext,
      StatementContext statementContext) {
    String methodName = methodContext.getName();
    String parserContext = String.format("method \"%s\"", methodName);
    ArgumentsProvider provider = new ArgumentsProvider(argsNode, parserContext);
    ElementContext element = provider.getElementArgument(context, elementName);
    element.setElementMethodUsage(context);
    MethodDeclaration elementGetter = element.getElementMethod().getDeclaration();
    boolean isContainer = element.getElementNodeType() == ElementType.CONTAINER;
    if (isContainer && !statementContext.hasDeclaredReturn()) {
      // for container return type is PageObject
      throw new UtamCompilationError(context.getErrorMessage(604, methodName));
    }
    TypeProvider returnType = statementContext.hasDeclaredReturn() ? statementContext
        .getDeclaredStatementReturnOrNull(context) : elementGetter.getReturnType();
    if(statementContext.hasDeclaredReturn()) {
      // if "returnType" is set, check it matches getter return type or matcher
      TypeProvider expectedType = hasMatcher ? PrimitiveType.BOOLEAN : elementGetter.getReturnType();
      if (!expectedType.isSameType(returnType)) {
        String errorMsg = context.getErrorMessage(613, methodName,
            expectedType.getSimpleName(),
            returnType.getSimpleName());
        throw new UtamCompilationError(errorMsg);
      }
    }
    List<UtamArgument> arguments = provider.getArguments(true);
    List<MethodParameter> parameters = getElementGetterFromContextParameters(context, methodContext, elementGetter, arguments);
    String invocationStr = elementGetter.getName();
    methodContext.getElementUsageTracker()
        .setElementUsage(statementContext.getVariableName(), element);
    MatcherObject matcher = matcherProvider.apply(context, methodContext);
    if(matcher != null && !statementContext.hasDeclaredReturn()) {
      matcher.checkMatcherOperand(context, returnType);
    }
    return new GetterInvocation(invocationStr, returnType, parameters, matcher);
  }

  @Override
  ComposeMethodStatement getComposeAction(TranslationContext context,
      MethodContext methodContext, StatementContext statementContext) {
    String methodName = methodContext.getName();
    // first statement can't be marked as chain
    checkFirsStatementCantBeChain(statementContext, methodName);

    Operand operand;
    GetterInvocation getterInvocation;
    if (isChain) {
      checkChainAllowed(statementContext, methodName);
      operand = statementContext.getChainOperand();
      // element from another page objects
      getterInvocation = getForeignElementGetter(context, methodContext, statementContext);
    } else {
      operand = SELF_OPERAND;
      getterInvocation = getElementGetterFromContext(context, methodContext, statementContext);
    }
    ApplyOperation operation = getterInvocation.getOperation(hasMatcher);
    return buildStatement(operand, operation, statementContext);
  }

  private static class GetterInvocation {

    private final String getterMethodName;
    private final TypeProvider getterReturnType;
    private final List<MethodParameter> getterParameters;
    private final MatcherObject matcherObject;

    GetterInvocation(String getterMethodName,
        TypeProvider getterReturnType,
        List<MethodParameter> getterParameters,
        MatcherObject matcherObject) {
      this.getterMethodName = getterMethodName;
      this.getterReturnType = getterReturnType;
      this.getterParameters = getterParameters;
      this.matcherObject = matcherObject;
    }

    ApplyOperation getOperation(boolean hasMatcher) {
      ActionType action = new CustomActionType(getterMethodName, getterReturnType);
      TypeProvider returnType = hasMatcher ? PrimitiveType.BOOLEAN : getterReturnType;
      return new ApplyOperation(action, returnType, getterParameters, matcherObject);
    }
  }
}
