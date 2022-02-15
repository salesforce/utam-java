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
import utam.compiler.representation.ComposeMethodStatement.Operation;
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

  private GetterInvocation getForeignElementGetter(TranslationContext context,
      MethodContext methodContext,
      StatementContext statementContext) {
    String parserContext = String.format("method \"%s\"", methodContext.getName());
    String elementGetterMethodName = getElementGetterMethodName(elementName, true);
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
    List<MethodParameter> parameters = parametersContext.getParameters();
    TypeProvider returnType = statementContext
        .getDeclaredReturnOrDefault(context, methodContext.getDeclaredReturnType(), null);
    if (returnType == null) {
      throw new UtamCompilationError(
          context.getErrorMessage("UMA005", methodContext.getName(), elementName));
    }
    return new GetterInvocation(elementGetterMethodName, returnType, parameters);
  }

  private GetterInvocation getElementGetterFromContext(TranslationContext context,
      MethodContext methodContext,
      StatementContext statementContext) {
    String methodName = methodContext.getName();
    String parserContext = String.format("method \"%s\"", methodName);
    ParametersContext parametersContext = new StatementParametersContext(parserContext,
        context,
        argsNode,
        methodContext);
    ArgumentsProvider provider = new ArgumentsProvider(argsNode, parserContext);
    ElementContext element = provider.getElementArgument(context, elementName);
    element.setElementMethodUsage(context);
    MethodDeclaration elementGetter = element.getElementMethod().getDeclaration();
    boolean isContainer = element.getElementNodeType() == ElementType.CONTAINER;
    if (isContainer && !statementContext.hasDeclaredReturn()) {
      // for container return type is PageObject
      throw new UtamCompilationError(context.getErrorMessage("UMA004", methodName));
    }
    TypeProvider returnType = statementContext.hasDeclaredReturn() ? statementContext
        .getDeclaredStatementReturnOrNull(context) : elementGetter.getReturnType();
    if (!isContainer && !hasMatcher) {
      checkDefinedReturnType(elementGetter.getReturnType(), returnType, methodContext.getName());
    }
    List<UtamArgument> arguments = provider.getArguments(true);
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
    String invocationStr = elementGetter.getName();
    methodContext.getElementUsageTracker()
        .setElementUsage(statementContext.getVariableName(), element);
    return new GetterInvocation(invocationStr, returnType, parameters);
  }

  @Override
  ComposeMethodStatement getComposeAction(TranslationContext context,
      MethodContext methodContext, StatementContext statementContext) {
    String methodName = methodContext.getName();
    String validationContextStr = String.format("method '%s'", methodName);
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

    Operation operation = getterInvocation.getOperation(getMatcher(validationContextStr));
    checkMatcher(getterInvocation.getterReturnType, validationContextStr);
    return buildStatement(operand, operation, context, methodContext, statementContext);
  }

  private static class GetterInvocation {

    private final String getterMethodName;
    private final TypeProvider getterReturnType;
    private final List<MethodParameter> getterParameters;

    GetterInvocation(String getterMethodName,
        TypeProvider getterReturnType,
        List<MethodParameter> getterParameters) {
      this.getterMethodName = getterMethodName;
      this.getterReturnType = getterReturnType;
      this.getterParameters = getterParameters;
    }

    Operation getOperation(UtamMatcher matcher) {
      ActionType action = new CustomActionType(getterMethodName, getterReturnType);
      TypeProvider returnType = matcher == null ? getterReturnType : PrimitiveType.BOOLEAN;
      return new ApplyOperation(action, returnType, getterParameters);
    }
  }
}
