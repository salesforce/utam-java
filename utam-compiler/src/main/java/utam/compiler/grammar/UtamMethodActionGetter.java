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
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import utam.compiler.UtamCompilationError;
import utam.compiler.grammar.ArgsProcessor.ArgsProcessorWithExpectedTypes;
import utam.compiler.grammar.UtamMethodActionApply.ApplyOperation;
import utam.compiler.helpers.ActionType;
import utam.compiler.helpers.ElementContext;
import utam.compiler.helpers.ElementContext.ElementType;
import utam.compiler.helpers.MethodContext;
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

  static final String ERR_RETURN_TYPE_CANT_BE_INFERRED = "method '%s': return type can't be inferred for element '%s', please provide return type";
  static final String ERR_CONTAINER_INVOCATION_NEEDS_RETURN_TYPE = "method '%s': first statement invokes container method and needs return type";

  @JsonCreator
  UtamMethodActionGetter(
      @JsonProperty(value = "element") String elementName,
      @JsonProperty(value = "args") UtamArgument[] args,
      @JsonProperty(value = "matcher") UtamMatcher matcher,
      @JsonProperty(value = "returnType") JsonNode returnType,
      @JsonProperty(value = "returnAll") Boolean isReturnList,
      @JsonProperty(value = "chain", defaultValue = "false") boolean isChain) {
    super(elementName, null, null, args, matcher, returnType, isReturnList, isChain);
  }

  private GetterInvocation getForeignElementGetter(TranslationContext context,
      MethodContext methodContext,
      StatementContext statementContext) {
    String methodName = getElementGetterMethodName(elementName, true);
    ArgsProcessor argsProcessor = new ArgsProcessor(context, methodContext,
        p -> methodContext.setStatementParameter(p, statementContext));
    List<MethodParameter> getterParameters;
    if (args != null) {
      getterParameters = argsProcessor.getParameters(args);
    } else {
      getterParameters = new ArrayList<>();
    }
    TypeProvider returnType = statementContext
        .getDeclaredReturnOrDefault(context, methodContext.getDeclaredReturnType(), null);
    if (returnType == null) {
      throw new UtamCompilationError(
          String.format(ERR_RETURN_TYPE_CANT_BE_INFERRED, methodContext.getName(), elementName));
    }
    return new GetterInvocation(methodName, returnType, getterParameters);
  }

  private GetterInvocation getElementGetterFromContext(TranslationContext context,
      MethodContext methodContext,
      StatementContext statementContext) {
    ElementContext element = context.getElement(elementName);
    element.setElementMethodUsage(context);
    MethodDeclaration elementGetter = context.getElement(elementName).getElementMethod()
        .getDeclaration();
    boolean isContainer = element.getElementNodeType() == ElementType.CONTAINER;
    if (isContainer && !statementContext.hasDeclaredReturn()) {
      // for container return type is PageObject
      throw new UtamCompilationError(
          String.format(ERR_CONTAINER_INVOCATION_NEEDS_RETURN_TYPE, methodContext.getName()));
    }
    TypeProvider returnType = statementContext.hasDeclaredReturn() ? statementContext
        .getDeclaredStatementReturnOrNull(context) : elementGetter.getReturnType();
    if (!isContainer && matcher == null) {
      checkDefinedReturnType(elementGetter.getReturnType(), returnType, methodContext.getName());
    }
    List<MethodParameter> getterParameters;
    String invocationStr = elementGetter.getName();
    if (args != null) {
      List<TypeProvider> expectedElementArgsTypes = elementGetter
          .getParameters()
          .stream()
          .map(MethodParameter::getType)
          .collect(Collectors.toList());
      getterParameters = new ArgsProcessorWithExpectedTypes(context, methodContext,
          expectedElementArgsTypes, p -> methodContext.setStatementParameter(p, statementContext))
          .getParameters(args);
    } else if (methodContext.isReusedElement(elementName)) {
      getterParameters = elementGetter.getParameters(); // element parameters already added
    } else {
      getterParameters = elementGetter.getParameters()
          .stream()
          .map(p -> methodContext.setStatementParameter(p, statementContext))
          .collect(Collectors.toList());
    }
    methodContext.setElementUsage(statementContext.getVariableName(), element);
    return new GetterInvocation(invocationStr, returnType, getterParameters);
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
    Operation operation = getterInvocation.getOperation(matcher);
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
