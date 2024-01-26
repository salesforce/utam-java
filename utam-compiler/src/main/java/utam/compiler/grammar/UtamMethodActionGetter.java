/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import static utam.compiler.diagnostics.ValidationUtilities.VALIDATION;
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
import utam.compiler.helpers.MethodContext;
import utam.compiler.helpers.ParametersContext;
import utam.compiler.helpers.ParametersContext.StatementParametersContext;
import utam.compiler.helpers.PrimitiveType;
import utam.compiler.helpers.StatementContext;
import utam.compiler.helpers.TranslationContext;
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

  @Override
  Statement getStatement(
      TranslationContext context, MethodContext methodContext, StatementContext statementContext) {
    String parserContext = String.format("method \"%s\"", methodContext.getName());
    VALIDATION.validateNotEmptyString(elementName, parserContext, "element");
    if (isChain) {
      chainValidations(statementContext, methodContext.getName());
      return new ForeignElementStatement(context, methodContext, statementContext);
    } else {
      ElementContext elementContext = context.getElement(elementName);
      if (elementContext == null) {
        String message = VALIDATION.getErrorMessage(601, methodContext.getName(), elementName);
        throw new UtamCompilationError(message);
      }
      return new SelfElementStatement(context, methodContext, statementContext, elementContext);
    }
  }

  /**
   * Non static class that transforms JSON to statement for "element" invoked from "chain"
   *
   * @author elizaveta.ivanova
   * @since 240
   */
  class ForeignElementStatement extends Statement {

    ForeignElementStatement(
        TranslationContext context,
        MethodContext methodContext,
        StatementContext statementContext) {
      super(context, methodContext, statementContext);
    }

    private List<MethodParameter> getParameters() {
      String parserContext = String.format("method \"%s\"", methodContext.getName());
      ParametersContext parametersContext =
          new StatementParametersContext(parserContext, context, methodContext);
      ArgumentsProvider provider = new ArgumentsProvider(argsNode, parserContext);
      List<UtamArgument> arguments =
          provider.getArguments(UtamArgument.ArgsValidationMode.LITERAL_ALLOWED);
      arguments.stream()
          .map(arg -> arg.asParameter(context, methodContext, parametersContext))
          .forEach(parametersContext::setParameter);
      return parametersContext.getParameters();
    }

    @Override
    ApplyOperation getApplyOperation() {
      String getterMethodName = getElementGetterMethodName(elementName, true);
      List<MethodParameter> parameters = getParameters();
      TypeProvider returnType =
          statementContext.getDeclaredReturnOrDefault(
              context, methodContext.getDeclaredReturnType(), null);
      if (returnType == null) {
        throw new UtamCompilationError(
            VALIDATION.getErrorMessage(605, methodContext.getName(), elementName));
      }
      // matcher parameters should be set after action parameters
      MatcherObject matcher = matcherProvider.apply(context, methodContext);
      ActionType action = new CustomActionType(getterMethodName, returnType);
      return new ApplyOperation(
          action, hasMatcher ? PrimitiveType.BOOLEAN : returnType, parameters, matcher);
    }

    @Override
    Operand getOperand() {
      return statementContext.getChainOperand();
    }
  }

  /**
   * Non static class that transforms JSON to a statement for "element" of same page object
   * (parameters and return can be validated)
   *
   * @author elizaveta.ivanova
   * @since 240
   */
  class SelfElementStatement extends Statement {

    private final ElementContext elementContext;

    SelfElementStatement(
        TranslationContext context,
        MethodContext methodContext,
        StatementContext statementContext,
        ElementContext elementContext) {
      super(context, methodContext, statementContext);
      this.elementContext = elementContext;
    }

    private List<MethodParameter> getParameters() {
      String parserContext = String.format("method \"%s\"", methodContext.getName());
      ArgumentsProvider provider = new ArgumentsProvider(argsNode, parserContext);
      List<UtamArgument> arguments =
          provider.getArguments(UtamArgument.ArgsValidationMode.LITERAL_ALLOWED);
      List<MethodParameter> getterNonLiteralParameters =
          elementContext.getGetterNonLiteralParameters();
      ParametersContext parametersContext =
          new StatementParametersContext(parserContext, context, methodContext);
      List<MethodParameter> parameters;
      if (!arguments.isEmpty()) {
        arguments.stream()
            .map(arg -> arg.asParameter(context, methodContext, parametersContext))
            .forEach(parametersContext::setParameter);
        List<TypeProvider> expectedElementArgsTypes =
            getterNonLiteralParameters.stream()
                .map(MethodParameter::getType)
                .collect(Collectors.toList());
        parameters = parametersContext.getParameters(expectedElementArgsTypes);
      } else if (methodContext.getElementUsageTracker().isReusedElement(elementName)) {
        parameters = getterNonLiteralParameters;
      } else {
        getterNonLiteralParameters.forEach(parametersContext::setParameter);
        parameters = parametersContext.getParameters();
      }
      return parameters;
    }

    @Override
    ApplyOperation getApplyOperation() {
      // mark that element getter is invoked, otherwise private element getter will not be generated
      context.setMethodUsage(elementContext.getElementGetterName());
      String methodName = methodContext.getName();
      MethodDeclaration elementGetter = elementContext.getElementMethod().getDeclaration();
      TypeProvider returnType =
          statementContext.hasDeclaredReturn()
              ? statementContext.getDeclaredStatementReturnOrNull(context)
              : elementGetter.getReturnType();
      if (statementContext.hasDeclaredReturn()) {
        // if "returnType" is set, check it matches getter return type or matcher
        TypeProvider expectedType =
            hasMatcher ? PrimitiveType.BOOLEAN : elementGetter.getReturnType();
        if (!expectedType.isSameType(returnType)) {
          String errorMsg =
              VALIDATION.getErrorMessage(
                  613, methodName, expectedType.getSimpleName(), returnType.getSimpleName());
          throw new UtamCompilationError(errorMsg);
        }
      }
      List<MethodParameter> parameters = getParameters();
      // matcher parameters should be set after action parameters
      MatcherObject matcher = matcherProvider.apply(context, methodContext);
      if (matcher != null && !statementContext.hasDeclaredReturn()) {
        matcher.checkMatcherOperand(returnType);
      }
      ActionType action = new CustomActionType(elementGetter.getName(), returnType);
      // should be after all other lines!
      methodContext
          .getElementUsageTracker()
          .setElementUsage(statementContext.getVariableName(), elementContext);
      return new ApplyOperation(
          action, hasMatcher ? PrimitiveType.BOOLEAN : returnType, parameters, matcher);
    }

    @Override
    Operand getOperand() {
      return SELF_OPERAND;
    }
  }
}
