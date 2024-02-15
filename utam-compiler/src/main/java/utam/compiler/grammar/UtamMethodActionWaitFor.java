/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import static utam.compiler.diagnostics.ValidationUtilities.VALIDATION;
import static utam.compiler.helpers.TypeUtilities.VOID;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import utam.compiler.UtamCompilationError;
import utam.compiler.grammar.UtamArgument.UtamArgumentPredicate;
import utam.compiler.grammar.UtamMethodActionApply.ApplyOperation;
import utam.compiler.helpers.*;
import utam.compiler.helpers.ParametersContext.StatementParametersContext;
import utam.compiler.representation.ComposeMethodStatement;
import utam.compiler.representation.ComposeMethodStatement.Operand;
import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.TypeProvider;

/**
 * mapping for element/apply statement, uses default deserializer
 *
 * @author elizaveta.ivanova
 * @since 236
 */
@JsonDeserialize
class UtamMethodActionWaitFor extends UtamMethodAction {

  static final String WAIT_FOR = "waitFor";

  @JsonCreator
  UtamMethodActionWaitFor(
      @JsonProperty(value = "element") String elementName,
      @JsonProperty(value = "apply") String apply, // always waitFor
      @JsonProperty(value = "args") JsonNode argsNode,
      @JsonProperty(value = "matcher") JsonNode matcherNode,
      @JsonProperty(value = "chain", defaultValue = "false") boolean isChain) {
    super(elementName, argsNode, matcherNode, null, null, isChain);
  }

  @Override
  Statement getStatement(
      TranslationContext context, MethodContext methodContext, StatementContext statementContext) {
    chainValidations(statementContext, methodContext.getName());
    if (statementContext.isInsidePredicate()) {
      String message = VALIDATION.getErrorMessage(615, methodContext.getName());
      throw new UtamCompilationError(message);
    }
    return new PredicateStatement(context, methodContext, statementContext);
  }

  /** information about applied action with a predicate */
  static class OperationWithPredicate extends ApplyOperation {

    final List<String> predicateCode = new ArrayList<>();
    private final List<TypeProvider> classImports = new ArrayList<>();

    private final String nullableErrorMessage;

    OperationWithPredicate(
        ActionType action,
        String nullableErrorMessage,
        TypeProvider returnType,
        List<ComposeMethodStatement> predicate) {
      super(action, returnType, new ArrayList<>(), null);
      for (ComposeMethodStatement statement : predicate) {
        predicateCode.addAll(statement.getCodeLines());
        ParameterUtils.setImports(classImports, statement.getClassImports());
        getActionParameters().addAll(statement.getParameters());
      }
      this.nullableErrorMessage = nullableErrorMessage == null ? "" : ", " + nullableErrorMessage;
    }

    @Override
    protected String getInvocationString() {
      String wrappedCode =
          predicateCode.stream()
              // predicate code may contain if statement, hence does not need ";"
              .map(str -> str + (str.endsWith("}") ? "" : ";"))
              .collect(Collectors.joining("\n"));
      return String.format("waitFor(() -> {\n%s\n}%s)", wrappedCode, nullableErrorMessage);
    }

    @Override
    protected List<TypeProvider> getAddedClassImports() {
      return classImports;
    }
  }

  /**
   * Non static class that transforms JSON to a predicate statement
   *
   * @author elizaveta.ivanova
   * @since 240
   */
  class PredicateStatement extends Statement {

    PredicateStatement(
        TranslationContext context,
        MethodContext methodContext,
        StatementContext statementContext) {
      super(context, methodContext, statementContext);
    }

    private void checkFunctionParameter(String contextString, List<MethodParameter> parameters) {
      if (parameters.isEmpty() || parameters.size() > 2) {
        String message =
            VALIDATION.getErrorMessage(
                108, contextString, "1 or 2", String.valueOf(parameters.size()));
        throw new UtamCompilationError(argsNode, message);
      }
      // check that first parameter is function
      MethodParameter parameter = parameters.get(0);
      if (parameter != null) {
        String actualType = parameter.getType().getSimpleName();
        String parameterValue = parameter.getValue();
        String message =
            VALIDATION.getErrorMessage(109, contextString, parameterValue, "function", actualType);
        throw new UtamCompilationError(argsNode, message);
      }
      // check that message is string literal
      if (parameters.size() == 2) {
        parameter = parameters.get(1);
        if (!parameter.isLiteral() || !parameter.getType().isSameType(PrimitiveType.STRING)) {
          String actualType = parameter.getType().getSimpleName();
          String message =
              VALIDATION.getErrorMessage(
                  109, contextString, parameter.getValue(), "literal string", actualType);
          throw new UtamCompilationError(argsNode, message);
        }
      }
    }

    @Override
    ApplyOperation getApplyOperation() {
      String methodName = methodContext.getName();
      String parserContext = String.format("method \"%s\"", methodName);
      TypeProvider defaultReturnType =
          statementContext.isLastStatement()
              ? methodContext.getDeclaredReturnType().getReturnTypeOrDefault(context, VOID)
              : VOID;
      TypeProvider declaredStatementReturnType =
          statementContext.getDeclaredReturnOrDefault(
              context, methodContext.getDeclaredReturnType(), defaultReturnType);
      ActionType action = new CustomActionType(WAIT_FOR, declaredStatementReturnType);
      methodContext.enterPredicateContext();
      ArgumentsProvider argumentsProvider = new ArgumentsProvider(argsNode, parserContext);
      ParametersContext parametersContext =
          new StatementParametersContext(parserContext, context, methodContext);
      List<UtamArgument> arguments = getArguments(argumentsProvider);
      List<MethodParameter> parameters =
          arguments.stream()
              .map(arg -> arg.asParameter(context, methodContext, parametersContext))
              .collect(Collectors.toList());
      checkFunctionParameter(parserContext, parameters);
      List<ComposeMethodStatement> predicateStatements =
          arguments.get(0).getPredicate(context, methodContext);
      methodContext.exitPredicateContext();
      String nullableErrorMessage = parameters.size() == 2 ? parameters.get(1).getValue() : null;
      TypeProvider operationReturnType =
          predicateStatements.get(predicateStatements.size() - 1).getReturnType();
      return new OperationWithPredicate(
          action, nullableErrorMessage, operationReturnType, predicateStatements);
    }

    // we override this method for waitForElement
    List<UtamArgument> getArguments(ArgumentsProvider argumentsProvider) {
      return argumentsProvider.getArguments(UtamArgument.ArgsValidationMode.PREDICATE);
    }

    @Override
    Operand getOperand() {
      return SELF_OPERAND;
    }
  }

  /**
   * Compose statement to wait for element by invoking its getter
   *
   * @author elizaveta.ivanova
   * @since 248
   */
  static class UtamMethodActionWaitForElement extends UtamMethodActionWaitFor {

    private final List<UtamArgument> args;
    private final boolean isNoArgsAllowed;
    private final String elementName;

    UtamMethodActionWaitForElement(String elementName, boolean isNoArgsAllowed) {
      super(null, "waitFor", null, null, false);
      UtamMethodAction getter =
          new UtamMethodActionGetter(elementName, null, null, null, null, false);
      UtamArgument argument = new UtamArgumentPredicate(getter);
      this.args = Collections.singletonList(argument);
      this.isNoArgsAllowed = isNoArgsAllowed;
      this.elementName = elementName;
    }

    @Override
    Statement getStatement(
        TranslationContext context,
        MethodContext methodContext,
        StatementContext statementContext) {
      // validation is performed here as args are not available at the time of construction
      if (isNoArgsAllowed
          && (!context
              .getElement(this.elementName)
              .getElementMethod()
              .getDeclaration()
              .getParameters()
              .isEmpty())) {
        String message = VALIDATION.getErrorMessage(206, this.elementName);
        throw new UtamCompilationError(message);
      }

      // instead of returning provided args, we always infer them from element
      return new PredicateStatement(context, methodContext, statementContext) {
        @Override
        List<UtamArgument> getArguments(ArgumentsProvider argumentsProvider) {
          return args;
        }
      };
    }
  }
}
