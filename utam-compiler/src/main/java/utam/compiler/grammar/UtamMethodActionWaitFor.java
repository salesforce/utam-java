/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import static utam.compiler.helpers.TypeUtilities.VOID;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import utam.compiler.UtamCompilationError;
import utam.compiler.grammar.UtamMethodActionApply.ApplyOperation;
import utam.compiler.helpers.ActionType;
import utam.compiler.helpers.MethodContext;
import utam.compiler.helpers.ParameterUtils;
import utam.compiler.helpers.ParametersContext;
import utam.compiler.helpers.ParametersContext.StatementParametersContext;
import utam.compiler.helpers.StatementContext;
import utam.compiler.helpers.TranslationContext;
import utam.compiler.representation.ComposeMethodStatement;
import utam.compiler.representation.ComposeMethodStatement.Operation;
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
  static final String ERR_NESTED_PREDICATE_PROHIBITED = "%s: nested waitFor is not supported";

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
  ComposeMethodStatement getComposeAction(TranslationContext context,
      MethodContext methodContext, StatementContext statementContext) {
    String methodName = methodContext.getName();
    String parserContext = String.format("method \"%s\"", methodName);

    // if statement is marked as a chain, it should be applied to previous result, so "element" is redundant
    checkChainElementRedundant(context, methodName);

    if (statementContext.isInsidePredicate()) {
      throw new UtamCompilationError(
          String.format(ERR_NESTED_PREDICATE_PROHIBITED, parserContext));
    }
    // first statement can't be marked as chain
    checkFirsStatementCantBeChain(statementContext, methodName);
    // previous return should be custom
    checkChainAllowed(statementContext, methodName);
    TypeProvider defaultReturnType = statementContext.isLastStatement() ?
        methodContext.getDeclaredReturnType().getReturnTypeOrDefault(context, VOID) : VOID;
    TypeProvider declaredStatementReturnType = statementContext
        .getDeclaredReturnOrDefault(context, methodContext.getDeclaredReturnType(),
            defaultReturnType);
    ActionType action = new CustomActionType(WAIT_FOR, declaredStatementReturnType);
    methodContext.enterPredicateContext();
    ArgumentsProvider argumentsProvider = new ArgumentsProvider(argsNode, parserContext);
    ParametersContext parametersContext = new StatementParametersContext(parserContext, context,
        argsNode, methodContext);
    List<UtamArgument> arguments = argumentsProvider.getArguments(false);
    List<MethodParameter> parameters = arguments
        .stream()
        .map(arg -> arg.asParameter(context, methodContext, parametersContext))
        .collect(Collectors.toList());
    checkFunctionParameter(context, parserContext, parameters);
    List<ComposeMethodStatement> predicate = arguments.get(0).getPredicate(context, methodContext);
    methodContext.exitPredicateContext();
    TypeProvider operationReturnType = predicate.get(predicate.size() - 1).getReturnType();
    Operation operation = new OperationWithPredicate(action, operationReturnType, predicate);
    checkMatcher(operationReturnType, parserContext);

    return buildStatement(SELF_OPERAND, operation, context, methodContext, statementContext);
  }

  private void checkFunctionParameter(TranslationContext context, String contextString, List<MethodParameter> parameters) {
    if (parameters.size() != 1) {
      String message = context.getErrorMessage("UA008", contextString, "1", String.valueOf(parameters.size()));
      throw new UtamCompilationError(argsNode, message);
    }
    MethodParameter parameter = parameters.get(0);
    if(parameter != null) {
      String actualType = parameter.getType().getSimpleName();
      String parameterValue = parameter.getValue();
      String message = context
          .getErrorMessage("UA009", contextString, parameterValue, "function", actualType);
      throw new UtamCompilationError(argsNode, message);
    }
  }

  /**
   * information about applied action with a predicate
   */
  static class OperationWithPredicate extends ApplyOperation {

    final List<String> predicateCode = new ArrayList<>();
    private final List<TypeProvider> classImports = new ArrayList<>();

    OperationWithPredicate(ActionType action, TypeProvider returnType,
        List<ComposeMethodStatement> predicate) {
      super(action, returnType, new ArrayList<>());
      for (ComposeMethodStatement statement : predicate) {
        predicateCode.addAll(statement.getCodeLines());
        ParameterUtils.setImports(classImports, statement.getClassImports());
        getActionParameters().addAll(statement.getParameters());
      }
    }

    @Override
    protected String getInvocationString() {
      String wrappedCode = predicateCode
          .stream()
          // predicate code may contain if statement, hence does not need ";"
          .map(str -> str + (str.endsWith("}") ? "" : ";"))
          .collect(Collectors.joining("\n"));
      return String.format("%s(() -> {\n%s\n})", WAIT_FOR, wrappedCode);
    }

    @Override
    protected List<TypeProvider> getAddedClassImports() {
      return classImports;
    }
  }
}
