/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import static utam.compiler.grammar.UtamMethodActionApply.ERR_ELEMENT_REDUNDANT_FOR_CHAIN;
import static utam.compiler.helpers.TypeUtilities.VOID;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import utam.compiler.UtamCompilationError;
import utam.compiler.grammar.ArgsProcessor.ArgsProcessorPredicate;
import utam.compiler.grammar.UtamMethodActionApply.ApplyOperation;
import utam.compiler.helpers.ActionType;
import utam.compiler.helpers.MethodContext;
import utam.compiler.helpers.ParameterUtils;
import utam.compiler.helpers.StatementContext;
import utam.compiler.helpers.TranslationContext;
import utam.compiler.representation.ComposeMethodStatement;
import utam.compiler.representation.ComposeMethodStatement.Operation;
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
      @JsonProperty(value = "apply") String apply,
      @JsonProperty(value = "args") UtamArgument[] args,
      @JsonProperty(value = "matcher") UtamMatcher matcher,
      @JsonProperty(value = "chain", defaultValue = "false") boolean isChain) {
    super(elementName, apply, null, args, matcher, null, null, isChain);
  }

  @Override
  ComposeMethodStatement getComposeAction(TranslationContext context,
      MethodContext methodContext, StatementContext statementContext) {
    String methodName = methodContext.getName();
    String validationContextStr = String.format("method '%s'", methodName);

    // if statement is marked as a chain, it should be applied to previous result, so "element" is redundant
    if (isChain && elementName != null) {
      throw new UtamCompilationError(
          String.format(ERR_ELEMENT_REDUNDANT_FOR_CHAIN, validationContextStr));
    }
    if (statementContext.isInsidePredicate()) {
      throw new UtamCompilationError(
          String.format(ERR_NESTED_PREDICATE_PROHIBITED, validationContextStr));
    }
    // first statement can't be marked as chain
    checkFirsStatementCantBeChain(statementContext, methodName);
    // previous return should be custom
    checkChainAllowed(statementContext, methodName);

    // check that only one arg is provided
    new ArgsProcessorPredicate(context, methodContext).getParameters(args);
    TypeProvider defaultReturnType = statementContext.isLastStatement() ?
        methodContext.getDeclaredReturnType().getReturnTypeOrDefault(context, VOID) : VOID;
    TypeProvider declaredStatementReturnType = statementContext
        .getDeclaredReturnOrDefault(context, methodContext.getDeclaredReturnType(),
            defaultReturnType);
    ActionType action = new CustomActionType(apply, declaredStatementReturnType);
    List<ComposeMethodStatement> predicate = args[0].getPredicate(context, methodContext);
    TypeProvider operationReturnType = predicate.get(predicate.size() - 1).getReturnType();
    Operation operation = new OperationWithPredicate(action, operationReturnType, predicate);
    checkMatcher(operationReturnType, validationContextStr);

    return buildStatement(SELF_OPERAND, operation, context, methodContext, statementContext);
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
