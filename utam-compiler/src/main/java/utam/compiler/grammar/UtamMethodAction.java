/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import static utam.compiler.diagnostics.ValidationUtilities.VALIDATION;
import static utam.compiler.grammar.JsonDeserializer.isEmptyNode;
import static utam.compiler.grammar.UtamArgument.processArgsNode;
import static utam.compiler.grammar.UtamPageObject.BEFORE_LOAD_METHOD_NAME;
import static utam.compiler.helpers.ElementContext.DOCUMENT_ELEMENT_NAME;
import static utam.compiler.helpers.ElementContext.ROOT_ELEMENT_NAME;
import static utam.compiler.helpers.TypeUtilities.isCustomType;

import com.fasterxml.jackson.databind.JsonNode;
import java.util.List;
import java.util.Set;
import java.util.function.BiFunction;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import utam.compiler.UtamCompilationError;
import utam.compiler.grammar.UtamMatcher.ComposeStatementMatcherProvider;
import utam.compiler.grammar.UtamMethodActionApply.ApplyOperation;
import utam.compiler.helpers.ActionType;
import utam.compiler.helpers.ElementContext;
import utam.compiler.helpers.MethodContext;
import utam.compiler.helpers.ReturnType;
import utam.compiler.helpers.ReturnType.StatementReturnType;
import utam.compiler.helpers.StatementContext;
import utam.compiler.helpers.TranslationContext;
import utam.compiler.representation.ComposeMethodStatement;
import utam.compiler.representation.ComposeMethodStatement.FlatMapEach;
import utam.compiler.representation.ComposeMethodStatement.ForEach;
import utam.compiler.representation.ComposeMethodStatement.MapEach;
import utam.compiler.representation.ComposeMethodStatement.Operand;
import utam.compiler.representation.MatcherObject;
import utam.core.declarative.representation.TypeProvider;

/**
 * Compose statement mapping
 *
 * @author elizaveta.ivanova
 * @since 228
 */
public abstract class UtamMethodAction {

  static final Operand SELF_OPERAND = new ConstOperand("this");
  private static final Set<String> BEFORE_LOAD_ELEMENTS =
      Stream.of(DOCUMENT_ELEMENT_NAME, ROOT_ELEMENT_NAME).collect(Collectors.toSet());
  final String elementName;
  // if set to true, action should be applied to the result of the previous statement
  final boolean isChain;
  final JsonNode argsNode;
  final BiFunction<TranslationContext, MethodContext, MatcherObject> matcherProvider;
  final boolean hasMatcher;
  private final JsonNode returnTypeJsonNode;
  private final Boolean isReturnList;

  UtamMethodAction(
      String elementName,
      JsonNode argsNode,
      JsonNode matcherNode,
      JsonNode returnTypeJsonNode,
      Boolean isReturnList,
      boolean isChain) {
    this.argsNode = argsNode;
    this.hasMatcher = !isEmptyNode(matcherNode);
    this.matcherProvider =
        (context, methodContext) ->
            isEmptyNode(matcherNode)
                ? null
                : new ComposeStatementMatcherProvider(matcherNode, methodContext)
                    .getMatcherObject(context);
    this.elementName = elementName;
    this.isChain = isChain;
    this.isReturnList = isReturnList;
    this.returnTypeJsonNode = returnTypeJsonNode;
  }

  final ReturnType getDeclaredReturnType(String methodName) {
    return new StatementReturnType(returnTypeJsonNode, isReturnList, methodName);
  }

  final void chainValidations(StatementContext statementContext, String methodName) {
    if (isChain) {
      // first statement can't be chain
      if (statementContext.isFirstStatement()) {
        String message = VALIDATION.getErrorMessage(616, methodName);
        throw new UtamCompilationError(message);
      }
      // chain should only be allowed if previous statement returned custom type
      TypeProvider previousStatementReturn = statementContext.getPreviousStatementReturnType();
      if (!isCustomType(previousStatementReturn)) {
        String returnType =
            previousStatementReturn == null ? "void" : previousStatementReturn.getSimpleName();
        String message = VALIDATION.getErrorMessage(614, methodName, returnType);
        throw new UtamCompilationError(message);
      }
    }
  }

  /**
   * check that beforeLoad method statement does not use elements other than "root" or "document"
   *
   * @param methodContext method context has method name
   */
  final void checkBeforeLoadElements(MethodContext methodContext) {
    if (BEFORE_LOAD_METHOD_NAME.equals(methodContext.getName())
        && elementName != null
        && !BEFORE_LOAD_ELEMENTS.contains(elementName)) {
      String message = VALIDATION.getErrorMessage(607);
      throw new UtamCompilationError(message);
    }
  }

  /**
   * Get abstraction object between JSON and statement representation
   *
   * @param context translation context
   * @param methodContext method context
   * @param statementContext statement context
   * @return object
   */
  abstract Statement getStatement(
      TranslationContext context, MethodContext methodContext, StatementContext statementContext);

  /**
   * Create a compose statement object from mapped Java entity. This method creates a structure that
   * will be used to generate the code for a given object in the `compose` array. Each object in the
   * `compose` array from the JSON PO will create one `ComposeMethodStatement`.
   *
   * @param context current PO context
   * @param methodContext context of the current method being compiled
   * @param statementContext statement context to collect args
   * @return compose method statement
   */
  ComposeMethodStatement getComposeAction(
      TranslationContext context, MethodContext methodContext, StatementContext statementContext) {
    Statement statement = getStatement(context, methodContext, statementContext);
    // operand should be invoked first because of order of parameters
    Operand operand = statement.getOperand();
    ApplyOperation operation = statement.getApplyOperation();
    MatcherObject matcher = operation.matcher;
    if (isApplyToList(operand)) {
      if (operation.isReturnsVoid() && !hasMatcher) {
        return new ForEach(operand, operation, statementContext);
      }
      if (statementContext.isFlatMap()) {
        return new FlatMapEach(operand, operation, matcher, statementContext);
      }
      return new MapEach(operand, operation, matcher, statementContext);
    }
    return new ComposeMethodStatement.Single(operand, operation, matcher, statementContext);
  }

  /**
   * Check if operand is a list. Override for basic action - for "apply" : "size" returns false
   *
   * @param operand operand
   * @return boolean true if apply action to list
   */
  boolean isApplyToList(Operand operand) {
    return operand.isApplyToList();
  }

  /**
   * Abstraction that connects JSON object and operand/operation in statement
   *
   * @author elizaveta.ivanova
   * @since 240
   */
  abstract static class Statement {

    final TranslationContext context;
    final MethodContext methodContext;
    final StatementContext statementContext;

    Statement(
        TranslationContext context,
        MethodContext methodContext,
        StatementContext statementContext) {
      this.statementContext = statementContext;
      this.context = context;
      this.methodContext = methodContext;
    }

    /**
     * get operation object to construct statement
     *
     * @return object
     */
    abstract ApplyOperation getApplyOperation();

    /**
     * get operand object to construct statement
     *
     * @return object
     */
    abstract Operand getOperand();
  }

  /**
   * action to be invoked on the custom element or self or document - no inferred return type or
   * args
   *
   * @since 232
   */
  static final class CustomActionType implements ActionType {

    private final String methodName;
    private final TypeProvider returnType;

    CustomActionType(String methodName, TypeProvider returnType) {
      this.methodName = methodName;
      this.returnType = returnType;
    }

    @Override
    public TypeProvider getReturnType() {
      return returnType;
    }

    @Override
    public List<TypeProvider> getParametersTypes(String parserContext, int parameterCount) {
      return null; // parameter types are not checked for custom action
    }

    @Override
    public String getApplyString() {
      return methodName;
    }
  }

  /**
   * Operand that does not require method invocation and is either "this" or static class or
   * variable
   *
   * @since 234
   */
  public static class ConstOperand extends Operand {

    private final String strValue;
    private final boolean isList;

    /**
     * Initializes a new instance of the ConstOperand class
     *
     * @param strValue the value of the operand
     */
    ConstOperand(String strValue) {
      this(strValue, false);
    }

    /**
     * Initializes a new instance of the ConstOperand class
     *
     * @param strValue the value of the operand
     * @param isList a value indicating whether this operand is a list
     */
    public ConstOperand(String strValue, boolean isList) {
      this.strValue = strValue;
      this.isList = isList;
    }

    @Override
    public boolean isApplyToList() {
      return isList;
    }

    @Override
    protected String getOperandString() {
      return strValue;
    }
  }

  /**
   * utility class to process arguments
   *
   * @author elizaveta.ivanova
   * @since 238
   */
  static final class ArgumentsProvider {

    final JsonNode argsNode;
    final String argsParserContext;

    ArgumentsProvider(JsonNode argsNode, String argsParserContext) {
      this.argsNode = argsNode;
      this.argsParserContext = argsParserContext;
      VALIDATION.validateOptionalNotEmptyArray(argsNode, argsParserContext, "args");
    }

    /**
     * get list of arguments after validation
     *
     * @param argsValidationMode predicate and method declaration can't have literals
     * @return list of args
     */
    List<UtamArgument> getArguments(UtamArgument.ArgsValidationMode argsValidationMode) {
      return processArgsNode(argsNode, argsParserContext, argsValidationMode);
    }

    /**
     * wrap getting an element
     *
     * @param context translation context
     * @param elementName element name
     * @return element context
     */
    ElementContext getElementArgument(TranslationContext context, String elementName) {
      ElementContext element = context.getElement(elementName);
      if (element == null) {
        String message = VALIDATION.getErrorMessage(101, argsParserContext, elementName);
        throw new UtamCompilationError(argsNode, message);
      }
      return element;
    }
  }
}
