/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import static utam.compiler.grammar.UtamPageObject.BEFORE_LOAD_METHOD_NAME;
import static utam.compiler.helpers.BasicElementActionType.size;
import static utam.compiler.helpers.ElementContext.DOCUMENT_ELEMENT_NAME;
import static utam.compiler.helpers.ElementContext.ROOT_ELEMENT_NAME;
import static utam.compiler.translator.TranslationTypesConfigJava.isCustomType;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import utam.compiler.UtamCompilationError;
import utam.compiler.helpers.ActionType;
import utam.compiler.helpers.MatcherType;
import utam.compiler.helpers.MethodContext;
import utam.compiler.helpers.PrimitiveType;
import utam.compiler.helpers.ReturnType;
import utam.compiler.helpers.StatementContext;
import utam.compiler.helpers.StatementContext.StatementType;
import utam.compiler.helpers.TranslationContext;
import utam.compiler.representation.ComposeMethodStatement;
import utam.compiler.representation.ComposeMethodStatement.FlatMapEach;
import utam.compiler.representation.ComposeMethodStatement.ForEach;
import utam.compiler.representation.ComposeMethodStatement.MapEach;
import utam.compiler.representation.ComposeMethodStatement.Matcher;
import utam.compiler.representation.ComposeMethodStatement.Operand;
import utam.compiler.representation.ComposeMethodStatement.Operation;
import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.TypeProvider;

/**
 * Compose statement mapping
 *
 * @author elizaveta.ivanova
 * @since 228
 */
@JsonDeserialize(using = UtamMethodActionDeserializer.class)
public abstract class UtamMethodAction {

  static final String ERR_FIRST_STATEMENT_CANT_BE_MARKED_AS_CHAIN = "method '%s': first statement can't be marked as chain";
  static final String ERR_INCORRECT_RETURN_TYPE = "method '%s' incorrect statement return type: expected '%s', provided is '%s'";
  static final String ERR_CHAIN_REQUIRES_CUSTOM_RETURN = "method '%s': to use chain, "
      + "previous statement should return custom type, but it returns '%s'";
  static final String ERR_ELEMENT_REDUNDANT_FOR_CHAIN = "%s: statement marked as chain "
      + "and will be applied to the result of the previous statement, "
      + "so element property is redundant";
  static final String ERR_DISALLOWED_ELEMENT = "Only document or root element are allowed in beforeLoad method";
  private static final Set<String> BEFORE_LOAD_ELEMENTS = Stream
      .of(DOCUMENT_ELEMENT_NAME, ROOT_ELEMENT_NAME).collect(
          Collectors.toSet());

  static final Operand SELF_OPERAND = new ConstOperand("this");

  final String elementName;
  final String apply;
  final UtamUtilityMethodAction applyExternal;
  final UtamMatcher matcher;
  final UtamArgument[] args;
  // if set to true, action should be applied to the result of the previous statement
  final boolean isChain;
  // need method context to use method name inside return type class
  private final Function<String, ReturnType> declaredTypeProvider;

  @JsonCreator
  UtamMethodAction(
      @JsonProperty(value = "element") String elementName,
      @JsonProperty(value = "apply") String apply,
      @JsonProperty(value = "applyExternal") UtamUtilityMethodAction applyExternal,
      @JsonProperty(value = "args") UtamArgument[] args,
      @JsonProperty(value = "matcher") UtamMatcher matcher,
      @JsonProperty(value = "returnType") JsonNode returnType,
      @JsonProperty(value = "returnAll") Boolean isReturnList,
      @JsonProperty(value = "chain", defaultValue = "false") boolean isChain) {
    this.args = args;
    this.matcher = matcher;
    this.elementName = elementName;
    this.apply = apply;
    this.applyExternal = applyExternal;
    this.isChain = isChain;
    this.declaredTypeProvider = str -> new ReturnType(returnType, isReturnList, str);
  }

  private MatcherType getMatcherType() {
    return matcher == null ? null : matcher.getMatcherType();
  }

  private List<MethodParameter> getMatcherParameters(TranslationContext context,
      MethodContext methodContext, StatementContext statementContext) {
    return matcher == null ? new ArrayList<>()
        : matcher.getParameters(context, methodContext, statementContext);
  }

  final ReturnType getDeclaredReturnType(String methodName) {
    return declaredTypeProvider.apply(methodName);
  }

  final void checkMatcher(TypeProvider operandType, String validationContextStr) {
    if (getMatcherType() != null) {
      getMatcherType().checkOperandForMatcher(operandType, validationContextStr);
    }
  }

  final void checkDefinedReturnType(TypeProvider expectedType, TypeProvider declaredType,
      String methodName) {
    TypeProvider adjustedExpectedType = matcher != null ? PrimitiveType.BOOLEAN : expectedType;
    if (!adjustedExpectedType.isSameType(declaredType)) {
      throw new UtamCompilationError(String
          .format(ERR_INCORRECT_RETURN_TYPE,
              methodName,
              adjustedExpectedType.getSimpleName(),
              declaredType.getSimpleName()));
    }
  }

  final void checkFirsStatementCantBeChain(StatementContext statementContext, String methodName) {
    if (statementContext.isFirstStatement() && isChain) {
      throw new UtamCompilationError(
          String.format(ERR_FIRST_STATEMENT_CANT_BE_MARKED_AS_CHAIN, methodName));
    }
  }

  // chain should only be allowed if previous statement returned custom type
  final void checkChainAllowed(StatementContext statementContext, String methodName) {
    TypeProvider previousStatementReturn = statementContext.getPreviousStatementReturnType();
    if (isChain && !isCustomType(previousStatementReturn)) {
      String returnType =
          previousStatementReturn == null ? "void" : previousStatementReturn.getSimpleName();
      throw new UtamCompilationError(
          String.format(ERR_CHAIN_REQUIRES_CUSTOM_RETURN, methodName, returnType));
    }
  }

  // if statement is marked as a chain, it should be applied to previous result, so "element" is redundant
  final void checkChainElementRedundant(String validationContextStr) {
    if (isChain && elementName != null) {
      throw new UtamCompilationError(
          String.format(ERR_ELEMENT_REDUNDANT_FOR_CHAIN, validationContextStr));
    }
  }

  /**
   * check that beforeLoad method statement does not use elements other than "root" or "document"
   *
   * @param methodContext method context has method name
   */
  final void checkBeforeLoadElements(MethodContext methodContext) {
    if (BEFORE_LOAD_METHOD_NAME.equals(methodContext.getName()) && elementName != null
        && !BEFORE_LOAD_ELEMENTS.contains(elementName)) {
      throw new UtamCompilationError(ERR_DISALLOWED_ELEMENT);
    }
  }

  /**
   * Create a compose statement object from mapped Java entity. This method creates a structure that
   * will be used to generate the code for a given object in the `compose` array. Each object in the
   * `compose` array from the JSON PO will create one `ComposeMethodStatement`.
   *
   * @param context          current PO context
   * @param methodContext    context of the current method being compiled
   * @param statementContext statement context to collect args
   * @return compose method statement
   */
  abstract ComposeMethodStatement getComposeAction(
      TranslationContext context,
      MethodContext methodContext,
      StatementContext statementContext);

  final ComposeMethodStatement buildStatement(
      Operand operand,
      Operation operation,
      TranslationContext context,
      MethodContext methodContext,
      StatementContext statementContext) {
    List<MethodParameter> matcherParameters = getMatcherParameters(context, methodContext,
        statementContext);
    ComposeMethodStatement.Matcher matcher =
        this.matcher == null ? null : new Matcher(getMatcherType(), matcherParameters);

    if (isApplyToList(operand)) {
      if (operation.isReturnsVoid() && matcher == null) {
        return new ForEach(operand, operation, statementContext);
      }
      if (statementContext.isFlatMap()) {
        return new FlatMapEach(operand, operation, matcher, statementContext);
      }
      return new MapEach(operand, operation, matcher, statementContext);
    }
    return new ComposeMethodStatement.Single(operand, operation, matcher, statementContext);
  }

  private boolean isApplyToList(Operand operand) {
    return operand.isApplyToList() && !size.getApplyString().equals(apply);
  }

  // overridden for beforeLoad which is not supposed to return value ever
  StatementType getStatementType(int index, int numberOfStatements) {
    return index == numberOfStatements - 1 ? StatementType.LAST_STATEMENT
        : StatementType.REGULAR_STATEMENT;
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
    public List<TypeProvider> getParametersTypes() {
      return null; //parameter types are not checked for custom action
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
     * @param isList   a value indicating whether this operand is a list
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
}
