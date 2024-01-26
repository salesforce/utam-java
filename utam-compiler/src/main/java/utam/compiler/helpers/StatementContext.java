/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.helpers;

import static utam.compiler.helpers.StatementContext.StatementType.PREDICATE_LAST_STATEMENT;
import static utam.compiler.helpers.StatementContext.StatementType.PREDICATE_STATEMENT;
import static utam.compiler.helpers.TypeUtilities.isListType;

import utam.compiler.grammar.UtamMethodAction.ConstOperand;
import utam.compiler.representation.ComposeMethodStatement.Operand;
import utam.core.declarative.representation.TypeProvider;

/**
 * helper for statements processing
 *
 * @author elizaveta.ivanova
 * @since 236
 */
public class StatementContext {

  private final StatementType statementType;
  private final ReturnType declaredStatementReturn;
  // statement index in the compose method, starting from 0
  private final int statementIndex;
  private final TypeProvider previousStatementReturn;
  private final boolean isUsedAsChain;

  /**
   * Initializes a new instance of the StatementContext class
   *
   * @param previousStatementReturn the return type of the previous statement
   * @param statementIndex the index of this statement
   * @param isUsedAsChain a value indicating if this statement is used as a chain
   * @param statementType the type of statement
   * @param declaredStatementReturn the declared statement return type
   */
  public StatementContext(
      TypeProvider previousStatementReturn,
      int statementIndex,
      boolean isUsedAsChain,
      StatementType statementType,
      ReturnType declaredStatementReturn) {
    this.statementIndex = statementIndex;
    this.statementType = statementType;
    this.declaredStatementReturn = declaredStatementReturn;
    this.previousStatementReturn = previousStatementReturn;
    this.isUsedAsChain = isUsedAsChain;
  }

  /**
   * Gets the chain operand of the statement context
   *
   * @return the chain operand of the statement context
   */
  public Operand getChainOperand() {
    String operandString = getVariableName(statementIndex - 1);
    boolean isList = isListType(previousStatementReturn);
    return new ConstOperand(operandString, isList);
  }

  /**
   * Gets the previous statement return type
   *
   * @return the previous statement return type
   */
  public TypeProvider getPreviousStatementReturnType() {
    return previousStatementReturn;
  }

  private String getVariableName(int statementIndex) {
    // changing this affects unit tests results!!!
    final String STATEMENT_VARIABLE_PREFIX = "statement";
    return getPredicateVariablePrefix() + STATEMENT_VARIABLE_PREFIX + statementIndex;
  }

  /**
   * Gets the variable name of the statement
   *
   * @return the variable name of the statement
   */
  public String getVariableName() {
    return getVariableName(statementIndex);
  }

  /**
   * Gets the matcher variable name
   *
   * @return the matcher variable name
   */
  public String getMatcherVariableName() {
    // changing this affects unit tests results!!!
    final String MATCHER_VARIABLE_PREFIX = "matcher";
    return getPredicateVariablePrefix() + MATCHER_VARIABLE_PREFIX + statementIndex;
  }

  /**
   * Gets the predicate variable prefix
   *
   * @return the predicate variable prefix
   */
  private String getPredicateVariablePrefix() {
    return isInsidePredicate() ? "p" : "";
  }

  /**
   * Gets a value indicating whether this statement is inside a predicate
   *
   * @return true if this statement is inside a predicate; otherwise false
   */
  public boolean isInsidePredicate() {
    return statementType == PREDICATE_STATEMENT || statementType == PREDICATE_LAST_STATEMENT;
  }

  /**
   * Gets the variable name declared for an element
   *
   * @param elementName the name of the element
   * @return the variable name for the named element
   */
  public String getElementVariableName(String elementName) {
    return getPredicateVariablePrefix() + elementName + statementIndex;
  }

  /**
   * Gets the declared return type of the statement, or null if none is declared
   *
   * @param context the translation context
   * @return the declared return type of the statement if one is declared; otherwise null
   */
  public TypeProvider getDeclaredStatementReturnOrNull(TranslationContext context) {
    return declaredStatementReturn.getReturnType(context);
  }

  /**
   * Gets a value indicting whether the statement has a declared return type
   *
   * @return true if the statement has a declared return type; otherwise, false
   */
  public boolean hasDeclaredReturn() {
    return declaredStatementReturn.isReturnTypeSet();
  }

  /**
   * Gets the declared return type of the statement, or a default value
   *
   * @param context the translation context
   * @param declaredMethodReturn the declared method return type
   * @param defaultReturnType the default return type
   * @return the declared return type of the statement if one is declared; otherwise null
   */
  public TypeProvider getDeclaredReturnOrDefault(
      TranslationContext context, ReturnType declaredMethodReturn, TypeProvider defaultReturnType) {
    TypeProvider declaredStatementReturn = this.declaredStatementReturn.getReturnType(context);
    if (declaredStatementReturn != null) {
      return declaredStatementReturn;
    }
    if (statementType == StatementType.LAST_STATEMENT) {
      return declaredMethodReturn.getReturnTypeOrDefault(context, defaultReturnType);
    }
    return defaultReturnType;
  }

  /**
   * Gets a value indicting whether the statement is the last statement of a predicate
   *
   * @return true if the statement is the last statement of a predicate; otherwise, false
   */
  public boolean isLastPredicateStatement() {
    return statementType == PREDICATE_LAST_STATEMENT;
  }

  /**
   * Gets a value indicting whether the statement is the last statement of a block
   *
   * @return true if the statement is the last statement of a block; otherwise, false
   */
  public boolean isLastStatement() {
    return statementType == StatementType.LAST_STATEMENT;
  }

  /**
   * Gets a value indicting whether the statement is the first statement of a block
   *
   * @return true if the statement is the first statement of a block; otherwise, false
   */
  public boolean isFirstStatement() {
    return statementIndex == 0;
  }

  /**
   * Gets a value indicting whether the statement is a flat map
   *
   * @return true if the statement is a flat map; otherwise, false
   */
  public boolean isFlatMap() {
    return isListType(previousStatementReturn) && this.declaredStatementReturn.isReturnAllSet();
  }

  /**
   * Gets a value indicting whether the statement is used as a chain
   *
   * @return true if the statement is used as a chain; otherwise, false
   */
  public boolean isUsedAsChain() {
    return isUsedAsChain;
  }

  /** The types of statements */
  public enum StatementType {
    /** a regular statement in a method */
    REGULAR_STATEMENT,

    /** the last statement in a method */
    LAST_STATEMENT,

    /** a statement in a predicate */
    PREDICATE_STATEMENT,

    /** the last statement of a predicate */
    PREDICATE_LAST_STATEMENT
  }
}
