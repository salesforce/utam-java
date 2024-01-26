/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.representation;

import static utam.compiler.helpers.ParameterUtils.setDeclarationImports;
import static utam.compiler.helpers.ParameterUtils.setImplementationImports;
import static utam.compiler.helpers.TypeUtilities.COLLECTOR_IMPORT;
import static utam.compiler.helpers.TypeUtilities.VOID;
import static utam.compiler.helpers.TypeUtilities.wrapAsList;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import utam.compiler.helpers.MethodContext;
import utam.compiler.helpers.ParameterUtils;
import utam.compiler.helpers.PrimitiveType;
import utam.compiler.helpers.StatementContext;
import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.TypeProvider;

/**
 * single statement in a compose method
 *
 * @author elizaveta.ivanova
 * @since 232
 */
public abstract class ComposeMethodStatement {

  final Operand operand;
  final Operation operation;
  private final StatementContext statementContext;
  private final List<TypeProvider> classImports = new ArrayList<>();
  private final List<TypeProvider> imports = new ArrayList<>();
  private final List<String> codeLines = new ArrayList<>();
  private final List<MethodParameter> parameters = new ArrayList<>();
  private final TypeProvider statementReturns;
  private final TypeProvider matcherOperandType;
  private final TypeProvider actionReturnType;

  /**
   * Initializes aa new instance of the ComposeMethodStatement class
   *
   * @param operand the operand of the statement
   * @param operation the operation of the statement
   * @param declaredReturnType the return type of the statement
   * @param matcher a matcher for the statement
   * @param statementContext the context of the statement
   */
  ComposeMethodStatement(
      Operand operand,
      Operation operation,
      TypeProvider declaredReturnType,
      MatcherObject matcher,
      StatementContext statementContext) {
    this.actionReturnType = declaredReturnType;
    this.matcherOperandType = matcher != null ? matcher.getOperandType() : null;
    this.statementContext = statementContext;
    this.statementReturns = getReturn(matcher, declaredReturnType);
    setImports(operation, operand);
    setParameters(operation, operand, matcher);
    this.operation = operation;
    this.operand = operand;
    this.codeLines.addAll(operand.getOperandInstantiationCode(statementReturns));
    setCodeLines(matcher);
  }

  /**
   * constructor for "apply" : "returnSelf" statement
   *
   * @param returnType self type to return
   */
  ComposeMethodStatement(TypeProvider returnType) {
    this.operand = null;
    this.operation = null;
    this.statementContext = null;
    this.statementReturns = returnType;
    this.matcherOperandType = null;
    this.actionReturnType = null;
  }

  private void setCodeLines(MatcherObject matcher) {
    String statementVariable = statementContext.getVariableName();
    if (matcher != null) {
      codeLines.add(getMethodCallString(true));
      String matcherCode = matcher.getCode(statementVariable);
      String matcherVariable = statementContext.getMatcherVariableName();
      if (statementContext.isLastStatement() || statementContext.isLastPredicateStatement()) {
        codeLines.add(String.format("Boolean %s = %s", matcherVariable, matcherCode));
        codeLines.add("return " + matcherVariable);
      } else {
        codeLines.add(matcherCode);
      }
    } else {
      boolean isUseVariable = isUseVariable();
      String invocationStr = getMethodCallString(isUseVariable);
      if (statementContext.isLastPredicateStatement()) {
        String lastStatement = getPredicateReturnStatement();
        codeLines.add(invocationStr.concat(lastStatement));
      } else {
        codeLines.add(invocationStr);
        if (statementContext.isLastStatement() && isUseVariable()) {
          codeLines.add("return " + statementContext.getVariableName());
        }
      }
    }
  }

  private boolean isUseVariable() {
    if (!statementContext.isLastStatement()
        && !statementContext.isLastPredicateStatement()
        && !statementContext.isUsedAsChain()) {
      return false;
    }
    return !actionReturnType.isSameType(VOID);
  }

  private String getPredicateReturnStatement() {
    if (actionReturnType.isSameType(VOID)) {
      // last statement of predicate can't return void, has to return boolean or original type
      return ";\nreturn true";
    }
    return String.format(";\nreturn %s", statementContext.getVariableName());
  }

  private boolean isReturnVoid() {
    return this.statementReturns.isSameType(VOID);
  }

  private TypeProvider getReturn(MatcherObject matcher, TypeProvider declaredReturnType) {
    if (matcher != null) {
      return PrimitiveType.BOOLEAN;
    }
    if (statementContext.isLastPredicateStatement()
        && MethodContext.isNullOrVoid(declaredReturnType)) {
      return VOID;
    }
    return declaredReturnType;
  }

  private void setImports(Operation operation, Operand operand) {
    setDeclarationImports(this.imports, operation.getActionParameters());
    setImplementationImports(this.classImports, operation.getActionParameters());
    ParameterUtils.setImports(this.classImports, operation.getAddedClassImports());
    ParameterUtils.setImports(this.classImports, operand.getAddedClassImports());
    // to declare variable need add class import
    ParameterUtils.setImport(this.classImports, statementReturns);
  }

  private void setParameters(Operation operation, Operand operand, MatcherObject matcher) {
    this.parameters.addAll(operand.getElementParameters());
    this.parameters.addAll(operation.getActionParameters());
    if (matcher != null) {
      this.parameters.addAll(matcher.getParameters());
    }
  }

  /**
   * Gets the parameters of the statement
   *
   * @return the list of parameters of the statement
   */
  public List<MethodParameter> getParameters() {
    return parameters;
  }

  /**
   * Gets the return type of the statement
   *
   * @return the return type of the statement
   */
  public TypeProvider getReturnType() {
    return statementReturns;
  }

  /**
   * Gets the code lines of the statement
   *
   * @return the list of code lines of the statement
   */
  public List<String> getCodeLines() {
    return codeLines;
  }

  /**
   * Gets the imports required by the statement
   *
   * @return the list of imports required to be added to the interface definition by tne statement
   */
  public List<TypeProvider> getImports() {
    return imports;
  }

  /**
   * Gets the class imports required by the statement
   *
   * @return the list of imports required to be added to the implementation definition by tne
   *     statement
   */
  public List<TypeProvider> getClassImports() {
    return classImports;
  }

  abstract String getMethodCallString(boolean useVariable);

  final String getVariableAssignmentPrefix(boolean useVariable) {
    if (!useVariable || isReturnVoid()) {
      return "";
    }
    String returnTypeStr =
        matcherOperandType != null
            ? matcherOperandType.getSimpleName()
            : getReturnType().getSimpleName();
    return String.format("%s %s = ", returnTypeStr, statementContext.getVariableName());
  }

  /** invokes method on a single operand */
  public static class Single extends ComposeMethodStatement {

    /**
     * Initializes aa new instance of the ComposeMethodStatement class for a single element
     *
     * @param operand the operand of the statement
     * @param operation the operation of the statement
     * @param matcher a matcher for the statement
     * @param statementContext the context of the statement
     */
    public Single(
        Operand operand,
        Operation operation,
        MatcherObject matcher,
        StatementContext statementContext) {
      super(operand, operation, operation.getReturnType(), matcher, statementContext);
    }

    @Override
    String getMethodCallString(boolean useVariable) {
      String statementString =
          String.format("%s.%s", operand.getOperandString(), operation.getInvocationString());
      return getVariableAssignmentPrefix(useVariable) + statementString;
    }
  }

  /** invokes method on list and returns void */
  public static class ForEach extends ComposeMethodStatement {

    /**
     * Initializes aa new instance of the ComposeMethodStatement class to be executed on a list
     *
     * @param operand the operand of the statement
     * @param operation the operation of the statement
     * @param statementContext the context of the statement
     */
    public ForEach(Operand operand, Operation operation, StatementContext statementContext) {
      super(operand, operation, VOID, null, statementContext);
    }

    @Override
    String getMethodCallString(boolean useVariable) {
      String statementString =
          String.format(
              "%s.forEach(element -> element.%s)",
              operand.getOperandString(), operation.getInvocationString());
      return getVariableAssignmentPrefix(useVariable) + statementString;
    }
  }

  /** invokes method on list and returns list */
  public static final class MapEach extends ComposeMethodStatement {

    /**
     * Initializes aa new instance of the ComposeMethodStatement class to be executed on a list and
     * returning a list
     *
     * @param operand the operand of the statement
     * @param operation the operation of the statement
     * @param matcher a matcher for the statement
     * @param statementContext the context of the statement
     */
    public MapEach(
        Operand operand,
        Operation operation,
        MatcherObject matcher,
        StatementContext statementContext) {
      super(operand, operation, wrapAsList(operation.getReturnType()), matcher, statementContext);
      ParameterUtils.setImport(getClassImports(), COLLECTOR_IMPORT);
    }

    @Override
    String getMethodCallString(boolean useVariable) {
      String statementString =
          String.format(
              "%s.stream().map(element -> element.%s).collect(Collectors.toList())",
              operand.getOperandString(), operation.getInvocationString());
      return getVariableAssignmentPrefix(useVariable) + statementString;
    }
  }

  /** invokes method on list and returns list */
  public static final class FlatMapEach extends ComposeMethodStatement {

    /**
     * Initializes aa new instance of the ComposeMethodStatement class to be executed on a list and
     * returning a list
     *
     * @param operand the operand of the statement
     * @param operation the operation of the statement
     * @param matcher a matcher for the statement
     * @param statementContext the context of the statement
     */
    public FlatMapEach(
        Operand operand,
        Operation operation,
        MatcherObject matcher,
        StatementContext statementContext) {
      super(operand, operation, wrapAsList(operation.getReturnType()), matcher, statementContext);
      ParameterUtils.setImport(getClassImports(), COLLECTOR_IMPORT);
    }

    @Override
    String getMethodCallString(boolean useVariable) {
      String statementString =
          String.format(
              "%s.stream().flatMap(element -> element.%s.stream()).collect(Collectors.toList())",
              operand.getOperandString(), operation.getInvocationString());
      return getVariableAssignmentPrefix(useVariable) + statementString;
    }
  }

  /** information about element action is applied to */
  public abstract static class Operand {

    /**
     * Gets the element parameters
     *
     * @return the list of element parameters
     */
    protected List<MethodParameter> getElementParameters() {
      return new ArrayList<>();
    }

    /**
     * Gets a value indicating whether this operand applies to a list
     *
     * @return true if the operand applies to each element of a list; otherwise, false
     */
    public abstract boolean isApplyToList();

    /**
     * Gets the operand instantiation code
     *
     * @param statementReturn the return type of the statement
     * @return the list of code statements of the operand
     */
    protected List<String> getOperandInstantiationCode(TypeProvider statementReturn) {
      return new ArrayList<>();
    }

    /**
     * Gets the list of imports required to add to the implementation class
     *
     * @return the list of types added to the implementation class
     */
    protected List<TypeProvider> getAddedClassImports() {
      return new ArrayList<>();
    }

    /**
     * Gets the operand string
     *
     * @return the operand string
     */
    protected abstract String getOperandString();
  }

  /** information about applied action */
  public abstract static class Operation {

    /**
     * Gets the action parameters
     *
     * @return the list of action parameters
     */
    protected abstract List<MethodParameter> getActionParameters();

    /**
     * Gets a value indicating whether this operation returns void
     *
     * @return true if the operation returns void; otherwise, false
     */
    public boolean isReturnsVoid() {
      return VOID.isSameType(getReturnType());
    }

    /**
     * Gets the return type of the operation
     *
     * @return the operation return type
     */
    public abstract TypeProvider getReturnType();

    /**
     * Gets the list of imports required to add to the implementation class
     *
     * @return the list of types added to the implementation class
     */
    protected List<TypeProvider> getAddedClassImports() {
      return new ArrayList<>();
    }

    /**
     * Gets the operation invocation string
     *
     * @return the operation invocation string
     */
    protected abstract String getInvocationString();
  }

  /** "apply" : "returnSelf" statement */
  public static class ReturnSelf extends ComposeMethodStatement {

    /**
     * Initializes a new instance of the ReturnSelf class, a statement that returns the PageObject
     *
     * @param returnType the return type of the Page Object
     */
    public ReturnSelf(TypeProvider returnType) {
      super(returnType);
    }

    @Override
    public List<MethodParameter> getParameters() {
      return new ArrayList<>();
    }

    @Override
    public List<String> getCodeLines() {
      return Collections.singletonList("return this");
    }

    @Override
    public List<TypeProvider> getImports() {
      return new ArrayList<>();
    }

    @Override
    public List<TypeProvider> getClassImports() {
      return new ArrayList<>();
    }

    @Override
    String getMethodCallString(boolean useVariable) {
      return null;
    }
  }
}
