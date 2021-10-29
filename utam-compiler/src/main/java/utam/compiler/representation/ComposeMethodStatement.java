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
import java.util.List;
import utam.compiler.helpers.MatcherType;
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

  ComposeMethodStatement(
      Operand operand,
      Operation operation,
      TypeProvider declaredReturnType,
      Matcher matcher,
      StatementContext statementContext) {
    this.actionReturnType = declaredReturnType;
    this.matcherOperandType = matcher != null ? matcher.matcherType.getOperandType() : null;
    this.statementContext = statementContext;
    this.statementReturns = getReturn(matcher, declaredReturnType);
    setImports(operation, operand);
    setParameters(operation, operand, matcher);
    this.operation = operation;
    this.operand = operand;
    this.codeLines.addAll(operand.getOperandInstantiationCode(statementReturns));
    setCodeLines(matcher);
  }

  private void setCodeLines(Matcher matcher) {
    String statementVariable = statementContext.getVariableName();
    if (matcher != null) {
      codeLines.add(getMethodCallString(true));
      String matcherCode = matcher.matcherType.getCode(statementVariable, matcher.matcherParameters);
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
        setRegularReturnStatement();
      }
    }
  }

  private boolean isUseVariable() {
    if(statementContext.isReturnSelf()) {
      return false;
    }
    if(!statementContext.isLastStatement()
        && !statementContext.isLastPredicateStatement()
        && !statementContext.isUsedAsChain()) {
      return false;
    }
    return !actionReturnType.isSameType(VOID);
  }

  private void setRegularReturnStatement() {
    if(statementContext.isReturnSelf()) {
      codeLines.add("return this");
    } else if (statementContext.isLastStatement() && isUseVariable()) {
      codeLines.add("return " + statementContext.getVariableName());
    }
  }

  private String getPredicateReturnStatement() {
    if(statementContext.isReturnSelf()) {
      return ";\nreturn this";
    }
    if(actionReturnType.isSameType(VOID)) {
      // last statement of predicate can't return void, has to return boolean or original type
      return ";\nreturn true";
    }
    return String.format(";\nreturn %s", statementContext.getVariableName());
  }

  private boolean isReturnVoid() {
    return this.statementReturns.isSameType(VOID);
  }

  private TypeProvider getReturn(Matcher matcher, TypeProvider declaredReturnType) {
    if (matcher != null) {
      return PrimitiveType.BOOLEAN;
    }
    if (statementContext.isLastPredicateStatement() && MethodContext.isNullOrVoid(declaredReturnType)) {
      return VOID;
    }
    if (declaredReturnType == null) {
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

  private void setParameters(Operation operation, Operand operand, Matcher matcher) {
    this.parameters.addAll(operand.getElementParameters());
    this.parameters.addAll(operation.getActionParameters());
    if(matcher != null) {
      this.parameters.addAll(matcher.matcherParameters);
    }
  }

  public List<MethodParameter> getParameters() {
    return parameters;
  }

  public TypeProvider getReturnType() {
    return statementReturns;
  }

  public List<String> getCodeLines() {
    return codeLines;
  }

  public List<TypeProvider> getImports() {
    return imports;
  }

  public List<TypeProvider> getClassImports() {
    return classImports;
  }

  abstract String getMethodCallString(boolean useVariable);

  final String getVariableAssignmentPrefix(boolean useVariable) {
    if (!useVariable || isReturnVoid()) {
      return "";
    }
    String returnTypeStr = matcherOperandType != null ? matcherOperandType.getSimpleName()
        : getReturnType().getSimpleName();
    return String.format("%s %s = ", returnTypeStr, statementContext.getVariableName());
  }

  /**
   * invokes method on a single operand
   */
  public static class Single extends ComposeMethodStatement {

    public Single(Operand operand, Operation operation, Matcher matcher, StatementContext statementContext) {
      super(operand, operation, operation.getReturnType(), matcher, statementContext);
    }

    @Override
    String getMethodCallString(boolean useVariable) {
      String statementString = String
          .format("%s.%s", operand.getOperandString(), operation.getInvocationString());
      return getVariableAssignmentPrefix(useVariable) + statementString;
    }
  }

  /**
   * invokes method on list and returns void
   */
  public static class ForEach extends ComposeMethodStatement {

    public ForEach(Operand operand, Operation operation, StatementContext statementContext) {
      super(operand, operation, VOID, null, statementContext);
    }

    @Override
    String getMethodCallString(boolean useVariable) {
      String statementString = String
          .format("%s.forEach(element -> element.%s)", operand.getOperandString(),
              operation.getInvocationString());
      return getVariableAssignmentPrefix(useVariable) + statementString;
    }
  }

  /**
   * invokes method on list and returns list
   */
  public static final class MapEach extends ComposeMethodStatement {

    public MapEach(Operand operand, Operation operation, Matcher matcher, StatementContext statementContext) {
      super(operand, operation, wrapAsList(operation.getReturnType()), matcher, statementContext);
      ParameterUtils.setImport(getClassImports(), COLLECTOR_IMPORT);
    }

    @Override
    String getMethodCallString(boolean useVariable) {
      String statementString = String
          .format("%s.stream().map(element -> element.%s).collect(Collectors.toList())",
              operand.getOperandString(),
              operation.getInvocationString());
      return getVariableAssignmentPrefix(useVariable)
          + statementString;
    }
  }

  /**
   * invokes method on list and returns list
   */
  public static final class FlatMapEach extends ComposeMethodStatement {

    public FlatMapEach(Operand operand, Operation operation, Matcher matcher, StatementContext statementContext) {
      super(operand, operation, wrapAsList(operation.getReturnType()), matcher, statementContext);
      ParameterUtils.setImport(getClassImports(), COLLECTOR_IMPORT);
    }

    @Override
    String getMethodCallString(boolean useVariable) {
      String statementString = String
          .format(
              "%s.stream().flatMap(element -> element.%s.stream()).collect(Collectors.toList())",
              operand.getOperandString(),
              operation.getInvocationString());
      return getVariableAssignmentPrefix(useVariable)
          + statementString;
    }
  }

  /**
   * information about element action is applied to
   */
  public static abstract class Operand {

    protected List<MethodParameter> getElementParameters() {
      return new ArrayList<>();
    }

    public abstract boolean isApplyToList();

    protected List<String> getOperandInstantiationCode(TypeProvider statementReturn) {
      return new ArrayList<>();
    }

    protected List<TypeProvider> getAddedClassImports() {
      return new ArrayList<>();
    }

    protected abstract String getOperandString();
  }

  /**
   * information about applied action
   */
  public static abstract class Operation {

    protected abstract List<MethodParameter> getActionParameters();

    public boolean isReturnsVoid() {
      return VOID.isSameType(getReturnType());
    }

    public abstract TypeProvider getReturnType();

    protected List<TypeProvider> getAddedClassImports() {
      return new ArrayList<>();
    }

    protected abstract String getInvocationString();
  }

  /**
   * information about matcher
   */
  public static class Matcher {

    private final MatcherType matcherType;
    private final List<MethodParameter> matcherParameters;

    public Matcher(MatcherType matcherType,
        List<MethodParameter> matcherParameters) {
      this.matcherType = matcherType;
      this.matcherParameters = matcherParameters;
    }
  }
}
