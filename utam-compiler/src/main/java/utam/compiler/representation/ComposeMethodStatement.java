/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.representation;

import static utam.compiler.helpers.ParameterUtils.getDeclarationImports;
import static utam.compiler.helpers.ParameterUtils.getImplementationImports;
import static utam.compiler.helpers.ParameterUtils.getParametersValuesString;
import static utam.compiler.helpers.TypeUtilities.COLLECTOR_IMPORT;
import static utam.compiler.helpers.TypeUtilities.VOID;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;
import utam.compiler.UtamCompilationError;
import utam.compiler.helpers.ActionType;
import utam.compiler.helpers.ElementContext;
import utam.compiler.helpers.ElementContext.Document;
import utam.compiler.helpers.MatcherType;
import utam.compiler.helpers.MethodContext;
import utam.compiler.helpers.PrimitiveType;
import utam.compiler.helpers.TypeUtilities;
import utam.compiler.helpers.TypeUtilities.ListOf;
import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.TypeProvider;
import utam.core.framework.base.UtamUtilitiesContext;

/**
 * single statement in a compose method, all statements are
 *
 * @author elizaveta.ivanova
 * @since 232
 */
public abstract class ComposeMethodStatement {

  public static final Operand SELF_OPERAND = new SelfOperand();
  public static final Operand DOCUMENT_OPERAND = new DocumentOperand();
  public static final String WAIT_FOR = "waitFor";
  private final List<TypeProvider> classImports = new ArrayList<>();
  private final List<TypeProvider> imports = new ArrayList<>();
  private final List<String> codeLines = new ArrayList<>();
  private final List<MethodParameter> parameters = new ArrayList<>();
  private final TypeProvider returns;

  ComposeMethodStatement(
      Operand operand,
      Operation operation,
      TypeProvider returnType,
      MatcherType matcher,
      List<MethodParameter> matcherParameters,
      boolean isLastPredicateStatement) {
    TypeProvider actionReturns = matcher != null ? PrimitiveType.BOOLEAN : returnType;
    this.imports.addAll(getDeclarationImports(operation.getActionParameters()));
    this.imports.addAll(operation.getAddedImports());
    this.classImports.addAll(getImplementationImports(operation.getActionParameters()));
    this.classImports.addAll(operation.getAddedClassImports());
    this.classImports.addAll(operand.getAddedClassImports());
    setParameters(operand.getElementParameters());
    setParameters(operation.getActionParameters());
    String elementValue = operand.getOperandCode(codeLines,
        str -> getNullConditionCode(str, actionReturns, isLastPredicateStatement));
    String invocationStr = operation.getCode(getMethodCallString(), elementValue);
    if (matcher != null) {
      invocationStr = matcher.getCode(isLastPredicateStatement, matcherParameters, invocationStr);
      setParameters(matcherParameters);
    } else {
      if (isLastPredicateStatement) {
        if (MethodContext.isNullOrVoid(actionReturns)) {
          // last statement of predicate can't return void, has to return boolean or original type
          invocationStr = invocationStr.concat(";\nreturn true;");
        } else {
          invocationStr = String.format("return %s;", invocationStr);
        }
      }
    }
    codeLines.add(invocationStr);
    this.returns = isLastPredicateStatement && MethodContext.isNullOrVoid(actionReturns)
        ? PrimitiveType.BOOLEAN
        : actionReturns;
  }

  private void setParameters(List<MethodParameter> parameters) {
    this.parameters.addAll(parameters);
  }

  ComposeMethodStatement(Operand operand, Operation operation, TypeProvider returnType,
      boolean isLastPredicateStatement) {
    this(operand, operation, returnType, null, null, isLastPredicateStatement);
  }

  public List<MethodParameter> getParameters() {
    return parameters;
  }

  public TypeProvider getReturnType() {
    return returns;
  }

  public List<String> getCodeLines() {
    return codeLines;
  }

  public boolean isUtilityMethodStatement() {
    return false;
  }

  List<TypeProvider> getImports() {
    return imports;
  }

  List<TypeProvider> getClassImports() {
    return classImports;
  }

  abstract String getMethodCallString();

  // for nullable element we need to check if getter returned null and exit
  abstract String getNullConditionCode(String value, TypeProvider returns,
      boolean isLastPredicateStatement);

  /**
   * invokes method on a single element
   */
  public static class Single extends ComposeMethodStatement {

    public Single(Operand operand, Operation operation, MatcherType matcher,
        List<MethodParameter> matcherParameters, boolean isLastPredicateStatement) {
      super(operand, operation, operation.getReturnType(), matcher, matcherParameters,
          isLastPredicateStatement);
    }

    // used in tests
    Single(Operand operand, Operation operation) {
      this(operand, operation, null, null, false);
    }

    @Override
    String getMethodCallString() {
      return "%s.%s";
    }

    @Override
    String getNullConditionCode(String value, TypeProvider returns,
        boolean isLastPredicateStatement) {
      if (isLastPredicateStatement && returns.isSameType(VOID)) {
        return String.format("if (%s == null) { return false; }", value);
      }
      if (returns.isSameType(VOID)) {
        return String.format("if (%s == null) { return; }", value);
      }
      return String.format("if (%s == null) { return %s; }", value, returns.getFalsyValue());
    }
  }

  /**
   * Represent a Compose Statement for an Imperative Extension
   */
  public static class Utility extends ComposeMethodStatement {

    static final String ERR_NULLABLE_NOT_SUPPORTED = "Nullable path is not supported for utility";

    /**
     * @param operand represents the imperative extension class
     * @param operation represents the static method being called on the imperative extension
     * @param isLastPredicateStatement a value representing whether this statement is the last
     *                                 statement of a predicate
     */
    public Utility(Operand operand, Operation operation, boolean isLastPredicateStatement) {
      super(operand, operation, operation.getReturnType(), isLastPredicateStatement);
      TypeProvider utilitiesContextType = new TypeUtilities.FromClass(UtamUtilitiesContext.class);
      getClassImports().add(utilitiesContextType);
    }

    /**
     * Method returning a template string that represents the main components of an imperative
     * extension statement
     *
     * @return a String that represents imperative extension statement structure
     * (ClassName.staticMethod)
     */
    @Override
    String getMethodCallString() {
      return "%s.%s";
    }

    @Override
    public boolean isUtilityMethodStatement() {
      return true;
    }

    @Override
    String getNullConditionCode(String value, TypeProvider returns,
        boolean isLastPredicateStatement) {
      throw new UtamCompilationError(ERR_NULLABLE_NOT_SUPPORTED);
    }
  }

  /**
   * invokes method on list and returns void
   */
  public static class VoidList extends ComposeMethodStatement {

    public VoidList(Operand operand, Operation operation, boolean isLastPredicateStatement) {
      super(operand, operation, VOID, isLastPredicateStatement);
    }

    @Override
    String getMethodCallString() {
      return "%s.forEach(element -> element.%s)";
    }

    @Override
    String getNullConditionCode(String value, TypeProvider returns,
        boolean isLastPredicateStatement) {
      if (isLastPredicateStatement) {
        return String.format("if (%s == null || %s.isEmpty()) { return false; }", value, value);
      }
      return String.format("if (%s == null || %s.isEmpty()) { return; }", value, value);
    }
  }

  /**
   * invokes method on list and returns list
   */
  public static final class ReturnsList extends ComposeMethodStatement {

    public ReturnsList(Operand operand, Operation operation, boolean isLastPredicateStatement) {
      super(operand, operation, new ListOf(operation.getReturnType()),
          isLastPredicateStatement);
      TypeProvider additionalImport = getReturnType();
      getImports().add(additionalImport);
      getClassImports().add(COLLECTOR_IMPORT);
      getClassImports().add(additionalImport);
    }

    @Override
    String getMethodCallString() {
      return "%s.stream().map(element -> element.%s).collect(Collectors.toList())";
    }

    @Override
    String getNullConditionCode(String value, TypeProvider returns,
        boolean isLastPredicateStatement) {
      if (isLastPredicateStatement) {
        return String.format("if (%s == null || %s.isEmpty()) { return %s; }", value, value,
            returns.getFalsyValue());
      }
      return String.format("if (%s == null || %s.isEmpty()) { return null; }", value, value);
    }
  }

  /**
   * information about element action is applied to
   */
  public static abstract class Operand {

    List<MethodParameter> getElementParameters() {
      return new ArrayList<>();
    }

    public boolean isList() {
      return false;
    }

    abstract String getOperandCode(List<String> codeLines,
        Function<String, String> getNullConditionCode);

    List<TypeProvider> getAddedClassImports() {
      return new ArrayList<>();
    }
  }

  public static class ElementOperand extends Operand {

    private final List<MethodParameter> parameters;
    private final ElementContext elementContext;
    private final boolean isElementAlreadyUsed;

    public ElementOperand(ElementContext elementContext, MethodContext methodContext) {
      this.elementContext = elementContext;
      this.isElementAlreadyUsed = methodContext.hasElement(elementContext.getName());
      if (isElementAlreadyUsed) {
        parameters = new ArrayList<>();
      } else {
        // remember that element is used to not propagate its parameters to method for second time
        // if element is already used in a previous statement, parameters were already added
        // should be done AFTER statement is created
        methodContext.setElementUsage(elementContext);
        parameters = elementContext.getParameters()
            .stream()
            .map(methodContext::setStatementArg)
            .collect(Collectors.toList());
      }
    }

    @Override
    String getOperandCode(List<String> codeLines, Function<String, String> getNullConditionCode) {
      if (elementContext.isNullable()) {
        String elementValue = getElementVariableName();
        // if element was not used in prev statements:
        // - add line like Type myElement = getMyElement();
        // add null check
        if (!isElementAlreadyUsed) {
          codeLines.add(String
              .format("%s %s = %s", getElementVariableType(), getElementVariableName(),
                  getElementGetterString()));
          codeLines.add(getNullConditionCode.apply(elementValue));
        }
        return elementValue;
      }
      return getElementGetterString();
    }

    private String getElementVariableName() {
      return elementContext.getName();
    }

    private String getElementVariableType() {
      if (elementContext.isList()) {
        return new ListOf(elementContext.getType()).getSimpleName();
      }
      return elementContext.getType().getSimpleName();
    }

    private String getElementGetterString() {
      List<MethodParameter> allParameters = elementContext.getParameters();
      String parameters = getParametersValuesString(allParameters);
      return String.format("this.%s(%s)", elementContext.getElementGetterName(), parameters);
    }

    @Override
    public boolean isList() {
      return elementContext.isList();
    }

    @Override
    List<MethodParameter> getElementParameters() {
      return parameters;
    }
  }

  /**
   * Information about an imperative extension class
   */
  public static class UtilityOperand extends Operand {

    private final TypeProvider type;

    /**
     * Creates a new UtilityOperand object.
     *
     * @param type holds information about the type of the utility class
     */
    public UtilityOperand(TypeProvider type) {
      this.type = type;
    }

    // the imperative utility class name from it's path (type property in the JSON statement)
    @Override
    String getOperandCode(List<String> codeLines, Function<String, String> getNullConditionCode) {
      return this.type.getSimpleName();
    }

    @Override
    List<TypeProvider> getAddedClassImports() {
      return Collections.singletonList(type);
    }
  }

  /**
   * operand for "element" : "self"
   */
  static class SelfOperand extends Operand {

    SelfOperand() {
    }

    @Override
    String getOperandCode(List<String> codeLines, Function<String, String> getNullConditionCode) {
      return "this";
    }
  }

  /**
   * operand for "element" : "document"
   */
  static class DocumentOperand extends Operand {

    private final ElementContext elementContext;

    DocumentOperand() {
      this.elementContext = Document.DOCUMENT_ELEMENT;
    }

    @Override
    String getOperandCode(List<String> codeLines, Function<String, String> getNullConditionCode) {
      return String.format("this.%s()", elementContext.getElementGetterName());
    }
  }

  /**
   * information about applied action
   */
  public static class Operation {

    final List<MethodParameter> actionParameters = new ArrayList<>();
    private final ActionType action;
    private final TypeProvider returnType;

    /**
     * @param action           method to invoke
     * @param returnType       returnType from action, for waitFor it's last predicate
     * @param actionParameters parameters for method invocation
     */
    public Operation(ActionType action, TypeProvider returnType,
        List<MethodParameter> actionParameters) {
      this.action = action;
      this.actionParameters.addAll(actionParameters);
      this.returnType = returnType;
    }

    List<MethodParameter> getActionParameters() {
      return actionParameters;
    }

    String getCode(String invocationPattern, String elementGetter) {
      String methodInvocation = String.format("%s(%s)",
          action.getApplyString(),
          getParametersValuesString(actionParameters));
      return String.format(invocationPattern, elementGetter, methodInvocation);
    }

    public boolean isReturnsVoid() {
      return VOID.isSameType(getReturnType());
    }

    final TypeProvider getReturnType() {
      return returnType;
    }

    final ActionType getAction() {
      return action;
    }

    List<TypeProvider> getAddedClassImports() {
      return new ArrayList<>();
    }

    List<TypeProvider> getAddedImports() {
      return new ArrayList<>();
    }
  }

  /**
   * information about applied action on imperative extension
   */
  public static class UtilityOperation extends Operation {

    /**
     * @param action           method to invoke
     * @param returnType       returnType from action
     * @param actionParameters parameters for method invocation
     */
    public UtilityOperation(ActionType action, TypeProvider returnType,
        List<MethodParameter> actionParameters) {
      super(action, returnType, actionParameters);

    }

    @Override
    List<TypeProvider> getAddedClassImports() {
      return getReturnTypeImportAsList();
    }

    @Override
    List<TypeProvider> getAddedImports() {
      return getReturnTypeImportAsList();
    }

    private List<TypeProvider> getReturnTypeImportAsList() {
      if (!(getReturnType() instanceof PrimitiveType)) {
        return Collections.singletonList(getReturnType());
      }
      return new ArrayList<>();
    }

    /**
     * Method invoke to construct the statement associated with a utility extension. Utility
     * statements are expressed as ClassName.staticMethod(this, [params])
     *
     * @param invocationPattern utility statement template string format
     * @param utilityClassName  utility class name
     * @return a string that represents the code for a utility compose statement
     */
    @Override
    String getCode(String invocationPattern, String utilityClassName) {
      String parametersValues = getParametersValuesString(actionParameters);
      String separator = parametersValues.length() > 0 ? ", " : "";
      String methodInvocation = String.format("%s(new %s(this)%s%s)",
          super.getAction().getApplyString(),
          UtamUtilitiesContext.class.getSimpleName(),
          separator,
          getParametersValuesString(actionParameters));
      return String.format(invocationPattern, utilityClassName, methodInvocation);
    }
  }

  public static class BasicElementOperation extends Operation {

    public BasicElementOperation(ActionType action, List<MethodParameter> actionParameters) {
      super(action, action.getReturnType(), actionParameters);
    }
  }

  /**
   * information about applied action with a predicate
   */
  public static class OperationWithPredicate extends Operation {

    final List<String> predicateCode = new ArrayList<>();
    private final List<TypeProvider> classImports = new ArrayList<>();
    private final List<TypeProvider> imports = new ArrayList<>();

    public OperationWithPredicate(ActionType action, TypeProvider returnType,
        List<ComposeMethodStatement> predicate) {
      super(action, returnType, new ArrayList<>());
      for (ComposeMethodStatement statement : predicate) {
        predicateCode.addAll(statement.getCodeLines());
        imports.addAll(statement.getImports());
        classImports.addAll(statement.getClassImports());
        actionParameters.addAll(statement.getParameters());
      }
    }

    @Override
    String getCode(String invocationPattern, String elementGetter) {
      String methodInvocation = String
          .format("%s(() -> {\n%s\n})", WAIT_FOR, String.join(";\n", predicateCode));
      return String.format(invocationPattern, elementGetter, methodInvocation);
    }

    @Override
    List<TypeProvider> getAddedClassImports() {
      return classImports;
    }

    @Override
    List<TypeProvider> getAddedImports() {
      return imports;
    }
  }
}
