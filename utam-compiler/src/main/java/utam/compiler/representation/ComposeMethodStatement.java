/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.representation;

import static utam.compiler.helpers.ParameterUtils.getParametersValuesString;
import static utam.compiler.helpers.TypeUtilities.COLLECTOR_IMPORT;
import static utam.compiler.helpers.TypeUtilities.LIST_IMPORT;
import static utam.compiler.helpers.TypeUtilities.SELECTOR;
import static utam.compiler.helpers.TypeUtilities.VOID;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import utam.compiler.helpers.ActionType;
import utam.compiler.helpers.ActionableActionType;
import utam.compiler.helpers.ElementContext;
import utam.compiler.helpers.MatcherType;
import utam.compiler.helpers.MethodContext;
import utam.compiler.helpers.PrimitiveType;
import utam.compiler.helpers.TypeUtilities;
import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.TypeProvider;

/**
 * single statement in a compose method, all statements are
 *
 * @author elizaveta.ivanova
 * @since 232
 */
public abstract class ComposeMethodStatement {

  private static final String PREDICATE_RETURN_TRUE = "return true;";
  final List<TypeProvider> classImports = new ArrayList<>();
  final List<TypeProvider> imports = new ArrayList<>();
  private final List<String> codeLines = new ArrayList<>();
  private final List<MethodParameter> parameters = new ArrayList<>();
  private final TypeProvider returns;

  ComposeMethodStatement(Operand operand,
      Operation operation,
      TypeProvider returnType,
      MatcherType matcher,
      List<MethodParameter> matcherParameters) {
    this.returns = matcher != null ? PrimitiveType.BOOLEAN : returnType;
    operand.setElementParameters(this.parameters);
    operation.setParameters(this.parameters);
    operation.setImports(this.imports);
    operation.setClassImports(this.classImports);
    if (matcherParameters != null) {
      this.parameters.addAll(matcherParameters);
    }
    parameters.removeIf(p -> p == null || p.isLiteral());
    String invocationStr = operation
        .getCode(getMethodCallString(), operand.getElementGetterString());
    if (matcher != null) {
      codeLines.add(matcher.getCode(matcherParameters, invocationStr));
    } else {
      codeLines.add(invocationStr);
    }
  }

  ComposeMethodStatement(Operand operand, Operation operation, TypeProvider returnType) {
    this(operand, operation, returnType, null, null);
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

  List<TypeProvider> getImports() {
    return imports;
  }

  List<TypeProvider> getClassImports() {
    return classImports;
  }

  abstract String getMethodCallString();

  /**
   * invokes method on a single element
   */
  public static class Single extends ComposeMethodStatement {

    public Single(Operand operand, Operation operation, MatcherType matcher,
        List<MethodParameter> matcherParameters) {
      super(operand, operation, matcher == null ? operation.getReturnType() : PrimitiveType.BOOLEAN,
          matcher, matcherParameters);
    }

    Single(Operand operand, Operation operation) {
      this(operand, operation, null, null);
    }

    @Override
    String getMethodCallString() {
      return "%s.%s";
    }
  }

  /**
   * Represent a Compose Statement for an Imperative Extension
   */
  public static class Utility extends ComposeMethodStatement {

    /**
     * @param operand - Represent the imperative extension class
     * @param operation - Represent the static method being called on the imperative extension
     */
    public Utility(UtilityOperand operand, Operation operation) {
      super(operand, operation, operation.getReturnType());
      classImports.add(operand.getType());
      TypeProvider utilitiesContextType = new TypeUtilities.FromClass(UtamUtilitiesContext.class);
      classImports.add(utilitiesContextType);
    }

    /**
     * Method returning a template string that represents the main components of an imperative extension statement
     * @return a String that represents imperative extension statement structure (ClassName.staticMethod)
     */
    @Override
    String getMethodCallString() {
      return "%s.%s";
    }
  }

  /**
   * invokes method on list and returns void
   */
  public static class VoidList extends ComposeMethodStatement {

    public VoidList(Operand operand, Operation operation) {
      super(operand, operation, VOID);
    }

    @Override
    String getMethodCallString() {
      return "%s.forEach(element -> element.%s)";
    }
  }

  /**
   * invokes method on list and returns list
   */
  public static final class ReturnsList extends ComposeMethodStatement {

    public ReturnsList(Operand operand, Operation operation) {
      super(operand, operation, new TypeUtilities.ListOf(operation.getReturnType()));
      imports.add(LIST_IMPORT);
      imports.add(operation.action.getReturnType());
      classImports.addAll(imports);
      classImports.add(COLLECTOR_IMPORT);
    }

    @Override
    String getMethodCallString() {
      return "%s.stream().map(element -> element.%s).collect(Collectors.toList())";
    }
  }

  /**
   * information about element action is applied to
   */
  public static class Operand {

    private final ElementContext elementContext;
    //this means that scope parameters were already added, same element can be used twice
    private final MethodContext methodContext;

    /**
     * @param elementContext element action is applied to
     * @param methodContext  methodContext to track elements
     */
    public Operand(ElementContext elementContext, MethodContext methodContext) {
      this.elementContext = elementContext;
      this.methodContext = methodContext;
    }

    // used in tests
    Operand(ElementContext elementContext) {
      this(elementContext, new MethodContext("test", null, false));
    }

    String getElementGetterString() {
      List<MethodParameter> allParameters = elementContext.getParameters();
      String methodName = elementContext.getElementMethod().getDeclaration().getName();
      String parameters = getParametersValuesString(allParameters);
      return String.format("this.%s(%s)", methodName, parameters);
    }

    void setElementParameters(List<MethodParameter> parameters) {
      if (methodContext.hasElement(elementContext.getName())) {
        return;
      }
      parameters.addAll(elementContext.getParameters());
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
     * @param methodContext context of the current method
     * @param type          holds information about the type of the utility class
     */
    public UtilityOperand(MethodContext methodContext, TypeProvider type) {
      super(null, methodContext);
      this.type = type;
    }

    /**
     * @return the imperative utility class name from it's path (type property in the JSON statement)
     */
    @Override String getElementGetterString() {
      return this.type.getSimpleName();
    }

    /**
     * Stub method that doesn't do anything
     *
     * @param parameters list of parameters associated with the utility method
     */
    @Override void setElementParameters(List<MethodParameter> parameters) {
    }

    /**
     * Getter that returns the type of the utility class. Used to add imports for the utility class
     *
     * @return the type field
     */
    public TypeProvider getType() {
      return type;
    }
  }

  /**
   * operand for "element" : "document"
   */
  public static class DocumentOperand extends Operand {

    public DocumentOperand() {
      super(null, null);
    }

    @Override
    String getElementGetterString() {
      return "this.getDocument()";
    }

    @Override
    void setElementParameters(List<MethodParameter> parameters) {
      // document does not have parameters
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

    void setParameters(List<MethodParameter> parameters) {
      parameters.addAll(actionParameters);
    }

    String getCode(String invocationPattern, String elementGetter) {
      String methodInvocation = String.format("%s(%s)",
          action.getInvokeMethodName(),
          getParametersValuesString(actionParameters));
      return String.format(invocationPattern, elementGetter, methodInvocation);
    }

    void setImports(List<TypeProvider> imports) {
      // parameters can be non primitive like Selector
      actionParameters
          .stream()
          .filter(p -> p != null && !p.isLiteral())
          .forEach(p -> imports.add(p.getType()));
    }

    void setClassImports(List<TypeProvider> imports) {
      // parameters can be non primitive like Selector
      setImports(imports);
      actionParameters
          .stream()
          .filter(p -> p != null && p.isLiteral())
          .forEach(p -> {
            if (SELECTOR.isSameType(p.getType())) {
              imports.add(p.getType());
            }
          });
    }

    public boolean isSizeAction() {
      return action.equals(ActionableActionType.size);
    }

    public boolean isReturnsVoid() {
      return VOID.isSameType(getReturnType());
    }

    TypeProvider getReturnType() {
      return returnType;
    }

    public ActionType getAction() {
      return action;
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
    public UtilityOperation(ActionType action, TypeProvider returnType, List<MethodParameter> actionParameters) {
      super(action, returnType, actionParameters);
    }

    /**
     * Method invoke to construct the statement associated with a utility extension.
     * Utility statements are expressed as ClassName.staticMethod(this, [params])
     * @param invocationPattern utility statement template string format
     * @param utilityClassName utility class name
     * @return a string that represents the code for a utility compose statement
     */
    @Override
    String getCode(String invocationPattern, String utilityClassName) {
      String parametersValues = getParametersValuesString(actionParameters);
      String separator = parametersValues.length() > 0  ? ", " : "";
      String methodInvocation = String.format("%s(new UtamUtilitiesContext(this)%s%s)",
              super.getAction().getInvokeMethodName(),
              separator,
              getParametersValuesString(actionParameters));
      return String.format(invocationPattern, utilityClassName, methodInvocation);
    }
  }

  public static class BasicElementOperation extends Operation {

    private final boolean isLastPredicateStatement;

    public BasicElementOperation(ActionType action,
        List<MethodParameter> actionParameters, boolean isLastPredicateStatement) {
      super(action,
          isLastPredicateStatement && MethodContext.isNullOrVoid(action.getReturnType())
              ? PrimitiveType.BOOLEAN
              : action.getReturnType(),
          actionParameters);
      this.isLastPredicateStatement =
          isLastPredicateStatement && MethodContext.isNullOrVoid(action.getReturnType());
    }

    @Override
    String getCode(String invocationPattern, String elementGetter) {
      String code = super.getCode(invocationPattern, elementGetter);
      if (isLastPredicateStatement) {
        // add last statement of predicate can't return null, has to return boolean or original type
        return String.format("%s;\n%s", code, PREDICATE_RETURN_TRUE);
      }
      return code;
    }
  }

  /**
   * information about applied action with a predicate
   */
  public static class OperationWithPredicate extends Operation {

    final List<String> predicateCode = new ArrayList<>();
    final List<TypeProvider> classImports = new ArrayList<>();
    final List<TypeProvider> imports = new ArrayList<>();

    public OperationWithPredicate(ActionType action, TypeProvider returnType,
        List<ComposeMethodStatement> predicate) {
      super(action, returnType, Collections.EMPTY_LIST);
      for (ComposeMethodStatement statement : predicate) {
        predicateCode.addAll(statement.getCodeLines());
        imports.addAll(statement.getImports());
        classImports.addAll(statement.getClassImports());
        actionParameters.addAll(statement.getParameters());
      }
      int lastStatementIndex = predicateCode.size() - 1;
      if (!predicateCode.get(lastStatementIndex).endsWith(PREDICATE_RETURN_TRUE)) {
        // last statement should be return, unless it's void action and 'return true' was already added
        predicateCode
            .set(lastStatementIndex,
                String.format("return %s;", predicateCode.get(lastStatementIndex)));
      }
    }

    @Override
    String getCode(String invocationPattern, String elementGetter) {
      String methodInvocation = String
          .format("waitFor(() -> {\n%s\n})", String.join(";\n", predicateCode));
      return String.format(invocationPattern, elementGetter, methodInvocation);
    }

    @Override
    void setImports(List<TypeProvider> imports) {
      super.setImports(imports);
      imports.addAll(this.imports);
    }

    @Override
    void setClassImports(List<TypeProvider> imports) {
      super.setClassImports(imports);
      imports.addAll(this.classImports);
    }
  }
}
