package utam.compiler.representation;

import static utam.compiler.helpers.ParameterUtils.getParametersValuesString;
import static utam.compiler.helpers.TypeUtilities.COLLECTOR_IMPORT;
import static utam.compiler.helpers.TypeUtilities.LIST_IMPORT;
import static utam.compiler.helpers.TypeUtilities.VOID;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import utam.compiler.helpers.ActionType;
import utam.compiler.helpers.ElementContext;
import utam.compiler.helpers.MethodContext;
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

  final List<TypeProvider> classImports = new ArrayList<>();
  final List<TypeProvider> imports = new ArrayList<>();
  final List<String> codeLines = new ArrayList<>();
  private final TypeProvider returns;
  private final List<MethodParameter> parameters = new ArrayList<>();

  ComposeMethodStatement(Operand operand, Operation operation, TypeProvider returnType) {
    this.returns = returnType;
    operand.setParameters(this.parameters);
    operation.setParameters(this.parameters);
    operation.setImports(this.imports);
    operation.setClassImports(this.classImports);
    parameters.removeIf(p -> p == null || p.isLiteral());
    codeLines
        .addAll(operation.getCodeLines(getMethodCallString(), operand.getElementGetterString()));
  }

  public List<MethodParameter> getParameters() {
    return parameters;
  }

  public TypeProvider getReturnType() {
    return returns;
  }

  public TypeProvider getReturnType(TypeProvider defaultReturn) {
    if(returns == null) {
      return defaultReturn;
    }
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

    Single(Operand operand, Operation operation, TypeProvider returnType) {
      super(operand, operation, returnType);
    }

    public Single(Operand operand, Operation operation) {
      this(operand, operation, operation.getReturnType());
    }

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
      this(elementContext, new MethodContext());
    }

    String getElementGetterString() {
      List<MethodParameter> allParameters = elementContext.getParameters();
      String methodName = elementContext.getElementMethod().getDeclaration().getName();
      String parameters = getParametersValuesString(allParameters);
      return "this." + String.format("%s(%s)", methodName, parameters);
    }

    void setParameters(List<MethodParameter> parameters) {
      if (methodContext.hasElement(elementContext.getName())) {
        return;
      }
      parameters.addAll(elementContext.getParameters());
    }
  }

  /**
   * information about applied action
   */
  public static class Operation {

    final List<MethodParameter> actionParameters;
    private final ActionType action;
    private final TypeProvider returnType;

    /**
     * @param action           method to invoke
     * @param returnType       returnType from action, for waitFor it's last predicate
     * @param actionParameters parameters for method invocation
     */
    Operation(ActionType action, TypeProvider returnType, List<MethodParameter> actionParameters) {
      this.action = action;
      this.actionParameters = actionParameters;
      this.returnType = returnType == null? action.getReturnType() : returnType;
    }

    public Operation(ActionType action, List<MethodParameter> actionParameters) {
      this(action, null, actionParameters);
    }

    // used in tests
    Operation(ActionType action) {
      this(action, Collections.EMPTY_LIST);
    }

    void setParameters(List<MethodParameter> parameters) {
      parameters.addAll(actionParameters);
    }

    List<String> getCodeLines(String invocationPattern, String elementGetter) {
      String methodInvocation = String.format("%s(%s)", action.getInvokeMethodName(),
          getParametersValuesString(actionParameters));
      String code = String.format(invocationPattern, elementGetter, methodInvocation);
      return Collections.singletonList(code);
    }

    void setImports(List<TypeProvider> imports) {
    }

    void setClassImports(List<TypeProvider> imports) {
    }

    public boolean isReturnsVoid() {
      return getReturnType().isSameType(VOID);
    }

    public TypeProvider getReturnType() {
      return returnType;
    }
  }

  /**
   * information about applied action with a predicate
   */
  public static class OperationWithPredicate extends Operation {

    final List<String> predicateCode = new ArrayList<>();
    final List<TypeProvider> classImports = new ArrayList<>();
    final List<TypeProvider> imports = new ArrayList<>();

    public OperationWithPredicate(ActionType action, TypeProvider returnType, List<ComposeMethodStatement> predicate) {
      super(action, returnType, Collections.EMPTY_LIST);
      for (int i = 0; i < predicate.size(); i++) {
        ComposeMethodStatement statement = predicate.get(i);
        predicateCode.addAll(statement.getCodeLines());
        imports.addAll(statement.getImports());
        classImports.addAll(statement.getClassImports());
        actionParameters.addAll(statement.getParameters());
      }
      int lastStatement = predicateCode.size() -1;
      predicateCode.set(lastStatement, String.format("return %s;", predicateCode.get(lastStatement)));
    }

    @Override
    List<String> getCodeLines(String invocationPattern, String elementGetter) {
      String methodInvocation = String
          .format("waitFor(() -> { \n%s \n}", String.join("; \n", predicateCode));
      String code = String.format(invocationPattern, elementGetter, methodInvocation);
      return Collections.singletonList(code);
    }

    @Override
    void setImports(List<TypeProvider> imports) {
      imports.addAll(this.imports);
    }

    @Override
    void setClassImports(List<TypeProvider> imports) {
      imports.addAll(this.classImports);
    }
  }
}
