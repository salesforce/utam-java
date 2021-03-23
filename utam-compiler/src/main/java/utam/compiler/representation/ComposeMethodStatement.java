package utam.compiler.representation;

import static utam.compiler.helpers.ParameterUtils.getParametersValuesString;
import static utam.compiler.helpers.TypeUtilities.COLLECTOR_IMPORT;
import static utam.compiler.helpers.TypeUtilities.LIST_IMPORT;
import static utam.compiler.helpers.TypeUtilities.VOID;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import utam.compiler.helpers.ActionType;
import utam.compiler.helpers.ElementContext;
import utam.compiler.helpers.TypeUtilities;
import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.TypeProvider;

/**
 * single statement in a compose method,
 * all statements are
 *
 * @author elizaveta.ivanova
 * @since 232
 */
public abstract class ComposeMethodStatement {

  private final TypeProvider returns;
  final List<TypeProvider> classImports = new ArrayList<>();
  final List<TypeProvider> imports = new ArrayList<>();
  private final List<MethodParameter> parameters = new ArrayList<>();
  final Operand operand;
  final Operation operation;

  ComposeMethodStatement(Operand operand, Operation operation, TypeProvider returnType) {
    this.returns = returnType;
    this.operand = operand;
    this.operation = operation;
    if (!operand.isAlreadyUsed) {
      parameters.addAll(operand.elementContext.getParameters());
    }
    parameters.addAll(operation.actionParameters);
    parameters.removeIf(MethodParameter::isLiteral);
  }

  abstract String getMethodCallString();

  public List<MethodParameter> getParameters() {
    return parameters;
  }

  public TypeProvider getReturnType() {
    return returns;
  }

  public List<String> getCodeLines() {
    String invokeAction = String
        .format(getMethodCallString(), operand.getElementGetterString(), operation.getCallString());
    return Collections.singletonList(invokeAction);
  }

  List<TypeProvider> getImports() {
    return imports;
  }

  List<TypeProvider> getClassImports() {
    return classImports;
  }

  /**
   * invokes method on a single element
   */
  public static class Single extends ComposeMethodStatement {

    public Single(Operand operand, Operation operation) {
      super(operand, operation, operation.action.getReturnType());
    }

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
      super(operand, operation, new TypeUtilities.ListOf(operation.action.getReturnType()));
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

    final ElementContext elementContext;
    //this means that scope parameters were already added, same element can be used twice
    final boolean isAlreadyUsed;

    /**
     * @param elementContext element action is applied to
     * @param isAlreadyUsed  true is it's not first statement with the element, to not reuse
     *                       parameters
     */
    public Operand(ElementContext elementContext, boolean isAlreadyUsed) {
      this.elementContext = elementContext;
      this.isAlreadyUsed = isAlreadyUsed;
    }

    // used in tests
    Operand(ElementContext elementContext) {
      this(elementContext, false);
    }

    String getElementGetterString() {
      List<MethodParameter> allParameters = elementContext.getParameters();
      String methodName = elementContext.getElementMethod().getDeclaration().getName();
      String parameters = getParametersValuesString(allParameters);
      return "this." + String.format("%s(%s)", methodName, parameters);
    }
  }

  /**
   * information about applied action
   */
  public static class Operation {

    final ActionType action;
    final List<MethodParameter> actionParameters;

    /**
     * @param action           method to invoke
     * @param actionParameters parameters for method invocation
     */
    public Operation(ActionType action, List<MethodParameter> actionParameters) {
      this.action = action;
      this.actionParameters = actionParameters;
    }

    // used in tests
    Operation(ActionType action) {
      this(action, Collections.EMPTY_LIST);
    }

    String getCallString() {
      return String.format("%s(%s)", action.getInvokeMethodName(),
          getParametersValuesString(actionParameters));
    }
  }
}
