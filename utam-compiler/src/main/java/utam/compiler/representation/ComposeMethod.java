package utam.compiler.representation;

import declarative.representation.MethodParameter;
import declarative.representation.PageObjectMethod;
import declarative.representation.TypeProvider;
import framework.consumer.UtamError;
import utam.compiler.helpers.ActionType;
import utam.compiler.helpers.ElementContext;
import utam.compiler.helpers.PrimitiveType;
import utam.compiler.helpers.TypeUtilities;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static utam.compiler.helpers.ParameterUtils.getParametersValuesString;
import static utam.compiler.helpers.TypeUtilities.COLLECTOR_IMPORT;
import static utam.compiler.helpers.TypeUtilities.LIST_IMPORT;

/**
 * business method is a sequence of internal method calls
 *
 * @author elizaveta.ivanova
 * @since 226
 */
public class ComposeMethod implements PageObjectMethod {

  private final String name;
  private final List<MethodParameter> parameters;
  private final List<String> code = new ArrayList<>();
  private final List<TypeProvider> classImports = new ArrayList<>();
  private final List<TypeProvider> imports = new ArrayList<>();
  private TypeProvider returns;
  private final String comments;

  public ComposeMethod(String name, List<ElementAction> statements, List<MethodParameter> parameters, String comments) {
    this.name = name;
    this.parameters = new ArrayList<>(parameters);
    statements.forEach(
        action -> {
          code.add(action.getCodeLine());
          imports.addAll(action.getImports());
          classImports.addAll(action.getClassImports());
          returns = action.getReturnType(); // set to return from last action
        });
    this.comments = comments;
  }

  static String getElementGetterString(ElementContext elementContext) {
    List<MethodParameter> allParameters = elementContext.getParameters();
    String methodName = elementContext.getElementMethod().getDeclaration().getName();
    String parameters = getParametersValuesString(allParameters);
    return "this." + String.format("%s(%s)", methodName, parameters);
  }

  private static String getCallString(
      ActionType actionType, List<MethodParameter> actionParameters) {
    return String.format("%s(%s)", actionType.getInvokeMethodName(), getParametersValuesString(actionParameters));
  }

  @Override
  public MethodDeclarationImpl getDeclaration() {
    return new MethodDeclarationImpl(name, parameters, returns, imports, comments);
  }

  @Override
  public List<TypeProvider> getClassImports() {
    return classImports;
  }

  @Override
  public List<String> getCodeLines() {
    return code;
  }

  @Override
  public boolean isPublic() {
    return true;
  }

  public static final class VoidListAction extends ElementAction {

    public VoidListAction(Set<String> elementNames, ElementContext element, ActionType action, List<MethodParameter> actionParameters) {
      super(elementNames, element, action, actionParameters);
    }

    @Override
    public TypeProvider getReturnType() {
      return PrimitiveType.VOID;
    }

    @Override
    String setMethodCallString(ElementContext element, ActionType action, List<MethodParameter> actionParameters) {
      String elementStr = getElementGetterString(element);
      return String.format(
          "%s.forEach(element -> element.%s)", elementStr, getCallString(action, actionParameters));
    }
  }

  public static final class SimpleListAction extends ElementAction {

    public SimpleListAction(Set<String> elementNames, ElementContext element, ActionType action, List<MethodParameter> actionParameters) {
      super(elementNames, element, action, actionParameters);
      if (!actionParameters.isEmpty()) {
        throw new UtamError("parameters are not supported for list action");
      }
    }

    @Override
    public TypeProvider getReturnType() {
      return returns;
    }

    @Override
    public List<TypeProvider> getClassImports() {
      return Stream.of(elementContext.getType()).collect(Collectors.toList());
    }

    @Override
    public String setMethodCallString(ElementContext element, ActionType action, List<MethodParameter> actionParameters) {
      return String.format(
          "%s.%s", getElementGetterString(element), getCallString(action, actionParameters));
    }
  }

  public static final class ListAction extends ElementAction {

    public ListAction(Set<String> elementNames, ElementContext element, ActionType action, List<MethodParameter> actionParameters) {
      super(elementNames, element, action, actionParameters);
    }

    @Override
    public TypeProvider getReturnType() {
      return new TypeUtilities.ListOf(returns);
    }

    @Override
    String setMethodCallString(ElementContext element, ActionType action, List<MethodParameter> actionParameters) {
      return String.format(
          "%s.stream().map(element -> element.%s).collect(Collectors.toList())",
          getElementGetterString(element), getCallString(action, actionParameters));
    }

    @Override
    public List<TypeProvider> getImports() {
      return Stream.of(LIST_IMPORT).collect(Collectors.toList());
    }

    @Override
    public List<TypeProvider> getClassImports() {
      return Stream.of(elementContext.getType(), LIST_IMPORT, COLLECTOR_IMPORT)
          .collect(Collectors.toList());
    }
  }

  public static class ElementAction {

    final TypeProvider returns;
    final ElementContext elementContext;
    private final String methodCallString;
    private final List<MethodParameter> parameters = new ArrayList<>();

    public ElementAction(Set<String> elementNames, ElementContext element, ActionType action, List<MethodParameter> actionParameters) {
      this.elementContext = element;
      this.returns = action.getReturnType();
      //this means that scope parameters were already added, same element can be used twice
      if (!elementNames.contains(element.getName())) {
        parameters.addAll(element.getParameters());
      }
      parameters.addAll(actionParameters);
      this.methodCallString = setMethodCallString(element, action, actionParameters);
      parameters.removeIf(MethodParameter::isLiteral);
    }

    String setMethodCallString(ElementContext element, ActionType action, List<MethodParameter> actionParameters) {
      return String.format(
          "%s.%s", getElementGetterString(element), getCallString(action, actionParameters));
    }

    public List<MethodParameter> getParameters() {
      return parameters;
    }

    public TypeProvider getReturnType() {
      return returns;
    }

    public String getCodeLine() {
      return methodCallString;
    }

    List<TypeProvider> getImports() {
      return TypeProvider.EMPTY_LIST;
    }

    List<TypeProvider> getClassImports() {
      return TypeProvider.EMPTY_LIST;
    }
  }
}
