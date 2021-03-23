package utam.compiler.grammar;

import static utam.compiler.helpers.ActionableActionType.getActionType;
import static utam.compiler.helpers.TypeUtilities.VOID;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import utam.compiler.helpers.ActionType;
import utam.compiler.helpers.ElementContext;
import utam.compiler.helpers.TranslationContext;
import utam.compiler.representation.ComposeMethodStatement;
import utam.compiler.representation.ComposeMethodStatement.Single;
import utam.compiler.representation.ComposeMethodStatement.Operand;
import utam.compiler.representation.ComposeMethodStatement.Operation;
import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.TypeProvider;
import utam.core.framework.consumer.UtamError;

/**
 * compose statement mapping
 *
 * @author elizaveta.ivanova
 * @since 228
 */
class UtamMethodAction {

  private static final String ERR_ACTION_CARDINALITY = "method '%s' can't be applied to the element '%s' "
      + "because it needs single instance and not a list";

  final String elementName;
  final String apply;
  final UtamArgument[] args;

  @JsonCreator
  UtamMethodAction(
      @JsonProperty(value = "element", required = true) String elementName,
      @JsonProperty(value = "apply", required = true) String apply,
      @JsonProperty(value = "args") UtamArgument[] args) {
    this.elementName = elementName;
    this.apply = apply;
    this.args = args;
  }

  ComposeMethodStatement getComposeAction(TranslationContext context, MethodContext methodContext,
      boolean isLastStatement) {
    ElementContext element = context.getElement(elementName);
    // register usage of getter from compose statement
    context.setPrivateMethodUsage(element.getElementMethod().getDeclaration().getName());
    String methodName = methodContext.methodName;

    ActionType action;
    List<MethodParameter> parameters;
    if (element.isCustom()) {
      parameters = UtamArgument.getArgsProcessor(args, null, methodName).getOrdered();
      // return type is irrelevant at statement level as we don't assign anything
      action = new Custom(apply, isLastStatement ? methodContext.methodReturnType : VOID,
          parameters);
    } else {
      action = getActionType(apply, element.getType(), element.getName());
      parameters = UtamArgument.getArgsProcessor(args, action.getParametersTypes(), methodName)
          .getOrdered();
    }
    boolean isListElement = element.isList();
    ComposeMethodStatement.Operand operand = new Operand(element,
        methodContext.elementNames.contains(elementName));
    // remember that element is used to not propagate its parameters to method for second time
    methodContext.elementNames.add(elementName);
    ComposeMethodStatement.Operation operation = new Operation(action, parameters);
    if (action.isSingleCardinality()) {
      // action with list cardinality can only be applied to single element
      if (isListElement) {
        throw new UtamError(
            String.format(ERR_ACTION_CARDINALITY, action.getApplyString(), elementName));
      }
      return new Single(operand, operation);
    }
    if (isListElement) {
      boolean isReturnVoid = action.getReturnType().isSameType(VOID);
      if (isReturnVoid) {
        return new ComposeMethodStatement.VoidList(operand, operation);
      }
      return new ComposeMethodStatement.ReturnsList(operand, operation);
    }
    return new Single(operand, operation);
  }

  static final class MethodContext {

    final String methodName;
    final TypeProvider methodReturnType;
    // to keep track of element usages
    final Set<String> elementNames = new HashSet<>();

    MethodContext(String methodName, TypeProvider methodReturnType) {
      this.methodName = methodName;
      this.methodReturnType = methodReturnType;
    }
  }

  static final class Custom implements ActionType {

    private final String methodName;
    private final List<TypeProvider> parametersTypes;
    private final TypeProvider returnType;

    Custom(String methodName, TypeProvider returnType, List<MethodParameter> parameters) {
      this.methodName = methodName;
      this.parametersTypes = parameters.stream().map(p -> p.getType()).collect(Collectors.toList());
      this.returnType = returnType;
    }

    @Override
    public TypeProvider getReturnType() {
      return returnType;
    }

    @Override
    public List<TypeProvider> getParametersTypes() {
      return parametersTypes;
    }

    @Override
    public String getApplyString() {
      return methodName;
    }
  }
}
