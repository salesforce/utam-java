package utam.compiler.grammar;

import static utam.compiler.helpers.ActionableActionType.getActionType;
import static utam.compiler.helpers.ActionableActionType.isWaitFor;
import static utam.compiler.helpers.TypeUtilities.VOID;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.List;
import java.util.stream.Collectors;
import utam.compiler.helpers.ActionType;
import utam.compiler.helpers.ActionableActionType;
import utam.compiler.helpers.ElementContext;
import utam.compiler.helpers.MethodContext;
import utam.compiler.helpers.PrimitiveType;
import utam.compiler.helpers.TranslationContext;
import utam.compiler.representation.ComposeMethodStatement;
import utam.compiler.representation.ComposeMethodStatement.Operand;
import utam.compiler.representation.ComposeMethodStatement.Operation;
import utam.compiler.representation.ComposeMethodStatement.OperationWithPredicate;
import utam.compiler.representation.ComposeMethodStatement.Single;
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

  private static final String ERR_ACTION_CARDINALITY =
      "method '%s' can't be applied to the element '%s' "
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

  Operation getCustomOperation(TranslationContext context, MethodContext methodContext) {
    boolean isWaitFor = isWaitFor(apply);
    List<TypeProvider> expectedParameters =
        isWaitFor ? ActionableActionType.waitFor.getParametersTypes() : null;
    List<MethodParameter> parameters = UtamArgument
        .getArgsProcessor(args, expectedParameters, methodContext.getName()).getOrdered();
    // return type is irrelevant at statement level as we don't assign except for last statement
    ActionType action = new Custom(apply, methodContext.getReturnType(VOID), parameters);
    if (isWaitFor) {
      List<ComposeMethodStatement> predicate = args[0].getPredicate(context, methodContext);
      return new OperationWithPredicate(action, methodContext.getReturnType(predicate,
          PrimitiveType.BOOLEAN), predicate);
    }
    return new Operation(action, parameters);
  }

  Operation getBasicOperation(TranslationContext context, ElementContext element,
      MethodContext methodContext) {
    ActionType action = getActionType(apply, element.getType(), element.getName());
    List<MethodParameter> parameters = UtamArgument
        .getArgsProcessor(args, action.getParametersTypes(), methodContext.getName())
        .getOrdered();
    // action with list cardinality can only be applied to single element
    if (action.isSingleCardinality() && element.isList()) {
      throw new UtamError(
          String.format(ERR_ACTION_CARDINALITY, action.getApplyString(), elementName));
    }
    if (isWaitFor(apply)) {
      List<ComposeMethodStatement> predicate = args[0].getPredicate(context, methodContext);
      return new OperationWithPredicate(action, methodContext.getReturnType(predicate, PrimitiveType.BOOLEAN), predicate);
    }
    return new Operation(action, parameters);
  }

  ComposeMethodStatement getComposeAction(TranslationContext context, MethodContext methodContext) {
    ElementContext element = context.getElement(elementName);
    // register usage of getter from compose statement
    context.setPrivateMethodUsage(element.getElementMethod().getDeclaration().getName());

    ComposeMethodStatement.Operand operand = new Operand(element, methodContext);
    ComposeMethodStatement.Operation operation =
        element.isCustom() ? getCustomOperation(context, methodContext) :
            getBasicOperation(context, element, methodContext);
    ComposeMethodStatement statement;
    if (element.isList()) {
      statement =
          operation.isReturnsVoid() ? new ComposeMethodStatement.VoidList(operand, operation)
              : new ComposeMethodStatement.ReturnsList(operand, operation);
    } else {
      statement = new Single(operand, operation);
    }
    // remember that element is used to not propagate its parameters to method for second time
    methodContext.setElementUsage(element);
    return statement;
  }

  /**
   * custom action to be invoked on the element
   *
   * @since 232
   */
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
