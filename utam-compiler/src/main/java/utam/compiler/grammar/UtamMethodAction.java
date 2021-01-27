package utam.compiler.grammar;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import utam.compiler.representation.ComposeMethod;
import utam.core.declarative.representation.MethodParameter;
import utam.compiler.helpers.ActionType;
import utam.compiler.helpers.ElementContext;
import utam.compiler.helpers.PrimitiveType;
import utam.compiler.helpers.TranslationContext;

import java.util.List;
import java.util.Set;

import static utam.compiler.helpers.ActionableActionType.getActionType;

/**
 * compose statement mapping
 * @author elizaveta.ivanova
 * @since 228
 */
class UtamMethodAction {

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

  ComposeMethod.ElementAction getComposeAction(Set<String> elementNames, TranslationContext context, String methodName) {
    ElementContext element = context.getElement(elementName);
    ActionType action = getActionType(apply, element.getType(), element.getName());
    List<MethodParameter> parameters = UtamArgument.literalParameters(args, action.getParametersTypes(), methodName).getOrdered();
    boolean isListAction = element.isList();
    ComposeMethod.ElementAction res;
    if (isListAction && action.getReturnType() == PrimitiveType.VOID) {
      res = new ComposeMethod.VoidListAction(elementNames, element, action, parameters);
    } else if (action.isListAction()) { // should be BEFORE next condition check
      res = new ComposeMethod.SimpleListAction(elementNames, element, action, parameters);
    } else if (isListAction) {
      res = new ComposeMethod.ListAction(elementNames, element, action, parameters);
    } else {
      res = new ComposeMethod.ElementAction(elementNames, element, action, parameters);
    }
    elementNames.add(elementName); //remember element
    // register usage of getter
    String elementGetter = element.getElementMethod().getDeclaration().getName();
    context.setPrivateMethodUsage(elementGetter);
    return res;
  }
}
