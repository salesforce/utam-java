/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
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
import utam.compiler.helpers.MatcherType;
import utam.compiler.helpers.MethodContext;
import utam.compiler.helpers.PrimitiveType;
import utam.compiler.helpers.TranslationContext;
import utam.compiler.representation.ComposeMethodStatement;
import utam.compiler.representation.ComposeMethodStatement.BasicElementOperation;
import utam.compiler.representation.ComposeMethodStatement.DocumentOperand;
import utam.compiler.representation.ComposeMethodStatement.Operand;
import utam.compiler.representation.ComposeMethodStatement.Operation;
import utam.compiler.representation.ComposeMethodStatement.OperationWithPredicate;
import utam.compiler.representation.ComposeMethodStatement.Single;
import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.TypeProvider;

/**
 * compose statement mapping
 *
 * @author elizaveta.ivanova
 * @since 228
 */
class UtamMethodAction {

  final String elementName;
  final String apply;
  UtamArgument[] args;
  private final UtamMatcher matcher;

  @JsonCreator
  UtamMethodAction(
      @JsonProperty(value = "element", required = true) String elementName,
      @JsonProperty(value = "apply", required = true) String apply,
      @JsonProperty(value = "matcher") UtamMatcher matcher,
      @JsonProperty(value = "args") UtamArgument[] args) {
    this.elementName = elementName;
    this.apply = apply;
    this.args = args;
    this.matcher = matcher;
  }

  // used in tests
  UtamMethodAction(String elementName, String apply) {
    this(elementName, apply, null, null);
  }

  private Operation getCustomOperation(TranslationContext context, MethodContext methodContext) {
    boolean isWaitFor = isWaitFor(apply);
    List<TypeProvider> expectedParameters =
        isWaitFor ? ActionableActionType.waitFor.getParametersTypes() : null;
    List<MethodParameter> parameters = UtamArgument
        .getArgsProcessor(args, expectedParameters, methodContext.getName()).getOrdered();
    // return type is irrelevant at statement level as we don't assign except for last statement
    ActionType action = new Custom(apply, methodContext.getReturnType(VOID),
        parameters);
    if (isWaitFor) {
      List<ComposeMethodStatement> predicate = args[0].getPredicate(context, methodContext);
      return new OperationWithPredicate(action, methodContext.getReturnType(predicate,
          PrimitiveType.BOOLEAN), predicate);
    }
    TypeProvider returnType = matcher == null? action.getReturnType() : PrimitiveType.BOOLEAN;
    return new Operation(action, returnType, parameters);
  }

  private Operation getBasicOperation(TranslationContext context, ElementContext element,
      MethodContext methodContext, boolean isLastPredicateStatement) {
    ActionType action = getActionType(apply, element.getType(), element.getName());
    List<MethodParameter> parameters = UtamArgument
        .getArgsProcessor(args, action.getParametersTypes(), methodContext.getName())
        .getOrdered();
    if (isWaitFor(apply)) {
      List<ComposeMethodStatement> predicate = args[0].getPredicate(context, methodContext);
      return new OperationWithPredicate(action, methodContext.getReturnType(predicate, null),
          predicate);
    }
    return new BasicElementOperation(action, parameters, isLastPredicateStatement);
  }

  ComposeMethodStatement getComposeAction(TranslationContext context, MethodContext methodContext, boolean isLastPredicateStatement) {
    ElementContext element = context.getElement(elementName);
    // register usage of getter from compose statement
    context.setPrivateMethodUsage(element.getElementMethod().getDeclaration().getName());

    ComposeMethodStatement.Operand operand = element.isDocumentElement()? new DocumentOperand() : new Operand(element, methodContext);
    ComposeMethodStatement.Operation operation =
        element.isCustom() ? getCustomOperation(context, methodContext) :
            getBasicOperation(context, element, methodContext, isLastPredicateStatement);
    ComposeMethodStatement statement;
    if (element.isList()
        // size() can only be applied to a single element
        && !operation.isSizeAction()) {
      statement =
          operation.isReturnsVoid() ? new ComposeMethodStatement.VoidList(operand, operation)
              : new ComposeMethodStatement.ReturnsList(operand, operation);
    } else {
      statement = new Single(operand, operation, getMatcherType(), getMatcherParameters(methodContext));
    }
    // remember that element is used to not propagate its parameters to method for second time
    methodContext.setElementUsage(element);
    return statement;
  }

  private List<MethodParameter> getMatcherParameters(MethodContext methodContext) {
    if (matcher == null) {
      return null;
    }
    return matcher.getParameters(String.format("method '%s'", methodContext.getName()));
  }

  private MatcherType getMatcherType() {
    if (matcher == null) {
      return null;
    }
    return this.matcher.matcherType;
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
      this.parametersTypes = parameters.stream().map(MethodParameter::getType).collect(Collectors.toList());
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
