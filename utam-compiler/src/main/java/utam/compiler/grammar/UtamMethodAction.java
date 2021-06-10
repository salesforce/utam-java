/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import static utam.compiler.helpers.ActionableActionType.getActionType;
import static utam.compiler.helpers.ActionableActionType.size;
import static utam.compiler.helpers.TypeUtilities.FUNCTION;
import static utam.compiler.helpers.TypeUtilities.VOID;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import utam.compiler.UtamCompilationError;
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
import utam.compiler.representation.ComposeMethodStatement.SelfOperand;
import utam.compiler.representation.ComposeMethodStatement.Utility;
import utam.compiler.representation.ComposeMethodStatement.UtilityOperand;
import utam.compiler.representation.ComposeMethodStatement.UtilityOperation;
import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.TypeProvider;
import utam.core.framework.consumer.UtamError;

/**
 * Compose statement mapping
 *
 * @author elizaveta.ivanova
 * @since 228
 */
class UtamMethodAction {

  private static final String WAIT_FOR = "waitFor";
  private static final String SELF_ELEMENT = "self";

  private static final String ERR_COMPOSE_ACTION_REQUIRED_KEYS =
      "compose method '%s': statement should either have 'apply' or 'applyExternal' properties";

  private static final String ERR_COMPOSE_ACTION_REDUNDANT_KEYS =
      "compose method '%s': statement should not have both 'apply' and 'applyExternal' properties";

  private static final String ERR_COMPOSE_ACTION_REDUNDANT_ELEMENT =
      "compose method '%s': utility statement should not have 'element' property";

  private static final String ERR_COMPOSE_WAIT_FOR_INCORRECT_OPERAND =
      "compose method '%s': waitFor cannot be applied to document or to an element marked as a list";

  final String elementName;
  final String apply;
  private final UtamMatcher matcher;
  UtamArgument[] args;
  final UtamUtilityMethodAction applyExternal;

  @JsonCreator
  UtamMethodAction(
      @JsonProperty(value = "element") String elementName,
      @JsonProperty(value = "apply") String apply,
      @JsonProperty(value = "args") UtamArgument[] args,
      @JsonProperty(value = "matcher") UtamMatcher matcher,
      @JsonProperty(value = "applyExternal") UtamUtilityMethodAction applyExternal) {
    this.elementName = elementName;
    this.apply = apply;
    this.args = args;
    this.matcher = matcher;
    this.applyExternal = applyExternal;
  }

  // used in tests
  UtamMethodAction(String elementName, String apply) {
    this(elementName, apply, null, null, null);
  }

  // used in tests
  UtamMethodAction(UtamUtilityMethodAction applyExternal) {
    this(null, null, null, null, applyExternal);
  }

  private Operation getCustomOperation(MethodContext methodContext) {
    List<MethodParameter> parameters = UtamArgument
        .getArgsProcessor(args, null, methodContext.getName()).getOrdered();
    // return type is irrelevant at statement level as we don't assign except for last statement
    ActionType action = new Custom(apply, methodContext.getReturnType(VOID), parameters);
    TypeProvider returnType = matcher == null ? action.getReturnType() : PrimitiveType.BOOLEAN;
    return new Operation(action, returnType, parameters);
  }

  private Operation getWaitForOperation(TranslationContext context, MethodContext methodContext) {
    List<TypeProvider> expectedParameters = Collections.singletonList(FUNCTION);
    List<MethodParameter> parameters = UtamArgument
        .getArgsProcessor(args, expectedParameters, methodContext.getName()).getOrdered();
    // return type is irrelevant at statement level as we don't assign except for last statement
    ActionType action = new Custom(apply, methodContext.getReturnType(VOID), parameters);
    List<ComposeMethodStatement> predicate = args[0].getPredicate(context, methodContext);
    return new OperationWithPredicate(action, methodContext.getReturnType(predicate,
        PrimitiveType.BOOLEAN), predicate);
  }

  private Operation getBasicOperation(ElementContext element, MethodContext methodContext,
      boolean isLastPredicateStatement) {
    ActionType action = getActionType(apply, element.getType(), element.getName());
    if (ActionableActionType.containsElement.getApplyString().equals(apply) && args.length == 1) {
      // If the action is "containsElement", it may have one argument (a selector),
      // or two arguments (a selector and a boolean indicating whether to search in
      // the shadow DOM) declared in the JSON. If the second argument is omitted,
      // it can be assumed to be false, so substitute that value here.
      args = new UtamArgument[]{args[0], new UtamArgument(Boolean.FALSE)};
    }
    List<MethodParameter> parameters = UtamArgument
        .getArgsProcessor(args, action.getParametersTypes(), methodContext.getName())
        .getOrdered();
    return new BasicElementOperation(action, parameters, isLastPredicateStatement);
  }

  /**
   * Create an Operation for a utility statement. For imperative extension, an operation is a custom
   * action specified in the invoke property in the JSON file. We access the value of the invoke
   * property through `applyExternal.getMethodName()`
   *
   * @param methodContext context of the current method being compiled
   * @return an operation that represents the structure of an imperative extension statement
   */
  private Operation getUtilityOperation(MethodContext methodContext) {
    List<MethodParameter> parameters = UtamArgument
        .getArgsProcessor(applyExternal.args, null, methodContext.getName()).getOrdered();
    ActionType action = new Custom(applyExternal.getMethodName(), methodContext.getReturnType(VOID),
        parameters);
    TypeProvider returnType = action.getReturnType();
    return new UtilityOperation(action, returnType, parameters);
  }

  /**
   * Create a compose statement object from mapped Java entity. This method creates a structure that
   * will be used to generate the code for a given object in the `compose` array. Each object in the
   * `compose` array from the JSON PO will create one `ComposeMethodStatement`.
   *
   * @param context                  current PO context
   * @param methodContext            context of the current method being compiled
   * @param isLastPredicateStatement boolean that adds extra logic if the last statement is a
   *                                 predicate
   * @return compose method statement
   */
  ComposeMethodStatement getComposeAction(TranslationContext context, MethodContext methodContext,
      boolean isLastPredicateStatement) {
    // either "apply" or "applyExternal" should be set
    if (apply == null && applyExternal == null) {
      throw new UtamError(
          String.format(
              ERR_COMPOSE_ACTION_REQUIRED_KEYS,
              methodContext.getName()
          )
      );
    }

    // both "apply" and "applyExternal" can't be set
    if (apply != null && applyExternal != null) {
      throw new UtamError(
          String.format(
              ERR_COMPOSE_ACTION_REDUNDANT_KEYS,
              methodContext.getName()
          )
      );
    }

    if (applyExternal != null) {
      if (elementName != null) {
        throw new UtamError(
            String.format(
                ERR_COMPOSE_ACTION_REDUNDANT_ELEMENT,
                methodContext.getName()
            )
        );
      }
      return getUtilityStatement(context, methodContext);
    }

    if (isWaitFor()) {
      return getWaitForStatement(context, methodContext);
    }

    if (isSelfElement()) {
      return new ComposeMethodStatement.Single(new SelfOperand(),
          getCustomOperation(methodContext));
    }

    ElementContext element = context.getElement(elementName);

    if (element.isDocumentElement()) {
      return new ComposeMethodStatement.Single(new DocumentOperand(),
          getCustomOperation(methodContext));
    }

    // register usage of getter from compose statement
    context.setPrivateMethodUsage(element.getElementMethod().getDeclaration().getName());

    ComposeMethodStatement.Operand operand = new Operand(element, methodContext);

    if (isSizeAction()) {
      ComposeMethodStatement.Operation operation = new BasicElementOperation(size,
          Collections.EMPTY_LIST, isLastPredicateStatement);
      return new ComposeMethodStatement.Single(operand, operation);
    }
    ComposeMethodStatement.Operation operation = element.isCustom() ?
        getCustomOperation(methodContext) :
        getBasicOperation(element, methodContext, isLastPredicateStatement);

    if (element.isList()) {
      return
          operation.isReturnsVoid() ? new ComposeMethodStatement.VoidList(operand, operation)
              : new ComposeMethodStatement.ReturnsList(operand, operation);
    }
    return new ComposeMethodStatement.Single(operand, operation, getMatcherType(),
        getMatcherParameters(methodContext));


  }

  private ComposeMethodStatement getUtilityStatement(TranslationContext context,
      MethodContext methodContext) {
    TypeProvider type = context.getUtilityType(applyExternal.getExternalClassPath());
    UtilityOperand utilityOperand = new UtilityOperand(type);
    ComposeMethodStatement.Operation operation = getUtilityOperation(methodContext);
    return new Utility(utilityOperand, operation);
  }

  private ComposeMethodStatement getWaitForStatement(TranslationContext context,
      MethodContext methodContext) {
    Operand operand;
    // for waitFor operand is irrelevant, but we do not validate for it to be redundant
    if (isSelfElement()) {
      operand = new SelfOperand();
    } else {
      ElementContext element = context.getElement(elementName);
      if (element.isList() || element.isDocumentElement()) {
        throw new UtamCompilationError(String.format(
            ERR_COMPOSE_WAIT_FOR_INCORRECT_OPERAND,
            methodContext.getName()
        ));
      }
      operand = new Operand(element, methodContext);
    }
    return new ComposeMethodStatement.Single(operand, getWaitForOperation(context, methodContext));
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

  private boolean isWaitFor() {
    return WAIT_FOR.equals(apply);
  }

  private boolean isSizeAction() {
    return size.getInvokeMethodName().equals(apply);
  }

  private boolean isSelfElement() {
    return elementName == null || SELF_ELEMENT.equals(elementName);
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
      this.parametersTypes = parameters.stream().map(MethodParameter::getType)
          .collect(Collectors.toList());
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
