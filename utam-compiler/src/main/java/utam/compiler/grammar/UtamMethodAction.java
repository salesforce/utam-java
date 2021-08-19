/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import static utam.compiler.helpers.BasicElementActionType.getActionType;
import static utam.compiler.helpers.BasicElementActionType.size;
import static utam.compiler.helpers.ElementContext.SELF_ELEMENT_NAME;
import static utam.compiler.helpers.TypeUtilities.FUNCTION;
import static utam.compiler.helpers.TypeUtilities.VOID;
import static utam.compiler.representation.ComposeMethodStatement.WAIT_FOR;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import utam.compiler.grammar.UtamArgument.ArgsProcessor;
import utam.compiler.grammar.UtamArgument.ArgsProcessorBasicAction;
import utam.compiler.grammar.UtamArgument.ArgsProcessorWithExpectedTypes;
import utam.compiler.helpers.ActionType;
import utam.compiler.helpers.ElementContext;
import utam.compiler.helpers.MatcherType;
import utam.compiler.helpers.MethodContext;
import utam.compiler.helpers.PrimitiveType;
import utam.compiler.helpers.TranslationContext;
import utam.compiler.representation.ComposeMethodStatement;
import utam.compiler.representation.ComposeMethodStatement.BasicElementOperation;
import utam.compiler.representation.ComposeMethodStatement.ElementOperand;
import utam.compiler.representation.ComposeMethodStatement.Operand;
import utam.compiler.representation.ComposeMethodStatement.Operation;
import utam.compiler.representation.ComposeMethodStatement.OperationWithPredicate;
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

  static final String ERR_COMPOSE_ACTION_REQUIRED_KEYS =
      "compose method '%s': statement should either have 'apply' or 'applyExternal' properties";
  static final String ERR_COMPOSE_ACTION_REDUNDANT_KEYS =
      "compose method '%s': statement should not have both 'apply' and 'applyExternal' properties";
  static final String ERR_COMPOSE_ACTION_REDUNDANT_ELEMENT =
      "compose method '%s': utility statement should not have 'element' property";
  // can be assigned a value "self"
  String elementName;
  // can be assigned a value with element getter
  String apply;
  final UtamUtilityMethodAction applyExternal;
  private final UtamMatcher matcher;
  UtamArgument[] args;

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

  private Operation getCustomOperation(TranslationContext context, MethodContext methodContext) {
    ArgsProcessor argsProcessor = new ArgsProcessor(context, methodContext);
    List<MethodParameter> parameters = argsProcessor
        .getParameters(args)
        .stream()
        .map(methodContext::setStatementArg)
        .collect(Collectors.toList());
    // return type is irrelevant at statement level as we don't assign except for last statement
    ActionType action = new Custom(apply, methodContext.getReturnType(VOID), parameters);
    TypeProvider returnType = matcher == null ? action.getReturnType() : PrimitiveType.BOOLEAN;
    return new Operation(action, returnType, parameters);
  }

  private Operation getWaitForOperation(TranslationContext context, MethodContext methodContext) {
    List<MethodParameter> parameters = new ArgsProcessorWithExpectedTypes(context, methodContext, Collections.singletonList(FUNCTION))
        .getParameters(args);
    // return type is irrelevant at statement level as we don't assign except for last statement
    ActionType action = new Custom(apply, methodContext.getReturnType(VOID), parameters);
    List<ComposeMethodStatement> predicate = args[0].getPredicate(context, methodContext);
    return new OperationWithPredicate(action, methodContext.getReturnType(predicate,
        PrimitiveType.BOOLEAN), predicate);
  }

  private Operation getBasicOperation(TranslationContext context, ElementContext element,
      MethodContext methodContext) {
    ActionType action = getActionType(apply, element.getType(), element.getName());
    String validationContextStr = String.format("method '%s'", methodContext.getName());
    if (matcher != null) {
      matcher.checkOperandForMatcher(action.getReturnType(), validationContextStr);
    }
    List<MethodParameter> parameters = new ArgsProcessorBasicAction(context, validationContextStr, action)
        .getParameters(args)
        .stream()
        .map(methodContext::setStatementArg)
        .collect(Collectors.toList());
    return new BasicElementOperation(action, parameters);
  }

  /**
   * Create an Operation for a utility statement. For imperative extension, an operation is a custom
   * action specified in the invoke property in the JSON file. We access the value of the invoke
   * property through `applyExternal.getMethodName()`
   *
   * @param methodContext context of the current method being compiled
   * @return an operation that represents the structure of an imperative extension statement
   */
  private Operation getUtilityOperation(TranslationContext context, MethodContext methodContext) {
    List<MethodParameter> parameters = new ArgsProcessor(context, methodContext)
        .getParameters(applyExternal.args)
        .stream()
        .map(methodContext::setStatementArg)
        .collect(Collectors.toList());
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
      if(elementName != null) {
        // if element name is set - use its getter as "apply"
        String applyGetter = context.getElement(elementName).getElementMethod().getDeclaration().getName();
        this.elementName = SELF_ELEMENT_NAME;
        this.apply = applyGetter;
      } else {
        throw new UtamError(
            String.format(
                ERR_COMPOSE_ACTION_REQUIRED_KEYS,
                methodContext.getName()
            )
        );
      }
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

    if (applyExternal != null && elementName != null) {
      throw new UtamError(
          String.format(
              ERR_COMPOSE_ACTION_REDUNDANT_ELEMENT,
              methodContext.getName()
          )
      );
    }

    Operand operand = getOperand(context, methodContext);
    Operation operation = getOperation(context, methodContext);

    if (applyExternal != null) {
      return new ComposeMethodStatement.Utility(operand, operation, isLastPredicateStatement);
    }
    // to make sure that private method is declared because it's being called from another method
    context.setMethodUsage(apply);
    if (operand.isList() && !isSizeAction()) {
      return
          operation.isReturnsVoid() ? new ComposeMethodStatement.VoidList(operand, operation,
              isLastPredicateStatement)
              : new ComposeMethodStatement.ReturnsList(operand, operation, isLastPredicateStatement);
    }
    MatcherType matcherType = matcher == null? null : matcher.getMatcherType();
    return new ComposeMethodStatement.Single(operand, operation, matcherType,
        getMatcherParameters(context, methodContext), isLastPredicateStatement);
  }

  // for waitFor or isPresent element can only be single and non document
  private Operand getOperand(TranslationContext context,
      MethodContext methodContext) {
    if (applyExternal != null) {
      TypeProvider type = context.getUtilityType(applyExternal.getExternalClassPath());
      return new UtilityOperand(type);
    }
    ElementContext element = context.getElement(elementName);
    if (element.isSelfElement()) {
      return ComposeMethodStatement.SELF_OPERAND;
    }
    if (element.isDocumentElement()) {
      return ComposeMethodStatement.DOCUMENT_OPERAND;
    }
    // register usage of getter from compose statement
    element.setElementMethodUsage(context);
    return new ElementOperand(element, methodContext);
  }

  private Operation getOperation(TranslationContext context, MethodContext methodContext) {
    if (applyExternal != null) {
      return getUtilityOperation(context, methodContext);
    }
    if (isWaitForAction()) {
      return getWaitForOperation(context, methodContext);
    }
    ElementContext element = context.getElement(elementName);
    if (element.isCustomElement() || element.isDocumentElement() || element.isSelfElement()) {
      return getCustomOperation(context, methodContext);
    }
    return getBasicOperation(context, element, methodContext);
  }

  private List<MethodParameter> getMatcherParameters(TranslationContext context, MethodContext methodContext) {
    if (matcher == null) {
      return null;
    }
    return matcher.getParameters(context, methodContext);
  }

  private boolean isWaitForAction() {
    return WAIT_FOR.equals(apply);
  }

  private boolean isSizeAction() {
    return size.getApplyString().equals(apply);
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
