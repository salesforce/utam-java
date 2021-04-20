/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import utam.compiler.helpers.*;
import utam.compiler.representation.ComposeMethodStatement;
import utam.compiler.representation.ComposeMethodStatement.BasicElementOperation;
import utam.compiler.representation.ComposeMethodStatement.DocumentOperand;
import utam.compiler.representation.ComposeMethodStatement.Operand;
import utam.compiler.representation.ComposeMethodStatement.Operation;
import utam.compiler.representation.ComposeMethodStatement.OperationWithPredicate;
import utam.compiler.representation.ComposeMethodStatement.Single;
import utam.compiler.representation.ComposeMethodStatement.Utility;
import utam.compiler.representation.ComposeMethodStatement.UtilityOperand;
import utam.compiler.representation.ComposeMethodStatement.UtilityOperation;
import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.TypeProvider;
import utam.core.framework.consumer.UtamError;

import java.util.List;
import java.util.stream.Collectors;

import static utam.compiler.helpers.ActionableActionType.getActionType;
import static utam.compiler.helpers.ActionableActionType.isWaitFor;
import static utam.compiler.helpers.TypeUtilities.VOID;

/**
 * Compose statement mapping
 *
 * @author elizaveta.ivanova
 * @since 228
 */
class UtamMethodAction {

  static final String ERR_COMPOSE_ACTION_REQUIRED_KEYS =
      "Statements for compose method '%s' should either have 'element' and 'apply' or 'applyExternal' properties";

  final String elementName;
  final String apply;
  UtamArgument[] args;
  UtamUtilityMethodAction applyExternal;
  private final UtamMatcher matcher;

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

  /**
   * Create an Operation for a utility statement.
   * For imperative extension, an operation is a custom action specified in the invoke property in the JSON file.
   * We access the value of the invoke property through `applyExternal.getMethodName()`
   * @param methodContext context of the current method being compiled
   * @return an operation that represents the structure of an imperative extension statement
   */
  private Operation getUtilityOperation(MethodContext methodContext) {
    List<MethodParameter> parameters = UtamArgument
            .getArgsProcessor(applyExternal.args, null, methodContext.getName()).getOrdered();
    ActionType action = new Custom(applyExternal.getMethodName(), methodContext.getReturnType(VOID), parameters);
    TypeProvider returnType = action.getReturnType();
    return new UtilityOperation(action, returnType, parameters);
  }

  /**
   * Create a compose statement object from mapped Java entity.
   * This method creates a structure that will be used to generate the code for a given object in the `compose` array.
   * Each object in the `compose` array from the JSON PO will create one `ComposeMethodStatement`.
   * @param context current PO context
   * @param methodContext context of the current method being compiled
   * @param isLastPredicateStatement boolean that adds extra logic if the last statement is a predicate
   * @return
   */
  ComposeMethodStatement getComposeAction(TranslationContext context, MethodContext methodContext, boolean isLastPredicateStatement) {
    if (apply == null && applyExternal == null) {
      throw new UtamError(
        String.format(
            ERR_COMPOSE_ACTION_REQUIRED_KEYS,
            methodContext.getName()
        )
      );
    }
    if (apply != null) {
      if (elementName == null) {
        throw new UtamError(
            String.format(
                    ERR_COMPOSE_ACTION_REQUIRED_KEYS,
                    methodContext.getName()
            )
        );
      }
    }

    ComposeMethodStatement.Operation operation;
    ComposeMethodStatement statement;

    if (applyExternal != null) {
      TypeProvider type = context.getUtilityType(applyExternal.getExternalClassPath());
      UtilityOperand utilityOperand = new UtilityOperand(methodContext, type);
      operation = getUtilityOperation(methodContext);
      statement = new Utility(utilityOperand, operation);
    } else {
      ElementContext element = context.getElement(elementName);
      // register usage of getter from compose statement
      if (!element.isDocumentElement()) {
        context.setPrivateMethodUsage(element.getElementMethod().getDeclaration().getName());
      }

      ComposeMethodStatement.Operand operand = element.isDocumentElement() ?
          new DocumentOperand() : new Operand(element, methodContext);
      operation = element.isCustom() || element.isDocumentElement() ?
          getCustomOperation(context, methodContext) :
          getBasicOperation(context, element, methodContext, isLastPredicateStatement);
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
    }
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
