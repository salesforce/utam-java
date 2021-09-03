/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import static utam.compiler.helpers.BasicElementActionType.getActionType;
import static utam.compiler.helpers.ParameterUtils.getParametersValuesString;
import static utam.compiler.helpers.TypeUtilities.VOID;
import static utam.compiler.helpers.TypeUtilities.wrapAsList;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import utam.compiler.grammar.ArgsProcessor.ArgsProcessorBasicAction;
import utam.compiler.helpers.ActionType;
import utam.compiler.helpers.ElementContext;
import utam.compiler.helpers.ElementContext.BasicReturnsAll;
import utam.compiler.helpers.ElementContext.CustomReturnsAll;
import utam.compiler.helpers.ElementContext.Document;
import utam.compiler.helpers.ElementContext.ElementType;
import utam.compiler.helpers.MethodContext;
import utam.compiler.helpers.ParameterUtils;
import utam.compiler.helpers.ReturnType;
import utam.compiler.helpers.StatementContext;
import utam.compiler.helpers.TranslationContext;
import utam.compiler.representation.ComposeMethodStatement;
import utam.compiler.representation.ComposeMethodStatement.Operand;
import utam.compiler.representation.ComposeMethodStatement.Operation;
import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.TypeProvider;

/**
 * mapping for element/apply statement, uses default deserializer
 *
 * @author elizaveta.ivanova
 * @since 236
 */
@JsonDeserialize
class UtamMethodActionApply extends UtamMethodAction {

  private static final Operand DOCUMENT_OPERAND = new ConstOperand(String.format("this.%s()",
      Document.DOCUMENT_ELEMENT.getElementGetterName()));

  @JsonCreator
  UtamMethodActionApply(
      @JsonProperty(value = "element") String elementName,
      @JsonProperty(value = "apply") String apply,
      @JsonProperty(value = "args") UtamArgument[] args,
      @JsonProperty(value = "matcher") UtamMatcher matcher,
      @JsonProperty(value = "returnType") JsonNode returnType,
      @JsonProperty(value = "returnAll") Boolean isReturnList,
      @JsonProperty(value = "chain", defaultValue = "false") boolean isChain) {
    super(elementName, apply, null, args, matcher, returnType, isReturnList, isChain);
  }

  // no validation for apply args
  private boolean isBasicElement(TranslationContext context) {
    if (isChain || elementName == null) {
      return false;
    }
    ElementContext.ElementType type = context.getElement(elementName).getElementNodeType();
    return type == ElementType.BASIC || type == ElementType.ROOT;
  }

  private Operand getOperand(TranslationContext context,
      MethodContext methodContext, StatementContext statementContext) {
    if (isChain) {
      return statementContext.getChainOperand();
    }
    ElementContext element = context.getElement(elementName);
    if (element.getElementNodeType() == ElementType.SELF) {
      return SELF_OPERAND;
    } else if (element.getElementNodeType() == ElementType.DOCUMENT) {
      return DOCUMENT_OPERAND;
    } else {
      // register usage of getter from compose statement
      element.setElementMethodUsage(context);
      return new ElementOperand(element, methodContext, statementContext);
    }
  }

  @Override
  ComposeMethodStatement getComposeAction(TranslationContext context,
      MethodContext methodContext, StatementContext statementContext) {
    String methodName = methodContext.getName();
    String validationContextStr = String.format("method '%s'", methodName);

    // if statement is marked as a chain, it should be applied to previous result, so "element" is redundant
    checkChainElementRedundant(validationContextStr);
    // first statement can't be marked as chain
    checkFirsStatementCantBeChain(statementContext, methodName);
    checkChainAllowed(statementContext, methodName);

    // set operand
    Operand operand = getOperand(context, methodContext, statementContext);

    // set operation
    Operation operation;

    if (isBasicElement(context)) {
      ElementContext element = ((ElementOperand) operand).elementContext;
      ActionType action = getActionType(apply, element.getType(), element.getName());
      TypeProvider expectedReturn =
          element.isReturnAll() ? wrapAsList(action.getReturnType()) : action.getReturnType();
      ReturnType declaredReturnType = getDeclaredReturnType(methodName);
      if (declaredReturnType.isReturnTypeSet() && !declaredReturnType.isReturnSelf()) {
        TypeProvider declaredReturn = getDeclaredReturnType(methodName)
            .getReturnTypeOrNull(context);
        checkDefinedReturnType(expectedReturn, declaredReturn, methodName);
      }
      List<MethodParameter> parameters = new ArgsProcessorBasicAction(context, validationContextStr,
          action, p -> methodContext.setStatementParameter(p, statementContext))
          .getParameters(args);
      TypeProvider returnType =
          declaredReturnType.isReturnSelf() ? context.getSelfType() : action.getReturnType();
      operation = new BasicElementOperation(action, returnType, parameters);
      checkMatcher(expectedReturn, validationContextStr);
    } else {
      // for self invocations make sure that private method is declared because it's being called from another method
      context.setMethodUsage(apply);
      ArgsProcessor argsProcessor = new ArgsProcessor(context, methodContext,
          p -> methodContext.setStatementParameter(p, statementContext));
      List<MethodParameter> parameters = argsProcessor.getParameters(args);
      TypeProvider defaultReturnType = statementContext.isLastStatement() ?
          methodContext.getDeclaredReturnType().getReturnTypeOrDefault(context, VOID) : VOID;
      TypeProvider operationReturnType = statementContext
          .getDeclaredReturnOrDefault(context, methodContext.getDeclaredReturnType(),
              defaultReturnType);
      ActionType action = new CustomActionType(apply, operationReturnType);
      operation = new ApplyOperation(action, operationReturnType, parameters);
    }

    return buildStatement(operand, operation, context, methodContext, statementContext);
  }

  static class ElementOperand extends Operand {

    private final List<MethodParameter> parameters;
    private final ElementContext elementContext;
    private final boolean isElementAlreadyUsed;
    private final List<TypeProvider> addedImports = new ArrayList<>();
    private final String elementVariableName;

    ElementOperand(ElementContext elementContext, MethodContext methodContext,
        StatementContext statementContext) {
      this.elementContext = elementContext;
      String elementName = elementContext.getName();
      this.isElementAlreadyUsed = methodContext.isReusedElement(elementName);
      if (isElementAlreadyUsed) {
        this.elementVariableName = methodContext.getReusedElementVariable(elementName);
        parameters = new ArrayList<>();
      } else {
        this.elementVariableName = statementContext.getElementVariableName(elementName);
        // remember that element is used to not propagate its parameters to method for second time
        // if element is already used in a previous statement, parameters were already added
        // should be done AFTER statement is created
        methodContext.setElementUsage(elementVariableName, elementContext);
        parameters = elementContext.getParameters()
            .stream()
            .map(p -> methodContext.setStatementParameter(p, statementContext))
            .collect(Collectors.toList());
        ParameterUtils.setImplementationImports(addedImports, parameters);
        ParameterUtils.setImport(addedImports, elementContext.getGetterReturnType());
      }
    }

    @Override
    protected List<TypeProvider> getAddedClassImports() {
      return addedImports;
    }

    @Override
    protected String getOperandString() {
      return elementVariableName;
    }

    @Override
    protected List<String> getOperandInstantiationCode(TypeProvider statementReturn) {
      List<String> codeLines = new ArrayList<>();
      if (isElementAlreadyUsed) {
        return codeLines;
      }
      String variableType = elementContext.getGetterReturnType().getSimpleName();
      String callGetter = String
          .format("%s %s = %s", variableType, elementVariableName, getElementGetterString());
      codeLines.add(callGetter);
      if (elementContext.isNullable()) {
        String exitValue =
            statementReturn.isSameType(VOID) ? "return"
                : (String.format("return %s", statementReturn.getFalsyValue()));
        codeLines.add(String.format("if (%s == null) { %s; }", elementVariableName, exitValue));
      }
      return codeLines;
    }

    private String getElementGetterString() {
      List<MethodParameter> allParameters = elementContext.getParameters();
      String parameters = getParametersValuesString(allParameters);
      return String.format("this.%s(%s)", elementContext.getElementGetterName(), parameters);
    }

    @Override
    public boolean isApplyToList() {
      return elementContext instanceof BasicReturnsAll
          || elementContext instanceof CustomReturnsAll;
    }

    @Override
    protected List<MethodParameter> getElementParameters() {
      return parameters;
    }
  }

  static class ApplyOperation extends Operation {

    private final List<MethodParameter> actionParameters = new ArrayList<>();
    private final ActionType action;
    private final TypeProvider returnType;

    /**
     * @param action           method to invoke
     * @param returnType       returnType from action, for waitFor it's last predicate
     * @param actionParameters parameters for method invocation
     */
    ApplyOperation(ActionType action, TypeProvider returnType,
        List<MethodParameter> actionParameters) {
      this.action = action;
      this.actionParameters.addAll(actionParameters);
      this.returnType = returnType;
    }

    @Override
    protected List<MethodParameter> getActionParameters() {
      return actionParameters;
    }

    @Override
    public final TypeProvider getReturnType() {
      return returnType;
    }

    final ActionType getAction() {
      return action;
    }

    @Override
    protected String getInvocationString() {
      return String.format("%s(%s)",
          action.getApplyString(),
          getParametersValuesString(actionParameters));
    }
  }

  static class BasicElementOperation extends ApplyOperation {

    BasicElementOperation(ActionType action, TypeProvider returnType,
        List<MethodParameter> actionParameters) {
      super(action, returnType, actionParameters);
    }
  }
}
