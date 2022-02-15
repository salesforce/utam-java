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
import static utam.compiler.helpers.ParameterUtils.getParametersValuesString;
import static utam.compiler.helpers.TypeUtilities.VOID;
import static utam.compiler.helpers.TypeUtilities.wrapAsList;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import java.util.ArrayList;
import java.util.List;
import utam.compiler.UtamCompilationError;
import utam.compiler.helpers.ActionType;
import utam.compiler.helpers.ElementContext;
import utam.compiler.helpers.ElementContext.BasicReturnsAll;
import utam.compiler.helpers.ElementContext.CustomReturnsAll;
import utam.compiler.helpers.ElementContext.Document;
import utam.compiler.helpers.ElementContext.ElementType;
import utam.compiler.helpers.MethodContext;
import utam.compiler.helpers.MethodContext.ElementsUsageTracker;
import utam.compiler.helpers.ParameterUtils;
import utam.compiler.helpers.ParametersContext;
import utam.compiler.helpers.ParametersContext.StatementParametersContext;
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

  private final String apply;

  @JsonCreator
  UtamMethodActionApply(
      @JsonProperty(value = "element") String elementName,
      @JsonProperty(value = "apply") String apply,
      @JsonProperty(value = "args") JsonNode argsNode,
      @JsonProperty(value = "matcher") JsonNode matcherNode,
      @JsonProperty(value = "returnType") JsonNode returnType,
      @JsonProperty(value = "returnAll") Boolean isReturnList,
      @JsonProperty(value = "chain", defaultValue = "false") boolean isChain) {
    super(elementName, argsNode, matcherNode, returnType, isReturnList, isChain);
    this.apply = apply;
  }

  private boolean isBasicElement(TranslationContext context, String methodName) {
    if (isChain || elementName == null) {
      return false;
    }
    ElementContext elementContext = getElementByName(context, methodName);
    ElementContext.ElementType type = elementContext.getElementNodeType();
    return type == ElementType.BASIC || type == ElementType.ROOT;
  }

  private ElementContext getElementByName(TranslationContext context, String methodName) {
    ElementContext element = context.getElement(elementName);
    if (element == null) {
      String message = context.getErrorMessage("UMA001", methodName, elementName);
      throw new UtamCompilationError(message);
    }
    return element;
  }

  private ElementOperand getReferencedElementOperand(
      ElementContext elementContext,
      TranslationContext context,
      MethodContext methodContext,
      StatementContext statementContext) {
    // register usage of getter from compose statement
    elementContext.setElementMethodUsage(context);
    ElementsUsageTracker usageTracker = methodContext.getElementUsageTracker();
    String elementName = elementContext.getName();
    if (usageTracker.isReusedElement(elementName)) {
      return new ElementOperand(elementContext, usageTracker.getReusedElementVariable(elementName));
    }
    String elementVariableName = statementContext.getElementVariableName(elementName);
    // remember that element is used to not propagate its parameters to method for second time
    // if element is already used in a previous statement, parameters were already added
    // should be done AFTER statement is created
    usageTracker.setElementUsage(elementVariableName, elementContext);
    String parserContext = String
        .format("method \"%s\", element \"%s\"", methodContext.getName(), elementName);
    ParametersContext parametersContext = new StatementParametersContext(parserContext, context,
        null, methodContext);
    elementContext.getParameters().forEach(parametersContext::setParameter);
    List<MethodParameter> parameters = parametersContext.getParameters();
    return new ElementOperand(elementContext, elementVariableName, parameters);
  }

  @Override
  boolean isApplyToList(Operand operand) {
    return operand.isApplyToList() && !size.getApplyString().equals(apply);
  }

  private Operation getCustomApplyOperation(
      TranslationContext context,
      MethodContext methodContext,
      StatementContext statementContext
  ) {
    // in case of self invocations make sure that private method is declared because it's being called from another method
    context.setMethodUsage(apply);
    String parserContext = String.format("method \"%s\"", methodContext.getName());
    ParametersContext parametersContext = new StatementParametersContext(parserContext, context,
        argsNode, methodContext);
    ArgumentsProvider argumentsProvider = new ArgumentsProvider(argsNode, parserContext);
    List<UtamArgument> arguments = argumentsProvider.getArguments(true);
    arguments
        .stream()
        .map(argument -> argument.asParameter(context, methodContext, parametersContext))
        .forEach(parametersContext::setParameter);
    TypeProvider defaultReturnType = statementContext.isLastStatement() ?
        methodContext.getDeclaredReturnType().getReturnTypeOrDefault(context, VOID) : VOID;
    TypeProvider operationReturnType = statementContext
        .getDeclaredReturnOrDefault(context, methodContext.getDeclaredReturnType(),
            defaultReturnType);
    ActionType action = new CustomActionType(apply, operationReturnType);
    List<MethodParameter> parameters = parametersContext.getParameters();
    return new ApplyOperation(action, operationReturnType, parameters);
  }

  private Operation getBasicActionOperation(
      ElementContext element,
      TranslationContext context,
      MethodContext methodContext) {
    String methodName = methodContext.getName();
    String parserContext = String.format("method \"%s\"", methodName);
    ArgumentsProvider argumentsProvider = new ArgumentsProvider(argsNode, parserContext);
    ParametersContext parametersContext = new StatementParametersContext(parserContext, context,
        argsNode, methodContext);
    List<UtamArgument> arguments = argumentsProvider.getArguments(true);
    arguments
        .stream()
        .map(argument -> argument.asParameter(context, methodContext, parametersContext))
        .forEach(parametersContext::setParameter);
    ActionType action = getActionType(apply, element.getType());
    if (action == null) {
      String message = context.getErrorMessage("UMA012", methodName, this.apply);
      throw new UtamCompilationError(message);
    }
    TypeProvider expectedReturn =
        element.isReturnAll() ? wrapAsList(action.getReturnType()) : action.getReturnType();
    ReturnType declaredReturnType = getDeclaredReturnType(methodName);
    if (declaredReturnType.isReturnTypeSet()) {
      TypeProvider declaredReturn = getDeclaredReturnType(methodName)
          .getReturnTypeOrNull(context);
      checkDefinedReturnType(expectedReturn, declaredReturn, methodName);
    }
    List<TypeProvider> expectedTypes = action.getParametersTypes(parserContext, arguments.size());
    List<MethodParameter> parameters = parametersContext.getParameters(expectedTypes);
    checkMatcher(expectedReturn, parserContext);
    return new ApplyOperation(action, action.getReturnType(), parameters);
  }

  @Override
  ComposeMethodStatement getComposeAction(TranslationContext context,
      MethodContext methodContext, StatementContext statementContext) {
    String methodName = methodContext.getName();
    // if statement is marked as a chain, it should be applied to previous result, so "element" is redundant
    checkChainElementRedundant(context, methodName);
    // first statement can't be marked as chain
    checkFirsStatementCantBeChain(statementContext, methodName);
    checkChainAllowed(statementContext, methodName);

    // set operand
    Operand operand;
    if (isChain) {
      operand = statementContext.getChainOperand();
    } else {
      ElementContext elementContext = getElementByName(context, methodContext.getName());
      if (elementContext.getElementNodeType() == ElementType.SELF) {
        operand = SELF_OPERAND;
      } else if (elementContext.getElementNodeType() == ElementType.DOCUMENT) {
        operand = DOCUMENT_OPERAND;
      } else {
        operand = getReferencedElementOperand(elementContext, context, methodContext,
            statementContext);
      }
    }

    // set operation
    Operation operation;
    if (isBasicElement(context, methodName)) {
      ElementContext element = ((ElementOperand) operand).elementContext;
      operation = getBasicActionOperation(element, context, methodContext);
    } else {
      operation = getCustomApplyOperation(context, methodContext, statementContext);
    }

    return buildStatement(operand, operation, context, methodContext, statementContext);
  }

  /**
   * operand represented by an element reference
   *
   * @author elizaveta.ivanova
   * @since 230
   */
  static class ElementOperand extends Operand {

    private final List<MethodParameter> parameters;
    private final ElementContext elementContext;
    private final List<TypeProvider> addedImports = new ArrayList<>();
    private final String elementVariableName;
    private final boolean isReusedElement;

    ElementOperand(
        ElementContext elementContext,
        String elementVariableName,
        List<MethodParameter> parameters) {
      this.elementContext = elementContext;
      this.elementVariableName = elementVariableName;
      this.parameters = parameters;
      this.isReusedElement = false;
      ParameterUtils.setImplementationImports(addedImports, parameters);
      ParameterUtils.setImport(addedImports, elementContext.getGetterReturnType());
    }

    ElementOperand(ElementContext elementContext, String elementVariableName) {
      this.elementContext = elementContext;
      this.elementVariableName = elementVariableName;
      parameters = new ArrayList<>();
      this.isReusedElement = true;
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
      if (isReusedElement) {
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

  /**
   * operation from "apply" property
   *
   * @author elizaveta.ivanova
   * @since 230
   */
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
}
