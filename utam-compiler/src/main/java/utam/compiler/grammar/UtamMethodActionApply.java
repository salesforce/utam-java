/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import static utam.compiler.diagnostics.ValidationUtilities.VALIDATION;
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
import utam.compiler.helpers.ElementContext.Navigation;
import utam.compiler.helpers.MethodContext;
import utam.compiler.helpers.MethodContext.ElementsUsageTracker;
import utam.compiler.helpers.ParameterUtils;
import utam.compiler.helpers.ParametersContext;
import utam.compiler.helpers.ParametersContext.StatementParametersContext;
import utam.compiler.helpers.PrimitiveType;
import utam.compiler.helpers.ReturnType;
import utam.compiler.helpers.StatementContext;
import utam.compiler.helpers.TranslationContext;
import utam.compiler.representation.ComposeMethodStatement.Operand;
import utam.compiler.representation.ComposeMethodStatement.Operation;
import utam.compiler.representation.MatcherObject;
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

  private static final Operand DOCUMENT_OPERAND =
      new ConstOperand(
          String.format("this.%s()", Document.DOCUMENT_ELEMENT.getElementGetterName()));
  private static final Operand NAVIGATION_OPERAND =
      new ConstOperand(
          String.format("this.%s()", Navigation.NAVIGATION_OBJECT.getElementGetterName()));

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

  @Override
  Statement getStatement(
      TranslationContext context, MethodContext methodContext, StatementContext statementContext) {
    String parserContext = String.format("method \"%s\"", methodContext.getName());
    VALIDATION.validateNotNullOrEmptyString(apply, parserContext, "apply");
    if (isChain) {
      chainValidations(statementContext, methodContext.getName());
      // if statement is marked as a chain, it should be applied to previous result, so "element" is
      // redundant
      if (elementName != null) {
        String message = VALIDATION.getErrorMessage(606, methodContext.getName());
        throw new UtamCompilationError(message);
      }
      return new ChainApplyStatement(context, methodContext, statementContext);
    }
    ElementContext elementContext = context.getElement(elementName);
    if (elementContext == null) {
      String message = VALIDATION.getErrorMessage(601, methodContext.getName(), elementName);
      throw new UtamCompilationError(message);
    }
    ElementContext.ElementType type = elementContext.getElementNodeType();
    if (type == ElementType.BASIC || type == ElementType.ROOT) {
      return new BasicApplyStatement(context, methodContext, statementContext, elementContext);
    }
    return new ApplyStatement(context, methodContext, statementContext, elementContext);
  }

  @Override
  boolean isApplyToList(Operand operand) {
    return operand.isApplyToList() && !size.getApplyString().equals(apply);
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
      String callGetter =
          String.format("%s %s = %s", variableType, elementVariableName, getElementGetterString());
      codeLines.add(callGetter);
      if (elementContext.isNullable()) {
        String exitValue =
            statementReturn.isSameType(VOID)
                ? "return"
                : (String.format("return %s", statementReturn.getFalsyValue()));
        codeLines.add(String.format("if (%s == null) { %s; }", elementVariableName, exitValue));
      }
      return codeLines;
    }

    private String getElementGetterString() {
      List<MethodParameter> getterParameters = elementContext.getGetterNonLiteralParameters();
      String parameters = getParametersValuesString(getterParameters);
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

    final MatcherObject matcher;
    private final List<MethodParameter> actionParameters = new ArrayList<>();
    private final ActionType action;
    private final TypeProvider returnType;

    /**
     * @param action method to invoke
     * @param returnType returnType from action, for waitFor it's last predicate
     * @param actionParameters parameters for method invocation
     * @param matcher matcher object
     */
    ApplyOperation(
        ActionType action,
        TypeProvider returnType,
        List<MethodParameter> actionParameters,
        MatcherObject matcher) {
      this.action = action;
      this.actionParameters.addAll(actionParameters);
      this.returnType = returnType;
      this.matcher = matcher;
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
      return String.format(
          "%s(%s)", action.getApplyString(), getParametersValuesString(actionParameters));
    }
  }

  /**
   * Non static class that transforms JSON to statement for "apply" invoked on basic element
   *
   * @author elizaveta.ivanova
   * @since 240
   */
  class BasicApplyStatement extends ApplyStatement {

    BasicApplyStatement(
        TranslationContext context,
        MethodContext methodContext,
        StatementContext statementContext,
        ElementContext elementContext) {
      super(context, methodContext, statementContext, elementContext);
    }

    private List<MethodParameter> getBasicActionParameters(ActionType action) {
      String parserContext = String.format("method \"%s\"", methodContext.getName());
      ArgumentsProvider argumentsProvider = new ArgumentsProvider(argsNode, parserContext);
      ParametersContext parametersContext =
          new StatementParametersContext(parserContext, context, methodContext);
      List<UtamArgument> arguments =
          argumentsProvider.getArguments(UtamArgument.ArgsValidationMode.LITERAL_ALLOWED);
      arguments.stream()
          .map(argument -> argument.asParameter(context, methodContext, parametersContext))
          .forEach(parametersContext::setParameter);
      List<TypeProvider> expectedTypes = action.getParametersTypes(parserContext, arguments.size());
      return parametersContext.getParameters(expectedTypes);
    }

    @Override
    ApplyOperation getApplyOperation() {
      String methodName = methodContext.getName();
      ActionType action = getActionType(apply, elementContext.getType(), null, methodName);
      ReturnType declaredReturnType = getDeclaredReturnType(methodName);
      if (declaredReturnType.isReturnTypeSet()) {
        // if "returnType" is set - check if it's correct
        TypeProvider declaredReturn = declaredReturnType.getReturnType(context);
        TypeProvider expectedReturn =
            hasMatcher
                ? PrimitiveType.BOOLEAN
                : (elementContext.isReturnAll()
                    ? wrapAsList(action.getReturnType())
                    : action.getReturnType());
        if (!expectedReturn.isSameType(declaredReturn)) {
          String errorMsg =
              VALIDATION.getErrorMessage(
                  613, methodName, expectedReturn.getSimpleName(), declaredReturn.getSimpleName());
          throw new UtamCompilationError(errorMsg);
        }
      }
      List<MethodParameter> parameters = getBasicActionParameters(action);
      // matcher parameters should be set after action parameters
      MatcherObject matcher = matcherProvider.apply(context, methodContext);
      if (matcher != null) {
        matcher.checkMatcherOperand(action.getReturnType());
      }
      return new ApplyOperation(action, action.getReturnType(), parameters, matcher);
    }

    @Override
    Operand getOperand() {
      return getReferencedElementOperand();
    }
  }

  /**
   * Non static class that transforms JSON to statement for "apply" invoked on self/document or
   * custom object
   *
   * @author elizaveta.ivanova
   * @since 240
   */
  class ApplyStatement extends Statement {

    final ElementContext elementContext;

    ApplyStatement(
        TranslationContext context,
        MethodContext methodContext,
        StatementContext statementContext,
        ElementContext elementContext) {
      super(context, methodContext, statementContext);
      this.elementContext = elementContext;
    }

    ElementOperand getReferencedElementOperand() {
      // mark that element getter is invoked, otherwise private element getter will not be generated
      context.setMethodUsage(elementContext.getElementGetterName());
      ElementsUsageTracker usageTracker = methodContext.getElementUsageTracker();
      String elementName = elementContext.getName();
      if (usageTracker.isReusedElement(elementName)) {
        return new ElementOperand(
            elementContext, usageTracker.getReusedElementVariable(elementName));
      }
      String elementVariableName = statementContext.getElementVariableName(elementName);
      // remember that element is used to not propagate its parameters to method for second time
      // if element is already used in a previous statement, parameters were already added
      // should be done AFTER statement is created
      usageTracker.setElementUsage(elementVariableName, elementContext);
      String parserContext =
          String.format("method \"%s\", element \"%s\"", methodContext.getName(), elementName);
      ParametersContext parametersContext =
          new StatementParametersContext(parserContext, context, methodContext);
      elementContext.getParameters().forEach(parametersContext::setParameter);
      List<MethodParameter> parameters = parametersContext.getParameters();
      return new ElementOperand(elementContext, elementVariableName, parameters);
    }

    List<MethodParameter> getActionParameters() {
      String parserContext = String.format("method \"%s\"", methodContext.getName());
      ParametersContext parametersContext =
          new StatementParametersContext(parserContext, context, methodContext);
      ArgumentsProvider argumentsProvider = new ArgumentsProvider(argsNode, parserContext);
      List<UtamArgument> arguments =
          argumentsProvider.getArguments(UtamArgument.ArgsValidationMode.LITERAL_ALLOWED);
      arguments.stream()
          .map(argument -> argument.asParameter(context, methodContext, parametersContext))
          .forEach(parametersContext::setParameter);
      return parametersContext.getParameters();
    }

    @Override
    ApplyOperation getApplyOperation() {
      // in case of self invocations make sure that private method is declared because it's being
      // called from another method
      context.setMethodUsage(apply);
      TypeProvider defaultReturnType =
          statementContext.isLastStatement()
              ? methodContext.getDeclaredReturnType().getReturnTypeOrDefault(context, VOID)
              : VOID;
      TypeProvider operationReturnType =
          statementContext.getDeclaredReturnOrDefault(
              context, methodContext.getDeclaredReturnType(), defaultReturnType);
      ActionType action = new CustomActionType(apply, operationReturnType);
      List<MethodParameter> parameters = getActionParameters();
      // matcher parameters should be set after action parameters
      MatcherObject matcher = matcherProvider.apply(context, methodContext);
      return new ApplyOperation(action, operationReturnType, parameters, matcher);
    }

    @Override
    Operand getOperand() {
      if (elementContext.getElementNodeType() == ElementType.SELF) {
        return SELF_OPERAND;
      } else if (elementContext.getElementNodeType() == ElementType.DOCUMENT) {
        return DOCUMENT_OPERAND;
      } else if (elementContext.getElementNodeType() == ElementType.NAVIGATION) {
        return NAVIGATION_OPERAND;
      } else {
        return getReferencedElementOperand();
      }
    }
  }

  /**
   * Non static class that transforms JSON to statement for "apply" invoked from a "chained"
   * statement
   *
   * @author elizaveta.ivanova
   * @since 240
   */
  class ChainApplyStatement extends ApplyStatement {

    ChainApplyStatement(
        TranslationContext context,
        MethodContext methodContext,
        StatementContext statementContext) {
      super(context, methodContext, statementContext, null);
    }

    @Override
    Operand getOperand() {
      return statementContext.getChainOperand();
    }
  }
}
