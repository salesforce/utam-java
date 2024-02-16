/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import static utam.compiler.diagnostics.ValidationUtilities.VALIDATION;
import static utam.compiler.helpers.ParameterUtils.getParametersValuesString;
import static utam.compiler.helpers.TypeUtilities.VOID;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import java.util.Collections;
import java.util.List;
import utam.compiler.grammar.UtamMethodActionApply.ApplyOperation;
import utam.compiler.helpers.ActionType;
import utam.compiler.helpers.MethodContext;
import utam.compiler.helpers.ReturnType;
import utam.compiler.helpers.StatementContext;
import utam.compiler.helpers.TranslationContext;
import utam.compiler.helpers.TypeUtilities;
import utam.compiler.representation.ComposeMethodStatement;
import utam.compiler.representation.ComposeMethodStatement.Operand;
import utam.compiler.representation.ComposeMethodStatement.Operation;
import utam.compiler.representation.ComposeMethodStatement.Single;
import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.TypeProvider;
import utam.core.framework.base.UtamUtilitiesContext;

/**
 * mapping for utility statement, uses default deserializer
 *
 * @author elizaveta.ivanova
 * @since 236
 */
@JsonDeserialize
class UtamMethodActionUtility extends UtamMethodAction {

  private final UtamUtilityMethodAction applyExternal;

  @JsonCreator
  UtamMethodActionUtility(
      @JsonProperty(value = "applyExternal") UtamUtilityMethodAction applyExternal,
      @JsonProperty(value = "returnType") JsonNode returnType,
      @JsonProperty(value = "returnAll") Boolean isReturnList) {
    // utility can't be marked as chain
    super(null, null, null, returnType, isReturnList, false);
    this.applyExternal = applyExternal;
  }

  @Override
  Statement getStatement(
      TranslationContext context, MethodContext methodContext, StatementContext statementContext) {
    throw new IllegalStateException("Compose statement is set without intermittent object");
  }

  /**
   * Create an Operation for a utility statement. For imperative extension, an operation is a custom
   * action specified in the invoke property in the JSON file. We access the value of the invoke
   * property through `applyExternal.getMethodName()`
   */
  @Override
  ComposeMethodStatement getComposeAction(
      TranslationContext context, MethodContext methodContext, StatementContext statementContext) {
    chainValidations(statementContext, methodContext.getName());
    String parserContext = String.format("method \"%s\"", methodContext.getName());
    VALIDATION.validateRequiredProperty(applyExternal, parserContext, "applyExternal");
    TypeProvider utilityType = context.getUtilityType(applyExternal.getExternalClassPath());
    Operand operand = new UtilityOperand(utilityType);
    List<MethodParameter> parameters = applyExternal.getParameters(context, methodContext);
    ReturnType returnTypeObject = getDeclaredReturnType(methodContext.getName());
    TypeProvider defaultReturnType =
        statementContext.isLastStatement()
            ? methodContext.getDeclaredReturnType().getReturnTypeOrDefault(context, VOID)
            : VOID;
    TypeProvider statementReturnType =
        returnTypeObject.getReturnTypeOrDefault(context, defaultReturnType);
    ActionType action = new CustomActionType(applyExternal.getMethodName(), statementReturnType);
    TypeProvider returnType =
        statementContext.getDeclaredReturnOrDefault(
            context, methodContext.getDeclaredReturnType(), action.getReturnType());
    Operation operation = new UtilityOperation(action, returnType, parameters);
    return new UtilityStatement(operand, operation, statementContext);
  }

  /** Information about an imperative extension class */
  static class UtilityOperand extends ConstOperand {

    private final TypeProvider type;

    /**
     * Creates a new UtilityOperand object.
     *
     * @param type holds information about the type of the utility class
     */
    UtilityOperand(TypeProvider type) {
      super(type.getSimpleName());
      this.type = type;
    }

    @Override
    protected List<TypeProvider> getAddedClassImports() {
      return Collections.singletonList(type);
    }
  }

  /** information about applied action on imperative extension */
  static class UtilityOperation extends ApplyOperation {

    /**
     * @param action method to invoke
     * @param returnType returnType from action
     * @param actionParameters parameters for method invocation
     */
    UtilityOperation(
        ActionType action, TypeProvider returnType, List<MethodParameter> actionParameters) {
      super(action, returnType, actionParameters, null);
    }

    /**
     * Method invoke to construct the statement associated with a utility extension. Utility
     * statements are expressed as ClassName.staticMethod(this, [params])
     *
     * @return string with code
     */
    @Override
    protected String getInvocationString() {
      String parametersValues = getParametersValuesString(getActionParameters());
      String separator = !parametersValues.isEmpty() ? ", " : "";
      return String.format(
          "%s(new %s(this)%s%s)",
          getAction().getApplyString(),
          UtamUtilitiesContext.class.getSimpleName(),
          separator,
          getParametersValuesString(getActionParameters()));
    }
  }

  /** Represent a Compose Statement for an Imperative Extension */
  static class UtilityStatement extends Single {

    /**
     * @param operand represents the imperative extension class
     * @param operation represents the static method being called on the imperative extension
     * @param statementContext statement context
     */
    UtilityStatement(Operand operand, Operation operation, StatementContext statementContext) {
      super(operand, operation, null, statementContext);
      TypeProvider utilitiesContextType = new TypeUtilities.FromClass(UtamUtilitiesContext.class);
      getClassImports().add(utilitiesContextType);
    }
  }
}
