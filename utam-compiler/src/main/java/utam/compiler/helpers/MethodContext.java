/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.helpers;

import static utam.compiler.helpers.TypeUtilities.PARAMETER_REFERENCE;
import static utam.compiler.helpers.TypeUtilities.VOID;

import java.util.AbstractMap.SimpleEntry;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import utam.compiler.UtamCompilationError;
import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.TypeProvider;

/**
 * Context of the method, keeps track of elements and return type. Class is stateful because of
 * elements usage tracker.
 *
 * @author elizaveta.ivanova
 * @since 232
 */
public final class MethodContext {

  static final String ERR_ARG_DUPLICATE_NAME = "%s: parameter with name '%s' already declared"
      + " \n if the same parameter needed in multiple places, use 'argumentReference' type parameter instead";
  static final String ERR_PARAMETER_NEVER_USED = "%s: declared parameter '%s' is never used";
  static final String ERR_METHOD_REFERENCE_ARGS = "%s: method level argument '%s' can't have reference type";
  static final String ERR_REFERENCE_MISSING = "%s: statement declares a reference to '%s', but there’s no matching method parameter";
  static final String ERR_REFERENCE_REQUIRED = "%s: statement declares parameter '%s' that should be added to method level parameters";
  static final String ERR_ARG_TYPE_MISMATCH = "%s: statement declares an argument '%s' with type '%s', but the type does not match method parameter";
  static final String ERR_LITERAL_PARAMETER_NOT_ALLOWED = "%s: literal parameter '%s' not allowed at the method level";
  private final String methodName;
  // to keep track of element usages, each predicate has its own tracker
  private final List<ElementsUsageTracker> elementsUsageTrackers = new ArrayList<>();
  private final ReturnType methodReturnType;
  private final String validationContext;
  private final Map<String, Entry<Boolean, MethodParameter>> declaredMethodLevelParameters = new HashMap<>();
  private final Map<String, MethodParameter> accumulatedStatementsParameters = new HashMap<>();
  private final List<MethodParameter> usedMethodParameters = new ArrayList<>();
  // index of the current elements usage tracker, starting from 0 and incrementing as we get into a predicate
  private int elementsUsageContextIndex = 0;

  public MethodContext(String methodName, ReturnType declaredReturnType) {
    this.methodName = methodName;
    this.validationContext = String.format("method '%s'", methodName);
    this.methodReturnType = declaredReturnType;
    this.elementsUsageTrackers.add(new ElementsUsageTracker());
  }

  /**
   * check if method returns void
   *
   * @param returnType return type
   * @return true if method returns void
   */
  public static boolean isNullOrVoid(TypeProvider returnType) {
    return returnType == null || returnType.isSameType(VOID);
  }

  /**
   * get declared return type of the method
   *
   * @return declared return type
   */
  public ReturnType getDeclaredReturnType() {
    return methodReturnType;
  }

  /**
   * get method name
   *
   * @return string with method name
   */
  public String getName() {
    return methodName;
  }

  /**
   * get current elements usage tracker
   *
   * @return instance of the tracker
   */
  public final ElementsUsageTracker getElementUsageTracker() {
    return elementsUsageTrackers.get(elementsUsageContextIndex);
  }

  /**
   * enters predicate context to track used elements separately
   */
  public final void enterPredicateContext() {
    this.elementsUsageContextIndex = elementsUsageTrackers.size();
    this.elementsUsageTrackers.add(new ElementsUsageTracker());
  }

  /**
   * exits predicate context to track used elements separately
   */
  public final void exitPredicateContext() {
    this.elementsUsageContextIndex--;
  }

  /**
   * set parameter declared at the method level
   *
   * @param parameter parameter instance
   */
  public void setDeclaredParameter(MethodParameter parameter) {
    if (parameter.isLiteral()) {
      throw new UtamCompilationError(
          String
              .format(ERR_LITERAL_PARAMETER_NOT_ALLOWED, validationContext, parameter.getValue()));
    }
    String argName = parameter.getValue();
    TypeProvider argType = parameter.getType();
    if (declaredMethodLevelParameters.containsKey(argName)) {
      throw new UtamCompilationError(
          String.format(ERR_ARG_DUPLICATE_NAME, validationContext, argName));
    }
    if (PARAMETER_REFERENCE.isSameType(argType)) {
      throw new UtamCompilationError(
          String.format(ERR_METHOD_REFERENCE_ARGS, validationContext, argName));
    }
    // parameter is not yet used, so set false
    declaredMethodLevelParameters.put(argName, new SimpleEntry<>(false, parameter));
    usedMethodParameters.add(parameter);
  }

  /**
   * get all method parameters
   *
   * @return list of parameters
   */
  public List<MethodParameter> getMethodParameters() {
    return usedMethodParameters;
  }

  /**
   * check that all parameters were used, trows an error if there was not
   */
  public void checkAllParametersWereUsed() {
    if (declaredMethodLevelParameters.isEmpty()) {
      return;
    }
    usedMethodParameters.forEach(p -> {
      String parameterName = p.getValue();
      if (!declaredMethodLevelParameters.get(parameterName).getKey()) {
        throw new UtamCompilationError(
            String.format(ERR_PARAMETER_NEVER_USED, validationContext, parameterName));
      }
    });
  }

  public MethodParameter setStatementParameter(MethodParameter parameter,
      StatementContext statementContext) {
    if (parameter == null) { //predicate
      return null;
    }
    Map<String, MethodParameter> statementArgs = statementContext.getStatementArgsMap();
    if (!parameter.isLiteral()) {
      String parameterName = parameter.getValue();
      TypeProvider argType = parameter.getType();
      if (statementArgs.containsKey(parameterName)) {
        throw new UtamCompilationError(
            String.format(
                ERR_ARG_DUPLICATE_NAME, validationContext, parameterName));
      }
      // find referenced parameter
      if (PARAMETER_REFERENCE.isSameType(argType)) {
        if (!declaredMethodLevelParameters.containsKey(parameterName)) {
          throw new UtamCompilationError(
              String.format(ERR_REFERENCE_MISSING, validationContext, parameterName));
        }
        MethodParameter referencedParameter = declaredMethodLevelParameters.get(parameterName)
            .getValue();
        // mark as used
        declaredMethodLevelParameters
            .put(parameterName, new SimpleEntry<>(true, referencedParameter));
        statementArgs.put(parameterName, referencedParameter);
        return referencedParameter;
      }
      // if method level args were provided, find referenced arg and check type matches
      if (!declaredMethodLevelParameters.isEmpty()) {
        if (!declaredMethodLevelParameters.containsKey(parameterName)) {
          throw new UtamCompilationError(
              String.format(
                  ERR_REFERENCE_REQUIRED, validationContext, parameterName));
        }
        TypeProvider declaredType = declaredMethodLevelParameters.get(parameterName).getValue()
            .getType();
        if (!argType.isSameType(declaredType)) {
          throw new UtamCompilationError(
              String.format(
                  ERR_ARG_TYPE_MISMATCH, validationContext, parameterName,
                  argType.getSimpleName()));
        }
      } else {
        if (accumulatedStatementsParameters.containsKey(parameterName)) {
          throw new UtamCompilationError(
              String.format(ERR_ARG_DUPLICATE_NAME, validationContext, parameterName));
        }
        accumulatedStatementsParameters.put(parameterName, parameter);
      }
      statementArgs.put(parameterName, parameter);
      usedMethodParameters.add(parameter);
    }
    List<MethodParameter> nested = parameter.getNestedParameters();
    if (nested != null) {
      nested.forEach(p -> setStatementParameter(p, statementContext));
    }
    return parameter;
  }

  /**
   * Tracker for elements used in compose methods. When same element is reused we want to store it
   * in a variable instead calling the same getter several times.
   *
   * @author elizaveta.ivanova
   * @since 236
   */
  public static class ElementsUsageTracker {

    private final Map<String, Entry<String, ElementContext>> elementVariables = new HashMap<>();

    /**
     * check if element has been used in previous statements
     *
     * @param name name of the element from compose statement
     * @return true if element was already used
     */
    public boolean isReusedElement(String name) {
      return elementVariables.containsKey(name);
    }

    /**
     * set element variable usage to track reusing of a same element
     *
     * @param elementVariable variable name for the element
     * @param context         element
     */
    public void setElementUsage(String elementVariable, ElementContext context) {
      elementVariables.put(context.getName(), new SimpleEntry<>(elementVariable, context));
    }

    /**
     * for an element that has been used in previous statements, return variable name
     *
     * @param name name of the element from compose statement
     * @return variable name string
     */
    public String getReusedElementVariable(String name) {
      return elementVariables.get(name).getKey();
    }
  }
}
