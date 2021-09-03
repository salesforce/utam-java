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
 * context of the method, keeps track of elements
 *
 * @author elizaveta.ivanova
 * @since 232
 */
public final class MethodContext {

  public static final String ERR_ARG_DUPLICATE_NAME = "%s: parameter with name '%s' already declared";
  public static final String BEFORE_LOAD_METHOD_NAME = "load";
  static final String ERR_METHOD_REFERENCE_ARGS = "%s: method level argument '%s' can't have reference type";
  static final String ERR_REFERENCE_MISSING = "%s: statement declares a reference to '%s', but there’s no matching method parameter";
  static final String ERR_ARG_TYPE_MISMATCH = "%s: statement declares an argument '%s' with type '%s', but the type does not match method parameter";
  static final String ERR_LITERAL_PARAMETER_NOT_ALLOWED = "%s: literal parameter '%s' not allowed at the method level";
  public static final String ERR_PARAMETER_NEVER_USED = "%s: declared parameter '%s' is never used";
  private final String methodName;
  // to keep track of element usages
  private final Map<String, Entry<String, ElementContext>> elementVariables = new HashMap<>();
  private final ReturnType methodReturnType;
  private final String validationContext;
  private final Map<String, Entry<Boolean, MethodParameter>> declaredMethodLevelParameters = new HashMap<>();
  private final Map<String, MethodParameter> accumulatedStatementsParameters = new HashMap<>();
  private final List<MethodParameter> usedMethodParameters = new ArrayList<>();

  public MethodContext(String methodName, ReturnType declaredReturnType) {
    this.methodName = methodName;
    this.validationContext = String.format("method '%s'", methodName);
    this.methodReturnType = declaredReturnType;
  }

  // used in tests
  public MethodContext() {
    this("test", new ReturnType(null, null, "test"));
  }

  public static boolean isNullOrVoid(TypeProvider returnType) {
    return returnType == null || returnType.isSameType(VOID);
  }

  public ReturnType getDeclaredReturnType() {
    return methodReturnType;
  }

  public String getName() {
    return methodName;
  }

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
   * for an element that has been used in previous statements, return variable name
   *
   * @param name name of the element from compose statement
   * @return variable name string
   */
  public String getReusedElementVariable(String name) {
    return elementVariables.get(name).getKey();
  }

  /**
   * track reusing of a same element
   *
   * @param elementVariable variable name for the element
   * @param context         element
   */
  public void setElementUsage(String elementVariable, ElementContext context) {
    elementVariables.put(context.getName(), new SimpleEntry<>(elementVariable, context));
  }

  /**
   * set parameter declared at the method level
   *
   * @param parameter value
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

  public List<MethodParameter> getMethodParameters() {
    return usedMethodParameters;
  }

  public void checkAllParametersWereUsed() {
    if(declaredMethodLevelParameters.isEmpty()) {
      return;
    }
    usedMethodParameters.forEach(p -> {
      String parameterName = p.getValue();
      if(!declaredMethodLevelParameters.get(parameterName).getKey()) {
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
        MethodParameter referencedParameter = declaredMethodLevelParameters.get(parameterName).getValue();
        // mark as used
        declaredMethodLevelParameters.put(parameterName, new SimpleEntry<>(true, referencedParameter));
        statementArgs.put(parameterName, referencedParameter);
        return referencedParameter;
      }
      // if method level args were provided, find referenced arg and check type matches
      if (!declaredMethodLevelParameters.isEmpty()) {
        if (!declaredMethodLevelParameters.containsKey(parameterName)) {
          throw new UtamCompilationError(
              String.format(
                  ERR_REFERENCE_MISSING, validationContext, parameterName));
        }
        TypeProvider declaredType = declaredMethodLevelParameters.get(parameterName).getValue().getType();
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
}
