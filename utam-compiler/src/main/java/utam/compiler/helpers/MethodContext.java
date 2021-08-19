/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.helpers;

import static utam.compiler.helpers.TypeUtilities.LIST_IMPORT;
import static utam.compiler.helpers.TypeUtilities.REFERENCE;
import static utam.compiler.helpers.TypeUtilities.VOID;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import utam.compiler.UtamCompilationError;
import utam.compiler.helpers.TypeUtilities.ListOf;
import utam.compiler.representation.ComposeMethodStatement;
import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.TypeProvider;
import utam.core.framework.consumer.UtamError;

/**
 * context of the method, keeps track of elements
 *
 * @author elizaveta.ivanova
 * @since 232
 */
public final class MethodContext {

  static final String ERR_METHOD_REFERENCE_ARGS = "%s: method level argument '%s' can't have reference type";
  static final String ERR_REFERENCE_MISSING = "%s: statement declares a reference to '%s', but there’s no matching method parameter";
  static final String ERR_ARG_TYPE_MISMATCH = "%s: statement declares an argument '%s' with type '%s', but the type does not match method parameter";
  static final String ERR_ARG_DUPLICATE_NAME = "%s: argument with name '%s' already declared";
  static final String ERR_LIST_OF_VOID_NOT_ALLOWED = "%s: cannot return list of null or void";
  static final String ERR_LITERAL_PARAMETER_NOT_ALLOWED = "%s: literal parameter '%s' not allowed at the method level";
  public static final String BEFORE_LOAD_METHOD_MANE = "load";
  private final String methodName;
  // to keep track of element usages
  private final Map<String, ElementContext> elementNames = new HashMap<>();
  private final TypeProvider methodReturnType;
  private final boolean isReturnsList;
  private final TypeProvider listType;
  private final String validationContext;
  private final Map<String, MethodParameter> methodArgs = new HashMap<>();
  private final List<Map<String, MethodParameter>> statementsArgs = new ArrayList<>();
  private int statementIndex = 0;

  public MethodContext(String methodName, TypeProvider returns, boolean isReturnsList) {
    this.methodName = methodName;
    this.validationContext = String.format("method '%s'", methodName);
    if (isNullOrVoid(returns) && isReturnsList) {
      throw new UtamCompilationError(
          String.format(ERR_LIST_OF_VOID_NOT_ALLOWED, validationContext));
    }
    this.listType = returns;
    this.methodReturnType = isReturnsList ? new ListOf(returns) : returns;
    this.isReturnsList = isReturnsList;
  }

  // used in tests
  public MethodContext(){
    this("test", null, false);
  }

  public static boolean isNullOrVoid(TypeProvider returnType) {
    return returnType == null || returnType.isSameType(VOID);
  }

  public String getName() {
    return methodName;
  }

  public TypeProvider getReturnType(TypeProvider defaultReturn) {
    return methodReturnType == null ? defaultReturn : methodReturnType;
  }

  public TypeProvider getReturnType(List<ComposeMethodStatement> statements,
      TypeProvider defaultReturn) {
    //if return type not set in JSON, get one from last statement
    ComposeMethodStatement lastStatement = statements.get(statements.size() - 1);
    TypeProvider lastStatementReturns = lastStatement.getReturnType();
    if (lastStatementReturns != null
        && methodReturnType != null
        && !lastStatementReturns.isSameType(methodReturnType)
        // Exclude load() method from this check, since it is always VOID type
        && !methodName.equals(BEFORE_LOAD_METHOD_MANE)) {
      throw new UtamError(String.format("method '%s' return type mismatch: "
              + "last statement returns '%s', method returns '%s'", methodName,
          lastStatementReturns.getSimpleName(), methodReturnType.getSimpleName()));
    }
    if (methodReturnType == null) {
      return lastStatement.getReturnType();
    }
    if (lastStatement.isUtilityMethodStatement()) {
      return methodReturnType;
    }
    return getReturnType(defaultReturn);
  }

  public boolean hasElement(String name) {
    return elementNames.containsKey(name);
  }

  public void setElementUsage(ElementContext context) {
    elementNames.put(context.getName(), context);
  }

  public List<TypeProvider> getReturnTypeImports(List<MethodParameter> methodParameters) {
    List<TypeProvider> imports = methodParameters.stream().map(MethodParameter::getType).collect(
        Collectors.toList());
    if (isReturnsList) {
      imports.add(LIST_IMPORT);
    }
    imports.add(listType);
    return imports;
  }

  /**
   * register method level args to make sure it's used
   *
   * @param parameter parameter
   */
  public void setMethodArg(MethodParameter parameter) {
    if (parameter.isLiteral()) {
      throw new UtamCompilationError(
          String.format(ERR_LITERAL_PARAMETER_NOT_ALLOWED, validationContext, parameter.getValue()));
    }
    String argName = parameter.getValue();
    TypeProvider argType = parameter.getType();
    if (methodArgs.containsKey(argName)) {
      throw new UtamCompilationError(
          String.format(
              ERR_ARG_DUPLICATE_NAME, validationContext, argName));
    }
    if (REFERENCE.isSameType(argType)) {
      throw new UtamCompilationError(
          String.format(ERR_METHOD_REFERENCE_ARGS, validationContext, argName));
    }
    methodArgs.put(argName, parameter);
  }

  public boolean hasMethodArgs() {
    return methodArgs.size() > 0;
  }

  /**
   * iterate to next statement to add args
   */
  public void nextStatement() {
    statementIndex++;
  }

  public MethodParameter setStatementArg(MethodParameter parameter) {
    if (parameter.isLiteral()) {
      List<MethodParameter> nested = parameter.getNestedParameters();
      if(nested != null) {
        nested.forEach(this::setStatementArg);
      }
      return parameter;
    }
    String argName = parameter.getValue();
    TypeProvider argType = parameter.getType();
    while (statementsArgs.size() <= statementIndex) {
      statementsArgs.add(new HashMap<>());
    }
    Map<String, MethodParameter> statementArgs = statementsArgs.get(statementIndex);
    if (statementArgs.containsKey(argName)) {
      throw new UtamCompilationError(
          String.format(
              ERR_ARG_DUPLICATE_NAME, validationContext, argName));
    }
    if (REFERENCE.isSameType(argType)) {
      // find referenced arg or throw
      if (!methodArgs.containsKey(argName)) {
        throw new UtamCompilationError(
            String.format(
                ERR_REFERENCE_MISSING, validationContext, argName));
      }
      statementArgs.put(argName, methodArgs.get(argName));
      return methodArgs.get(argName);
    }
    if (!methodArgs.isEmpty()) {
      // if method level args were provided, find referenced arg or throw
      if (!methodArgs.containsKey(argName)) {
        throw new UtamCompilationError(
            String.format(
                ERR_REFERENCE_MISSING, validationContext, argName));
      }
      TypeProvider declaredType = methodArgs.get(argName).getType();
      if (!argType.isSameType(declaredType)) {
        throw new UtamCompilationError(
            String.format(
                ERR_ARG_TYPE_MISMATCH, validationContext, argName, argType.getSimpleName()));
      }
    }
    statementArgs.put(argName, parameter);
    return parameter;
  }
}
