/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.helpers;

import static utam.compiler.helpers.TypeUtilities.LIST_IMPORT;
import static utam.compiler.helpers.TypeUtilities.VOID;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
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

  private final String methodName;
  // to keep track of element usages
  private final Map<String, ElementContext> elementNames = new HashMap<>();
  private final TypeProvider methodReturnType;
  private final boolean isReturnsList;
  private final TypeProvider listType;
  private static final String BEFORELOAD_METHOD_MANE = "load";

  public MethodContext(String methodName, TypeProvider returns, boolean isReturnsList) {
    this.methodName = methodName;
    if (isNullOrVoid(returns) && isReturnsList) {
      throw new UtamError(
          String.format("method '%s' cannot return list of null or void", methodName));
    }
    this.listType = returns;
    this.methodReturnType = isReturnsList ? new ListOf(returns) : returns;
    this.isReturnsList = isReturnsList;
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
        && !methodName.equals(BEFORELOAD_METHOD_MANE)) {
      throw new UtamError(String.format("method '%s' return type mismatch: "
              + "last statement returns '%s', method returns '%s'", methodName,
          lastStatementReturns.getSimpleName(), methodReturnType.getSimpleName()));
    }
    if (methodReturnType == null) {
      return lastStatement.getReturnType();
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
}
