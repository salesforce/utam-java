/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.representation;

import utam.compiler.grammar.UtamMethodDescription;
import utam.core.declarative.representation.PageObjectMethod;
import utam.core.declarative.representation.TypeProvider;
import utam.core.declarative.representation.UnionType;

/**
 * page element getter method
 *
 * @author elizaveta.ivanova
 * @since 236
 */
public abstract class BasicElementGetterMethod implements PageObjectMethod {

  final String methodName;
  final TypeProvider returnType;
  private final boolean isPublic;
  final MethodParametersTracker parametersTracker;
  final UtamMethodDescription description;

  BasicElementGetterMethod(
      String methodName,
      boolean isPublic,
      TypeProvider returnType,
      UtamMethodDescription description) {
    this.methodName = methodName;
    this.returnType = returnType;
    this.isPublic = isPublic;
    this.parametersTracker = new MethodParametersTracker(String.format("method '%s'", methodName));
    this.description = description;
  }

  /**
   * union type declared for a basic element. Can be null if element does not have one
   *
   * @return instance of a union type
   */
  public UnionType getInterfaceUnionType() {
    return null;
  }

  /**
   * union type declared for a basic element. Can be null if element does not have one
   *
   * @return instance of a union type
   */
  public UnionType getClassUnionType() {
    return null;
  }

  @Override
  public final boolean isPublic() {
    return isPublic;
  }
}
