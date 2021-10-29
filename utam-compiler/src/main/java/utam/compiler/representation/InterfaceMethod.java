/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.representation;

import java.util.ArrayList;
import java.util.List;
import utam.compiler.helpers.BasicElementUnionType;
import utam.compiler.helpers.TypeUtilities.ListOf;
import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.PageObjectMethod;
import utam.core.declarative.representation.TypeProvider;

/**
 * method declared inside interface
 *
 * @author elizaveta.ivanova
 * @since 226
 */
public class InterfaceMethod extends MethodDeclarationImpl implements PageObjectMethod {

  private static final List<String> EMPTY_CODE = new ArrayList<>();

  public InterfaceMethod(
      String methodName,
      TypeProvider returnType,
      List<MethodParameter> methodParameters) {
    super(
        methodName,
        methodParameters,
        returnType);
  }

  @Override
  public List<TypeProvider> getClassImports() {
    return getDeclaration().getImports();
  }

  @Override
  public MethodDeclarationImpl getDeclaration() {
    return this;
  }

  @Override
  public List<String> getCodeLines() {
    return EMPTY_CODE;
  }

  @Override
  public boolean isPublic() {
    return true;
  }

  @Override
  public boolean isReturnsBasicElement() {
    TypeProvider returnType =  getDeclaration().getReturnType();
    return isElementReturned(returnType);
  }

  private static boolean isElementReturned(TypeProvider returnType) {
    if(returnType instanceof BasicElementUnionType) {
      return true;
    }
    if(returnType instanceof ListOf) {
      return returnType.getBoundTypes().get(0) instanceof BasicElementUnionType;
    }
    return false;
  }
}
