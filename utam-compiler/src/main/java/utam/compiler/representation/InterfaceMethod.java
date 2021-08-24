/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.representation;

import static utam.compiler.helpers.ParameterUtils.getDeclarationImports;
import static utam.compiler.helpers.TypeUtilities.VOID;
import static utam.compiler.helpers.TypeUtilities.isElementReturned;

import java.util.ArrayList;
import java.util.List;
import utam.compiler.helpers.MethodContext;
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

  public InterfaceMethod(MethodContext methodContext, List<MethodParameter> methodParameters,
      String comments) {
    super(
        methodContext.getName(),
        methodParameters,
        methodContext.getReturnType(VOID),
        getDeclarationImports(methodParameters, methodContext.getReturnType(VOID)),
        comments);
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
  public boolean isBasicElementGetterMethod() {
    TypeProvider returnType =  getDeclaration().getReturnType();
    return isElementReturned(returnType);
  }
}
