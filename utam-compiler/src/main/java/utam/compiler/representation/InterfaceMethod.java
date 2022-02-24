/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.representation;

import static utam.compiler.types.BasicElementUnionType.asUnionTypeOrNull;

import java.util.ArrayList;
import java.util.List;
import utam.compiler.grammar.UtamMethodDescription;
import utam.core.declarative.representation.MethodDeclaration;
import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.PageObjectMethod;
import utam.core.declarative.representation.TypeProvider;
import utam.core.declarative.representation.UnionType;

/**
 * method declared inside interface
 *
 * @author elizaveta.ivanova
 * @since 226
 */
public class InterfaceMethod extends MethodDeclarationImpl implements PageObjectMethod {

  private static final List<String> EMPTY_CODE = new ArrayList<>();

  /**
   * Initializes a new instance of the InterfaceMethod class
   * @param methodName       the name of the method
   * @param returnType       the return type of the method
   * @param methodParameters the list of parameters for the method
   */
  public InterfaceMethod(
      String methodName,
      TypeProvider returnType,
      List<MethodParameter> methodParameters,
      UtamMethodDescription methodDescription) {
    super(
        methodName,
        methodParameters,
        returnType, methodDescription);
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

  /**
   * interface can declare public basic elements
   *
   * @author elizaveta.ivanova
   * @since 236
   */
  public static class AbstractBasicElementGetter extends BasicElementGetterMethod {

    private final MethodDeclaration declaration;
    private final UnionType unionType;

    /**
     * Initializes a new instance of the AbstractBasicElementGetter class
     * @param methodName the name of the method
     * @param parameters the list of parameters of the method
     * @param returnType the return type of the method
     */
    public AbstractBasicElementGetter(
        String methodName,
        List<MethodParameter> parameters,
        TypeProvider returnType,
        UtamMethodDescription description) {
      super(methodName, true, returnType, description);
      this.declaration = new MethodDeclarationImpl(methodName, parameters, returnType, description);
      this.unionType = asUnionTypeOrNull(returnType);
    }

    @Override
    public MethodDeclaration getDeclaration() {
      return declaration;
    }

    @Override
    public List<String> getCodeLines() {
      return EMPTY_CODE;
    }

    @Override
    public List<TypeProvider> getClassImports() {
      return new ArrayList<>();
    }

    @Override
    public UnionType getInterfaceUnionType() {
      return unionType;
    }
  }
}
