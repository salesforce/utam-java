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
import utam.compiler.helpers.ParameterUtils;
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

  /**
   * interface can declare public basic elements
   *
   * @author elizaveta.ivanova
   * @since 236
   */
  public static class AbstractBasicElementGetter extends BasicElementGetterMethod {

    private static List<TypeProvider> getImports(TypeProvider returnType, List<MethodParameter> parameters) {
      List<TypeProvider> imports = new ArrayList<>();
      ParameterUtils.setImport(imports, returnType);
      ParameterUtils.setDeclarationImports(imports, parameters);
      return imports;
    }

    private final MethodDeclaration declaration;
    private final UnionType unionType;

    public AbstractBasicElementGetter(
        String methodName,
        List<MethodParameter> parameters,
        TypeProvider returnType) {
      declaration = new MethodDeclarationImpl(methodName, parameters, returnType, getImports(returnType, parameters));
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
    public boolean isPublic() {
      return true;
    }

    @Override
    public UnionType getInterfaceUnionType() {
      return unionType;
    }
  }
}
