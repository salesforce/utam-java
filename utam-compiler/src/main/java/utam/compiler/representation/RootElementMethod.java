/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.representation;


import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import utam.compiler.helpers.ParameterUtils;
import utam.core.declarative.representation.MethodDeclaration;
import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.PageObjectMethod;
import utam.core.declarative.representation.TypeProvider;
import utam.core.declarative.representation.UnionType;

/**
 * getter for root element
 *
 * @author elizaveta.ivanova
 * @since 232
 */
public class RootElementMethod {

  // this method must exist in BasePageObject
  private final static String BASE_METHOD_NAME = "getRootElement";
  private final static String PUBLIC_METHOD_NAME = "getRoot";
  private static final List<MethodParameter> EMPTY_PARAMETERS = new ArrayList<>();

  /**
   * Protected getter for root element of default type
   *
   * @since 236
   */
  public static class ProtectedDefaultType implements PageObjectMethod {

    private final TypeProvider returnType;

    public ProtectedDefaultType(TypeProvider returnType) {
      this.returnType = returnType;
    }

    @Override
    public boolean isPublic() {
      return false;
    }

    @Override
    public MethodDeclaration getDeclaration() {
      return new MethodDeclarationImpl(
          BASE_METHOD_NAME,
          EMPTY_PARAMETERS,
          returnType,
          Stream.of(returnType).collect(Collectors.toList()));
    }

    @Override
    public List<String> getCodeLines() {
      return new ArrayList<>(); // this method is never actually declared
    }

    @Override
    public List<TypeProvider> getClassImports() {
      return new ArrayList<>();
    }
  }

  /**
   * Public getter for root element of default type
   *
   * @since 236
   */
  public static class PublicDefaultType implements PageObjectMethod {

    private final List<TypeProvider> imports;
    private final TypeProvider returnType;

    public PublicDefaultType(TypeProvider returnType) {
      this.returnType = returnType;
      this.imports = Collections.singletonList(returnType);
    }

    @Override
    public MethodDeclaration getDeclaration() {
      return new MethodDeclarationImpl(
          PUBLIC_METHOD_NAME,
          EMPTY_PARAMETERS,
          returnType,
          imports);
    }

    @Override
    public List<String> getCodeLines() {
      return Collections.singletonList("return this.getRootElement()");
    }

    @Override
    public List<TypeProvider> getClassImports() {
      return imports;
    }

    @Override
    public boolean isPublic() {
      return true;
    }
  }

  /**
   * Public getter for root element of custom union type
   *
   * @since 236
   */
  public static class PublicCustomType implements PageObjectMethod {

    static final String PROXY_CODE_LINE_TEMPLATE = "return getProxy(this.getRootElement(), %s.class)";
    private final TypeProvider returnType;
    private final List<TypeProvider> imports = new ArrayList<>();
    private final List<TypeProvider> classImports = new ArrayList<>();

    public PublicCustomType(UnionType unionType) {
      this.returnType = unionType.getType();
      ParameterUtils.setImports(imports, unionType.getExtendedTypes());
      ParameterUtils.setImport(classImports, returnType);
    }

    @Override
    public MethodDeclaration getDeclaration() {
      return new MethodDeclarationImpl(
          PUBLIC_METHOD_NAME,
          EMPTY_PARAMETERS,
          this.returnType,
          imports);
    }

    @Override
    public List<TypeProvider> getClassImports() {
      return classImports;
    }

    @Override
    public List<String> getCodeLines() {
      String type = this.returnType.getSimpleName();
      return Collections.singletonList(String.format(PROXY_CODE_LINE_TEMPLATE, type));
    }

    @Override
    public boolean isPublic() {
      return true;
    }
  }

  /**
   * Private getter for root element of custom union type
   *
   * @since 236
   */
  public static class PrivateCustomType implements PageObjectMethod {

    private final TypeProvider returnType;
    private final List<TypeProvider> classImports = new ArrayList<>();

    public PrivateCustomType(UnionType unionType) {
      this.returnType = unionType.getType();
      ParameterUtils.setImports(classImports, unionType.getExtendedTypes());
    }

    @Override
    public boolean isPublic() {
      return false;
    }

    @Override
    public MethodDeclaration getDeclaration() {
      return new MethodDeclarationImpl(
          PUBLIC_METHOD_NAME,
          EMPTY_PARAMETERS,
          this.returnType,
          new ArrayList<>());
    }

    @Override
    public List<TypeProvider> getClassImports() {
      return classImports;
    }

    @Override
    public List<String> getCodeLines() {
      String type = this.returnType.getSimpleName();
      return Collections
          .singletonList(String.format(PublicCustomType.PROXY_CODE_LINE_TEMPLATE, type));
    }
  }
}
