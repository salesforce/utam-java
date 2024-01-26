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
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import utam.compiler.helpers.ParameterUtils;
import utam.compiler.representation.JavadocObject.EmptyJavadoc;
import utam.core.declarative.representation.MethodDeclaration;
import utam.core.declarative.representation.MethodParameter;
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
  private static final String BASE_METHOD_NAME = "getRootElement";
  private static final String PUBLIC_METHOD_NAME = "getRoot";

  /**
   * Protected getter for root element of default type
   *
   * @since 236
   */
  public static class ProtectedDefaultType extends BasicElementGetterMethod {

    /**
     * Initializes a new instance of the ProtectedDefaultType class
     *
     * @param returnType the return type
     */
    public ProtectedDefaultType(TypeProvider returnType) {
      super(BASE_METHOD_NAME, false, returnType, null);
    }

    @Override
    public MethodDeclaration getDeclaration() {
      List<MethodParameter> parameters = new ArrayList<>();
      JavadocObject javadoc = new EmptyJavadoc();
      return new MethodDeclarationImpl(
          methodName,
          parameters,
          returnType,
          Stream.of(returnType).collect(Collectors.toList()),
          javadoc);
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
  public static class PublicDefaultType extends BasicElementGetterMethod {

    private final List<TypeProvider> imports;

    /**
     * Initializes a new instance of the PublicDefaultType class
     *
     * @param returnType the default return type
     */
    public PublicDefaultType(TypeProvider returnType) {
      super(PUBLIC_METHOD_NAME, true, returnType, null);
      this.imports = Collections.singletonList(returnType);
    }

    @Override
    public MethodDeclaration getDeclaration() {
      List<MethodParameter> parameters = new ArrayList<>();
      JavadocObject javadoc = new EmptyJavadoc();
      return new MethodDeclarationImpl(methodName, parameters, returnType, imports, javadoc);
    }

    @Override
    public List<String> getCodeLines() {
      return Collections.singletonList("return this.getRootElement()");
    }

    @Override
    public List<TypeProvider> getClassImports() {
      return imports;
    }
  }

  /**
   * Public getter for root element of custom union type
   *
   * @since 236
   */
  public static class PublicCustomType extends BasicElementGetterMethod {

    static final String PROXY_CODE_LINE_TEMPLATE =
        "return getProxy(this.getRootElement(), %s.class)";
    private final List<TypeProvider> imports = new ArrayList<>();
    private final List<TypeProvider> classImports = new ArrayList<>();

    /**
     * Initializes a new instance of the PublicCustomType class
     *
     * @param unionType the union type that is the return type
     */
    public PublicCustomType(UnionType unionType) {
      super(PUBLIC_METHOD_NAME, true, unionType, null);
      ParameterUtils.setImports(imports, unionType.getExtendedTypes());
      ParameterUtils.setImport(classImports, returnType);
    }

    @Override
    public MethodDeclaration getDeclaration() {
      List<MethodParameter> parameters = new ArrayList<>();
      JavadocObject javadoc = new EmptyJavadoc();
      return new MethodDeclarationImpl(methodName, parameters, returnType, imports, javadoc);
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
    public UnionType getInterfaceUnionType() {
      return asUnionTypeOrNull(returnType);
    }
  }

  /**
   * Private getter for root element of custom union type
   *
   * @since 236
   */
  public static class PrivateCustomType extends BasicElementGetterMethod {

    /**
     * Initializes a new instance of the PrivateCustomtype class
     *
     * @param unionType the union type that is the return type
     */
    public PrivateCustomType(UnionType unionType) {
      super(PUBLIC_METHOD_NAME, false, unionType, null);
    }

    @Override
    public MethodDeclaration getDeclaration() {
      List<MethodParameter> parameters = new ArrayList<>();
      JavadocObject javadoc = new EmptyJavadoc();
      return new MethodDeclarationImpl(methodName, parameters, this.returnType, javadoc);
    }

    @Override
    public List<TypeProvider> getClassImports() {
      return new ArrayList<>();
    }

    @Override
    public List<String> getCodeLines() {
      String type = this.returnType.getSimpleName();
      return Collections.singletonList(
          String.format(PublicCustomType.PROXY_CODE_LINE_TEMPLATE, type));
    }

    @Override
    public UnionType getInterfaceUnionType() {
      return asUnionTypeOrNull(returnType);
    }
  }
}
