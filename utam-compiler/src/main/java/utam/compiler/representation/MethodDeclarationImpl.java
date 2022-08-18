/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.representation;

import static utam.compiler.helpers.ParameterUtils.getParametersDeclarationString;
import static utam.compiler.helpers.ParameterUtils.setDeclarationImports;
import static utam.compiler.helpers.ParameterUtils.setImport;

import java.util.ArrayList;
import java.util.List;
import utam.compiler.grammar.UtamMethodDescription;
import utam.core.declarative.representation.MethodDeclaration;
import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.TypeProvider;

/**
 * helper class to build method declaration
 *
 * @since 230
 */
class MethodDeclarationImpl implements MethodDeclaration {

  private final String methodName;
  private final List<TypeProvider> imports;
  private final List<MethodParameter> parameters;
  private final TypeProvider returnType;
  private final UtamMethodDescription.MethodDescription description;

  MethodDeclarationImpl(
      String methodName,
      List<MethodParameter> parameters,
      TypeProvider returnType,
      List<TypeProvider> imports,
      UtamMethodDescription description) {
    this.methodName = methodName;
    this.imports = imports;
    this.returnType = returnType;
    this.parameters = parameters;
    this.description = description == null? new UtamMethodDescription().getDescription(this) : description.getDescription(this);
  }

  MethodDeclarationImpl(
      String methodName,
      List<MethodParameter> parameters,
      TypeProvider returnType,
      UtamMethodDescription description) {
    this(methodName, parameters, returnType, buildImports(returnType, parameters), description);
  }

  // used in tests
  MethodDeclarationImpl(
      List<MethodParameter> parameters,
      TypeProvider returnType) {
    this("test", parameters, returnType, null);
  }

  private static List<TypeProvider> buildImports(
      TypeProvider returnType,
      List<MethodParameter> methodParameters) {
    List<TypeProvider> imports = new ArrayList<>();
    setDeclarationImports(imports, methodParameters);
    setImport(imports, returnType);
    return imports;
  }

  @Override
  public final List<MethodParameter> getParameters() {
    return parameters;
  }

  /**
   * Gets the list of imports for the method declaration
   *
   * @return the list of imports for the method declaration
   */
  public final List<TypeProvider> getImports() {
    return imports;
  }

  @Override
  public final String getName() {
    return methodName;
  }

  @Override
  public final TypeProvider getReturnType() {
    return returnType;
  }

  @Override
  public final String getCodeLine() {
    String parametersStr = getParametersDeclarationString(parameters);
    return String.format("%s %s(%s)", getReturnType().getReturnString(), methodName, parametersStr);
  }

  @Override
  public List<String> getDescription() {
    return description.getJavadoc();
  }

  @Override
  public boolean isDeprecated() {
    return description.isDeprecated();
  }
}
