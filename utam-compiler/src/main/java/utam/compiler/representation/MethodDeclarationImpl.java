/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.representation;

import static utam.compiler.helpers.ParameterUtils.setImport;
import static utam.compiler.translator.TranslationUtilities.EMPTY_COMMENTS;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import utam.compiler.helpers.ParameterUtils;
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

  private final String comments;

  MethodDeclarationImpl(
      String methodName,
      List<MethodParameter> parameters,
      TypeProvider returnType) {
    this(methodName, parameters, returnType, buildImports(returnType, parameters));
  }

  MethodDeclarationImpl(
      String methodName,
      List<MethodParameter> parameters,
      TypeProvider returnType,
      List<TypeProvider> imports,
      String comments) {
    this.methodName = methodName;
    this.imports = imports;
    this.returnType = returnType;
    this.parameters = parameters;
    this.comments = comments;
  }

  MethodDeclarationImpl(
      String methodName,
      List<MethodParameter> parameters,
      TypeProvider returnType,
      List<TypeProvider> imports) {
    this(methodName, parameters, returnType, imports, EMPTY_COMMENTS);
  }

  private static List<TypeProvider> buildImports(
      TypeProvider returnType,
      List<MethodParameter> methodParameters) {
    List<TypeProvider> imports = new ArrayList<>();
    methodParameters.forEach(p -> ParameterUtils.setDeclarationImport(imports, p));
    setImport(imports, returnType);
    return imports;
  }

  @Override
  public final List<MethodParameter> getParameters() {
    return parameters;
  }

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
    return String.format("%s %s(%s)", getReturnTypeStr(), methodName, parametersStr);
  }

  // method can be overridden if return type is a bounded generic
  String getReturnTypeStr() {
    return getReturnType().getSimpleName();
  }

  @Override
  public String getComments() {
    return comments;
  }

  private static String getParametersDeclarationString(List<MethodParameter> parameters) {
    return parameters.stream()
        .map(MethodParameter::getDeclaration)
        .filter(str -> !str.isEmpty()) // hardcoded values passed as empty string
        .collect(Collectors.joining(", "));
  }
}
