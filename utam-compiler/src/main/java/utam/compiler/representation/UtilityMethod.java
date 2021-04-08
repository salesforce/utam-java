/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.representation;

import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.PageClassField;
import utam.core.declarative.representation.PageObjectMethod;
import utam.core.declarative.representation.TypeProvider;
import utam.compiler.helpers.TypeUtilities;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static utam.compiler.helpers.ParameterUtils.getParametersValuesString;
import static utam.compiler.helpers.TypeUtilities.LIST_IMPORT;
import static utam.compiler.translator.TranslationUtilities.EMPTY_COMMENTS;

/**
 * representation of chained method
 *
 * @author elizaveta.ivanova
 * @since 226
 */
public class UtilityMethod implements PageObjectMethod {

  private final List<TypeProvider> interfaceImports;
  private final List<TypeProvider> classImports;
  private final String name;
  private final TypeProvider returns;
  private final Utility reference;
  private final String comments;

  public UtilityMethod(String methodName, TypeProvider returns, boolean isReturnsList, Utility reference, String comments) {
    this.name = methodName;
    this.returns = isReturnsList ? new TypeUtilities.ListOf(returns) : returns;
    this.interfaceImports = Stream.of(returns).collect(Collectors.toList());
    if (isReturnsList) {
      interfaceImports.add(LIST_IMPORT);
    }
    this.reference = reference;
    this.classImports = new ArrayList<>(interfaceImports);
    this.classImports.add(reference.utilityField.getType());
    this.comments = comments;
  }

  // used in tests
  UtilityMethod(TypeProvider returns, boolean isReturnsList, Utility reference) {
    this("testMethod", returns, isReturnsList, reference, EMPTY_COMMENTS);
  }

  @Override
  public MethodDeclarationImpl getDeclaration() {
    return new MethodDeclarationImpl(name, reference.parameters, returns, interfaceImports, comments);
  }

  @Override
  public final List<String> getCodeLines() {
    return Stream.of(reference.getCode()).collect(Collectors.toList());
  }

  @Override
  public List<TypeProvider> getClassImports() {
    return classImports;
  }

  @Override
  public boolean isPublic() {
    return true;
  }

  public static class Utility {

    private final String applyMethod;
    private final List<MethodParameter> parameters;
    private final PageClassField utilityField;

    public Utility(
        String applyMethod, List<MethodParameter> parameters, PageClassField field) {
      this.applyMethod = applyMethod;
      this.parameters = parameters;
      this.utilityField = field;
    }

    String getCode() {
      return String.format(
          "this.%s.%s(%s)",
          utilityField.getName(), applyMethod, getParametersValuesString(parameters));
    }
  }
}
