/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.helpers;

import static utam.compiler.helpers.AnnotationUtils.getWrappedString;
import static utam.compiler.helpers.TypeUtilities.SELECTOR;
import static utam.compiler.helpers.TypeUtilities.VOID;

import utam.compiler.translator.TranslationUtilities;
import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.TypeProvider;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

/**
 * @author elizaveta.ivanova
 * @since 228
 */
public class ParameterUtils {

  public static final List<MethodParameter> EMPTY_PARAMETERS = new ArrayList<>();

  public static String getParametersValuesString(List<MethodParameter> parameters) {
    return parameters.stream()
        .map(MethodParameter::getValue)
        .collect(Collectors.joining(", "));
  }

  public static class Regular implements MethodParameter {

    final String valueAsString;
    final TypeProvider type;

    public Regular(String valueAsString, TypeProvider type) {
      this.valueAsString = valueAsString;
      this.type = type;
    }

    @Override
    public boolean isLiteral() {
      return false;
    }

    @Override
    public String getValue() {
      return valueAsString;
    }

    @Override
    public String getDeclaration() {
      return String.format("%s %s", type.getSimpleName(), getValue());
    }

    @Override
    public TypeProvider getType() {
      return type;
    }

    @Override
    public boolean equals(Object obj) {
      if (!(obj instanceof MethodParameter)) {
        return false;
      }
      return this.getDeclaration().equals(((MethodParameter)obj).getDeclaration());
    }

    @Override
    public List<MethodParameter> getNestedParameters() {
      return null;
    }
  }

  public static class Literal extends Regular {

    private final List<MethodParameter> nestedParameters;

    public Literal(Object value, TypeProvider type) {
      this(value, type, null);
    }

    public Literal(Object value, TypeProvider type, List<MethodParameter> nestedParameters) {
      super(value.toString(), type);
      this.nestedParameters = nestedParameters;
    }

    @Override
    public String getValue() {
      if (type.isSameType(PrimitiveType.STRING)) {
        return getWrappedString(valueAsString);
      }
      return super.getValue();
    }

    @Override
    public String getDeclaration() {
      return "";
    }

    @Override
    public boolean isLiteral() {
      return true;
    }

    @Override
    public List<MethodParameter> getNestedParameters() {
      return nestedParameters;
    }
  }

  /**
   * a literal class, for example utam.pageobjects.MyButton.class
   */
  public static class LiteralPageObjectClass extends Literal {

    public LiteralPageObjectClass(TypeProvider type) {
      super(String.format("%s.class", type.getSimpleName()), type);
    }
  }

  /**
   * translates method parameters into a list of types to import in interface
   * @param imports list of existing imports
   * @param parameter method parameter
   */
  public static void setDeclarationImport(List<TypeProvider> imports, MethodParameter parameter) {
    TypeProvider type = getDeclarationImport(parameter);
    if(type != null) {
      setImport(imports, type);
    }
  }

  /**
   * translates method parameters into a list of types to import in interface
   * @param imports list of existing imports
   * @param parameters method parameters
   */
  public static void setDeclarationImports(List<TypeProvider> imports, List<MethodParameter> parameters) {
    parameters.forEach(p -> setDeclarationImport(imports, p));
  }

  /**
   * translates method parameters into a list of types to import in class
   * @param imports list of existing imports
   * @param parameter method parameter
   */
  private static void setImplementationImport(List<TypeProvider> imports, MethodParameter parameter) {
    TypeProvider type = getImplementationImport(parameter);
    if(type != null) {
      setImport(imports, type);
    }
  }

  /**
   * translates method parameters into a list of types to import in class
   * @param imports list of existing imports
   * @param parameters method parameters
   */
  public static void setImplementationImports(List<TypeProvider> imports, List<MethodParameter> parameters) {
    parameters.forEach(p -> setImplementationImport(imports, p));
  }

  public static void setImport(List<TypeProvider> imports, TypeProvider type) {
    if(type == null || type.isSameType(VOID)) {
      return;
    }
    type.getImportableTypes()
        .stream()
        // only if importable
        .filter(TranslationUtilities::isImportableType)
        // and was not already added
        .filter(t -> imports.stream().map(TypeProvider::getFullName).noneMatch(iStr -> iStr.equals(t.getFullName())))
        .forEach(imports::add);
  }

  public static void setImports(List<TypeProvider> imports, List<TypeProvider> types) {
    types.forEach(t -> setImport(imports, t));
  }

  private static TypeProvider getDeclarationImport(MethodParameter parameter) {
    if (parameter.isLiteral()) {
      return null;
    }
    return parameter.getType();
  }

  private static TypeProvider getImplementationImport(MethodParameter parameter) {
    // selector literal requires imports
    if(parameter.isLiteral()) {
      if(SELECTOR.isSameType(parameter.getType())) {
        return SELECTOR;
      } else if(parameter instanceof LiteralPageObjectClass) {
        return parameter.getType();
      }
      return null;
    }
    return parameter.getType();
  }
}
