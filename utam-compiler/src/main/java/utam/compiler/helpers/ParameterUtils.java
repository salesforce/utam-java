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
import static utam.compiler.helpers.TypeUtilities.isCustomType;
import static utam.compiler.translator.TranslationUtilities.isImportableType;

import java.util.List;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.TypeProvider;

/**
 * @author elizaveta.ivanova
 * @since 228
 */
public class ParameterUtils {

  /**
   * Gets the parameter values string from a list of parameters
   *
   * @param parameters the list of method parameters
   * @return the parameter list as a comma-delimited string
   */
  public static String getParametersValuesString(List<MethodParameter> parameters) {
    return parameters.stream().map(MethodParameter::getValue).collect(Collectors.joining(", "));
  }

  /**
   * Get comma separated string of non literal parameters for a method
   *
   * @param parameters list of parameters
   * @return string to use in a method declaration
   */
  public static String getParametersDeclarationString(List<MethodParameter> parameters) {
    return parameters.stream()
        .filter(p -> !p.isLiteral())
        .map(MethodParameter::getDeclaration)
        .collect(Collectors.joining(", "));
  }

  /**
   * Check if parameter has expected type. For literal parameters logic can be different than just
   * comparing types
   *
   * @param parameter parameter in question, can be null
   * @param expectedType type to compare with, can be null
   * @return true if type is as expected
   */
  static boolean isExpectedType(MethodParameter parameter, TypeProvider expectedType) {
    if (parameter == null || expectedType == null) {
      return true;
    }
    if (parameter instanceof LiteralPageObjectTypeParameter) {
      return ((LiteralPageObjectTypeParameter) parameter).isSameType(expectedType);
    }
    return parameter.getType().isSameType(expectedType);
  }

  /**
   * translates method parameters into a list of types to import in interface
   *
   * @param imports list of existing imports
   * @param parameters method parameters
   */
  public static void setDeclarationImports(
      List<TypeProvider> imports, List<MethodParameter> parameters) {
    parameters.stream().filter(p -> !p.isLiteral()).forEach(p -> setImport(imports, p.getType()));
  }

  /**
   * translates method parameters into a list of types to import in class
   *
   * @param imports list of existing imports
   * @param parameters method parameters
   */
  public static void setImplementationImports(
      List<TypeProvider> imports, List<MethodParameter> parameters) {
    parameters.forEach(
        parameter -> {
          if (parameter.isLiteral()) { // literal selector and custom type require class imports
            TypeProvider literalParameterType = parameter.getType();
            if (SELECTOR.isSameType(literalParameterType)) {
              setImport(imports, SELECTOR);
            } else if (isCustomType(literalParameterType)) {
              setImport(imports, literalParameterType);
            }
          } else {
            setImport(imports, parameter.getType());
          }
        });
  }

  /**
   * Sets the imports for the parameter
   *
   * @param imports the list of types to be imported
   * @param type the type of the parameter
   */
  public static void setImport(List<TypeProvider> imports, TypeProvider type) {
    if (type == null || type.isSameType(VOID)) {
      return;
    }
    // predicate to check that this type was not already added to imports
    Predicate<TypeProvider> unique =
        typeProvider ->
            imports.stream()
                .map(TypeProvider::getFullName)
                .noneMatch(name -> name.equals(typeProvider.getFullName()));
    for (TypeProvider importable : type.getImportableTypes()) {
      if (isImportableType(importable) && unique.test(importable)) {
        imports.add(importable);
      }
    }
  }

  /**
   * Sets the imports for the parameter
   *
   * @param imports the list of types to be imported
   * @param types the list of types of the parameter
   */
  public static void setImports(List<TypeProvider> imports, List<TypeProvider> types) {
    types.forEach(t -> setImport(imports, t));
  }

  /** Represents a regular method parameter */
  public static class Regular implements MethodParameter {

    final String valueAsString;
    final TypeProvider type;
    private final String description;

    /**
     * Initializes a new instance of the Regular method parameter
     *
     * @param valueAsString the value of the parameter
     * @param type the type of the parameter
     * @param description description of the parameter role
     */
    public Regular(String valueAsString, TypeProvider type, String description) {
      this.valueAsString = valueAsString;
      this.type = type;
      this.description = description;
    }

    public Regular(String valueAsString, TypeProvider type) {
      this(valueAsString, type, null);
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
    public List<MethodParameter> getNestedParameters() {
      return null;
    }

    @Override
    public String getDescription() {
      return description;
    }
  }

  /** Represents a literal parameter */
  public static class Literal extends Regular {

    private final List<MethodParameter> nestedParameters;

    /**
     * Initializes a new instance of the Literal class
     *
     * @param value the value of the parameter
     * @param type the type of the parameter
     */
    public Literal(Object value, TypeProvider type) {
      this(value, type, null);
    }

    /**
     * Initializes a new instance of the Literal class
     *
     * @param value the value of the parameter
     * @param type the type of the parameter
     * @param nestedParameters the list of nested parameters
     */
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
    public boolean isLiteral() {
      return true;
    }

    @Override
    public List<MethodParameter> getNestedParameters() {
      return nestedParameters;
    }

    @Override
    public String getDeclaration() {
      return "";
    }
  }

  /** a literal class, for example utam.pageobjects.MyButton.class */
  public static class LiteralPageObjectTypeParameter extends Literal {

    private final TypeProvider baseType;

    /**
     * Initializes a new instance of the LiteralPageObjectClass class
     *
     * @param type the type of the Page Object
     * @param baseType the base type of the Page Object
     */
    public LiteralPageObjectTypeParameter(TypeProvider type, TypeProvider baseType) {
      super(String.format("%s.class", type.getSimpleName()), type);
      this.baseType = baseType;
    }

    boolean isSameType(TypeProvider expectedType) {
      return this.baseType.isSameType(expectedType);
    }
  }
}
