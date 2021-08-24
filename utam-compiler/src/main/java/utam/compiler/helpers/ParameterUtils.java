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

import java.util.Objects;
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

  public static class Primitive extends Regular {

    public Primitive(String value, PrimitiveType type) {
      super(value, type);
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
   * a literal class, for example MyButton.class
   */
  public static class LiteralClass extends Literal {

    public LiteralClass(TypeProvider type) {
      super(String.format("%s.class", type.getSimpleName()), type);
    }
  }

  /**
   * translates method parameters and returns into a list of types to import
   * @param parameters method parameters
   * @param returnType return type
   * @return list of types to import
   */
  public static List<TypeProvider> getDeclarationImports(List<MethodParameter> parameters, TypeProvider returnType) {
    List<TypeProvider> imports = getDeclarationImports(parameters);
    imports.add(returnType);
    return imports;
  }

  /**
   * translates method parameters into a list of types to import
   * @param parameters method parameters
   * @return list of types to import
   */
  public static List<TypeProvider> getDeclarationImports(List<MethodParameter> parameters) {
    return parameters
        .stream()
        .map(ParameterUtils::getDeclarationImport)
        .filter(Objects::nonNull)
        .collect(Collectors.toList());
  }

  /**
   * translates method parameters into a list of types to import in class
   * @param parameters method parameters
   * @return list of types to import
   */
  public static List<TypeProvider> getImplementationImports(List<MethodParameter> parameters) {
    return parameters
        .stream()
        .map(ParameterUtils::getImplementationImport)
        .filter(Objects::nonNull)
        .collect(Collectors.toList());
  }

  private static TypeProvider getDeclarationImport(MethodParameter parameter) {
    if (parameter == null || parameter.isLiteral()) {
      return null;
    }
    return parameter.getType();
  }

  private static TypeProvider getImplementationImport(MethodParameter parameter) {
    if(parameter == null) {
      return null;
    }
    // some literals require imports
    if(parameter.isLiteral()) {
      if(SELECTOR.isSameType(parameter.getType())) {
        return SELECTOR;
      } else if(parameter instanceof LiteralClass) {
        return parameter.getType();
      }
      return null;
    }
    return parameter.getType();
  }
}
