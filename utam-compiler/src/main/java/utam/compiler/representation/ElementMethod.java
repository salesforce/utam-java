/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.representation;

import static utam.compiler.helpers.ParameterUtils.getParametersValuesString;
import static utam.compiler.helpers.TypeUtilities.LIST_IMPORT;
import static utam.compiler.translator.TranslationUtilities.getElementGetterMethodName;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import utam.compiler.helpers.ElementContext;
import utam.compiler.helpers.MatcherType;
import utam.compiler.helpers.TypeUtilities;
import utam.core.declarative.representation.MethodDeclaration;
import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.PageObjectMethod;
import utam.core.declarative.representation.TypeProvider;

/**
 * generate code of getter method for basic element
 *
 * @author elizaveta.ivanova
 * @since 226
 */
public abstract class ElementMethod implements PageObjectMethod {

  static final String BASE_PAGE_OBJECT_METHOD = "element";
  private static final String BUILDER_METHOD = "build";
  static final String LIST_BUILDER_METHOD = "buildList";

  static String getSingleElementMethodCode(ElementContext element, boolean isNullable) {
    if (!element.getParameters().isEmpty()) {
      return String.format(
          "%s(%s).%s(%s.class, %s)",
          BASE_PAGE_OBJECT_METHOD,
          String.format("this.%s, %s", element.getName(), isNullable),
          BUILDER_METHOD,
          element.getType().getSimpleName(),
          getParametersValuesString(element.getParameters()));
    } else if (isNullable) {
      return String.format(
          "%s(%s).%s(%s.class)",
          BASE_PAGE_OBJECT_METHOD,
          String.format("this.%s, %s", element.getName(), isNullable),
          BUILDER_METHOD,
          element.getType().getSimpleName());
    } else {
      return String.format("this.%s", element.getName());
    }
  }

  static String getElementListMethodCode(ElementContext element, boolean isNullable) {
    String selectorParametersStr;
    if (!element.getParameters().isEmpty()) {
      selectorParametersStr = ", " + getParametersValuesString(element.getParameters());
    } else {
      selectorParametersStr = "";
    }
    return String.format(
        "%s(%s).%s(%s.class%s)",
        BASE_PAGE_OBJECT_METHOD,
        String.format("this.%s, %s", element.getName(), isNullable),
        LIST_BUILDER_METHOD,
        element.getType().getSimpleName(),
        selectorParametersStr);
  }

  static String getElementFilteredListMethodCode(
      String elementName,
      TypeProvider elementType,
      List<MethodParameter> elementParameters,
      String predicateCode,
      boolean isReturnFirstMatch,
      boolean isNullable) {
    String selectorParametersStr;
    if (!elementParameters.isEmpty()) {
      selectorParametersStr = ", " + getParametersValuesString(elementParameters);
    } else {
      selectorParametersStr = "";
    }
    String elementInstance = String.format("this.%s, %s", elementName, isNullable);
    String builderMethod = isReturnFirstMatch ? BUILDER_METHOD : LIST_BUILDER_METHOD;
    return String.format(
        "%s(%s).%s(%s.class, %s%s)",
        BASE_PAGE_OBJECT_METHOD,
        elementInstance,
        builderMethod,
        elementType.getSimpleName(),
        predicateCode,
        selectorParametersStr);
  }

  static String getPredicateCode(
      String applyMethod,
      List<MethodParameter> applyParameters,
      MatcherType matcherType,
      List<MethodParameter> matcherParameters) {
    String applyInvocationCode =
        String.format("elm.%s(%s)", applyMethod, getParametersValuesString(applyParameters));
    String matcherCode = matcherType.getCode(matcherParameters, applyInvocationCode);
    return String.format("elm -> %s", matcherCode);
  }

  public static final class Single implements PageObjectMethod {

    private final String methodCode;
    private final TypeProvider returnType;
    private final List<MethodParameter> parameters;
    private final String methodName;
    private final boolean isPublic;

    public Single(ElementContext element, boolean isPublic, boolean isNullable) {
      this.methodCode = getSingleElementMethodCode(element, isNullable);
      this.parameters = element.getParameters();
      this.methodName = getElementGetterMethodName(element.getName(), isPublic);
      this.returnType = element.getType();
      this.isPublic = isPublic;
    }

    @Override
    public MethodDeclaration getDeclaration() {
      return new MethodDeclarationImpl(
          methodName, parameters, returnType, Stream.of(returnType).collect(Collectors.toList()));
    }

    @Override
    public List<TypeProvider> getClassImports() {
      return getDeclaration().getImports();
    }

    @Override
    public List<String> getCodeLines() {
      return Stream.of(methodCode).collect(Collectors.toList());
    }

    @Override
    public boolean isPublic() {
      return this.isPublic;
    }
  }

  public static final class Multiple implements PageObjectMethod {

    private final String methodCode;
    private final TypeProvider returnType;
    private final List<MethodParameter> parameters;
    private final String methodName;
    private final boolean isPublic;

    public Multiple(ElementContext element, boolean isPublic, boolean isNullable) {
      this.methodCode = ElementMethod.getElementListMethodCode(element, isNullable);
      this.parameters = element.getParameters();
      this.methodName = getElementGetterMethodName(element.getName(), isPublic);
      this.returnType = element.getType();
      this.isPublic = isPublic;
    }

    @Override
    public MethodDeclaration getDeclaration() {
      return new MethodDeclarationImpl(
          methodName,
          parameters,
          new TypeUtilities.ListOf(returnType),
          Stream.of(returnType, LIST_IMPORT).collect(Collectors.toList()));
    }

    @Override
    public List<TypeProvider> getClassImports() {
      return getDeclaration().getImports();
    }

    @Override
    public List<String> getCodeLines() {
      return Stream.of(methodCode).collect(Collectors.toList());
    }

    @Override
    public boolean isPublic() {
      return this.isPublic;
    }
  }

  public static final class Filtered implements PageObjectMethod {

    private final boolean isPublic;
    private final String methodName;
    private final TypeProvider returnType;
    private final List<MethodParameter> parameters;
    private final List<String> codeLines = new ArrayList<>();
    private final TypeProvider returnListType;
    private final List<TypeProvider> imports = new ArrayList<>();

    public Filtered(
        String elementName,
        TypeProvider elementType,
        List<MethodParameter> elementParameters,
        boolean isPublic,
        String applyMethod,
        List<MethodParameter> applyParameters,
        MatcherType matcherType,
        List<MethodParameter> matcherParameters,
        boolean isFindFirstMatch,
        boolean isNullable) {
      this.isPublic = isPublic;
      this.methodName = getElementGetterMethodName(elementName, isPublic);
      this.returnType = elementType;
      this.returnListType = isFindFirstMatch ? null : new TypeUtilities.ListOf(returnType);
      this.parameters = new ArrayList<>(elementParameters);
      this.parameters.addAll(applyParameters);
      this.parameters.addAll(matcherParameters);
      this.imports.add(returnType);
      if (returnListType != null) {
        this.imports.add(returnListType);
      }
      this.codeLines.add(
          getElementFilteredListMethodCode(
              elementName,
              elementType,
              elementParameters,
              getPredicateCode(applyMethod, applyParameters, matcherType, matcherParameters),
              isFindFirstMatch, isNullable));
    }

    @Override
    public MethodDeclaration getDeclaration() {
      TypeProvider returns =
          returnListType != null ? new TypeUtilities.ListOf(returnType) : returnType;
      return new MethodDeclarationImpl(methodName, parameters, returns, imports);
    }

    @Override
    public List<String> getCodeLines() {
      return codeLines;
    }

    @Override
    public List<TypeProvider> getClassImports() {
      return getDeclaration().getImports();
    }

    @Override
    public boolean isPublic() {
      return this.isPublic;
    }
  }
}
