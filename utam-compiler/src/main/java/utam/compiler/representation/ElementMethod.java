/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.representation;

import static utam.compiler.helpers.ParameterUtils.EMPTY_PARAMETERS;
import static utam.compiler.helpers.ParameterUtils.getParametersValuesString;
import static utam.compiler.helpers.TypeUtilities.BASIC_ELEMENT;
import static utam.compiler.helpers.TypeUtilities.wrapAsList;
import static utam.compiler.translator.TranslationUtilities.getElementGetterMethodName;
import static utam.compiler.types.BasicElementUnionType.asUnionTypeOrNull;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import utam.compiler.helpers.ElementContext;
import utam.compiler.helpers.MatcherType;
import utam.compiler.helpers.ParameterUtils;
import utam.compiler.helpers.TypeUtilities.FromClass;
import utam.core.declarative.representation.MethodDeclaration;
import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.PageObjectMethod;
import utam.core.declarative.representation.TypeProvider;
import utam.core.declarative.representation.UnionType;
import utam.core.driver.Document;

/**
 * generate code of getter method for basic element
 *
 * @author elizaveta.ivanova
 * @since 226
 */
public abstract class ElementMethod {

  private static final TypeProvider DOCUMENT_TYPE = new FromClass(Document.class);
  private static final MethodDeclaration DOCUMENT_GETTER_DECLARATION = new MethodDeclarationImpl(
      "getDocument",
      EMPTY_PARAMETERS,
      DOCUMENT_TYPE,
      Collections.emptyList());
  public static final PageObjectMethod DOCUMENT_GETTER = new PageObjectMethod() {
    @Override
    public MethodDeclaration getDeclaration() {
      return DOCUMENT_GETTER_DECLARATION;
    }

    @Override
    public List<String> getCodeLines() {
      return Collections
          .singletonList("this.getDocument()");
    }

    @Override
    public List<TypeProvider> getClassImports() {
      return Collections.emptyList();
    }

    @Override
    public boolean isPublic() {
      return false;
    }
  };

  static String getScopeElementCode(ElementContext scopeElement) {
    String methodName = scopeElement.getElementMethod().getDeclaration().getName();
    String scopeVariableName = scopeElement.getName();
    String parameters = ParameterUtils.getParametersValuesString(scopeElement.getParameters());
    return String.format("BasicElement %s = this.%s(%s)", scopeVariableName, methodName, parameters);
  }

  static String getElementLocationCode(String locationFieldName, List<MethodParameter> locatorParameters) {
    if(locatorParameters.isEmpty()) {
      return String.format("this.%s", locationFieldName);
    }
    return String.format("this.%s.setParameters(%s)", locationFieldName, getParametersValuesString(locatorParameters));
  }

  private static List<String> getElementMethodCode(ElementContext element,
      List<MethodParameter> locatorParameters, TypeProvider implClass, boolean isList) {
    List<String> code = new ArrayList<>();
    code.add(getScopeElementCode(element.getScopeElement()));
    String scopeVariableName = element.getScopeElement().getName();
    code.add(String.format("return basic(%s, %s).%s(%s.class, %s.class)",
        scopeVariableName,
        getElementLocationCode(element.getName(), locatorParameters),
        isList? "buildList" : "build",
        element.getType().getSimpleName(),
        implClass.getSimpleName()));
    return code;
  }

  static String getPredicateCode(
      String applyMethod,
      List<MethodParameter> applyParameters,
      MatcherType matcherType,
      List<MethodParameter> matcherParameters) {
    String applyInvocationCode =
        String.format("elm.%s(%s)", applyMethod, getParametersValuesString(applyParameters));
    String matcherCode = matcherType.getCode(applyInvocationCode, matcherParameters);
    return String.format("elm -> %s", matcherCode);
  }

  private static void setInterfaceImports(List<TypeProvider> imports, TypeProvider returnType) {
    ParameterUtils.setImport(imports, returnType);
  }

  private static void setClassImports(List<TypeProvider> classImports, TypeProvider interfaceType,
      TypeProvider implType) {
    ParameterUtils.setImport(classImports, interfaceType);
    ParameterUtils.setImport(classImports, implType);
    ParameterUtils.setImport(classImports, BASIC_ELEMENT);
  }

  public static final class Single extends BasicElementGetterMethod {

    private final List<String> methodCode;
    private final TypeProvider returnType;
    private final List<MethodParameter> parameters;
    private final String methodName;
    private final boolean isPublic;
    private final List<TypeProvider> imports = new ArrayList<>();
    private final List<TypeProvider> classImports = new ArrayList<>();
    private final UnionType unionType;

    public Single(ElementContext element, List<MethodParameter> locatorParameters, boolean isPublic, TypeProvider implType) {
      this.methodCode = getElementMethodCode(element, locatorParameters, implType, false);
      this.parameters = element.getParameters();
      this.methodName = getElementGetterMethodName(element.getName(), isPublic);
      this.returnType = element.getGetterReturnType();
      this.isPublic = isPublic;
      setInterfaceImports(imports, returnType);
      setClassImports(classImports, returnType, implType);
      this.unionType = asUnionTypeOrNull(implType);
    }

    @Override
    public MethodDeclaration getDeclaration() {
      return new MethodDeclarationImpl(methodName, parameters, returnType, imports);
    }

    @Override
    public List<TypeProvider> getClassImports() {
      return classImports;
    }

    @Override
    public List<String> getCodeLines() {
      return methodCode;
    }

    @Override
    public boolean isPublic() {
      return this.isPublic;
    }

    @Override
    public UnionType getInterfaceUnionType() {
      return asUnionTypeOrNull(returnType);
    }

    @Override
    public UnionType getClassUnionType() {
      return unionType;
    }
  }

  public static final class Multiple extends BasicElementGetterMethod {

    private final List<String> methodCode;
    private final TypeProvider listReturnType;
    private final List<MethodParameter> parameters;
    private final String methodName;
    private final boolean isPublic;
    private final List<TypeProvider> imports = new ArrayList<>();
    private final List<TypeProvider> classImports = new ArrayList<>();
    private final UnionType unionType;

    public Multiple(ElementContext element, List<MethodParameter> locatorParameters, boolean isPublic, TypeProvider implType) {
      this.methodCode = getElementMethodCode(element, locatorParameters, implType, true);
      this.parameters = element.getParameters();
      this.methodName = getElementGetterMethodName(element.getName(), isPublic);
      this.listReturnType = element.getGetterReturnType();
      this.isPublic = isPublic;
      setInterfaceImports(imports, listReturnType);
      setClassImports(classImports, listReturnType, implType);
      this.unionType = asUnionTypeOrNull(implType);
    }

    @Override
    public MethodDeclaration getDeclaration() {
      return new MethodDeclarationImpl(
          methodName,
          parameters,
          listReturnType,
          imports);
    }

    @Override
    public List<TypeProvider> getClassImports() {
      return classImports;
    }

    @Override
    public List<String> getCodeLines() {
      return methodCode;
    }

    @Override
    public boolean isPublic() {
      return this.isPublic;
    }

    @Override
    public UnionType getInterfaceUnionType() {
      return asUnionTypeOrNull(listReturnType);
    }

    @Override
    public UnionType getClassUnionType() {
      return unionType;
    }
  }

  public static final class Filtered extends BasicElementGetterMethod {

    private final boolean isPublic;
    private final String methodName;
    private final TypeProvider returnType;
    private final MethodParametersTracker parametersTracker;
    private final List<String> code = new ArrayList<>();
    private final List<TypeProvider> imports = new ArrayList<>();
    private final List<TypeProvider> classImports = new ArrayList<>();
    private final UnionType unionType;

    public Filtered(
        ElementContext scopeElement,
        String elementName,
        TypeProvider elementType,
        TypeProvider implType,
        List<MethodParameter> locatorParameters,
        boolean isPublic,
        String applyMethod,
        List<MethodParameter> applyParameters,
        MatcherType matcherType,
        List<MethodParameter> matcherParameters,
        boolean isFindFirstMatch) {
      this.isPublic = isPublic;
      this.methodName = getElementGetterMethodName(elementName, isPublic);
      this.returnType = isFindFirstMatch ? elementType : wrapAsList(elementType);
      this.parametersTracker = new MethodParametersTracker(String.format("method '%s'", methodName));
      parametersTracker.setMethodParameters(scopeElement.getParameters());
      parametersTracker.setMethodParameters(locatorParameters);
      parametersTracker.setMethodParameters(applyParameters);
      parametersTracker.setMethodParameters(matcherParameters);
      setInterfaceImports(imports, returnType);
      setClassImports(classImports, returnType, implType);
      String predicateCode = getPredicateCode(applyMethod, applyParameters, matcherType, matcherParameters);
      code.add(getScopeElementCode(scopeElement));
      String scopeVariableName = scopeElement.getName();
      String locationCode = getElementLocationCode(elementName, locatorParameters);
      code.add(String.format(
          "return basic(%s, %s).%s(%s.class, %s.class, %s)",
          scopeVariableName,
          locationCode,
          isFindFirstMatch ? "build" : "buildList",
          elementType.getSimpleName(),
          implType.getSimpleName(),
          predicateCode));
      this.unionType = asUnionTypeOrNull(implType);
    }

    @Override
    public MethodDeclaration getDeclaration() {
      return new MethodDeclarationImpl(methodName, parametersTracker.getMethodParameters(), returnType, imports);
    }

    @Override
    public List<String> getCodeLines() {
      return code;
    }

    @Override
    public List<TypeProvider> getClassImports() {
      return classImports;
    }

    @Override
    public boolean isPublic() {
      return this.isPublic;
    }

    @Override
    public UnionType getInterfaceUnionType() {
      return asUnionTypeOrNull(returnType);
    }

    @Override
    public UnionType getClassUnionType() {
      return unionType;
    }
  }
}
