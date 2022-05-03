/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.representation;

import static utam.compiler.helpers.ParameterUtils.getParametersValuesString;
import static utam.compiler.helpers.TypeUtilities.BASIC_ELEMENT;
import static utam.compiler.helpers.TypeUtilities.wrapAsList;
import static utam.compiler.translator.TranslationUtilities.getElementGetterMethodName;
import static utam.compiler.types.BasicElementUnionType.asUnionTypeOrNull;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import utam.compiler.grammar.UtamMethodDescription;
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
      new ArrayList<>(),
      DOCUMENT_TYPE,
      Collections.emptyList(),
      null);

  /**
   * The Page Object method for a document getter
   */
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

  /**
   * represents an element getter method for a basic element
   */
  public static final class Single extends BasicElementGetterMethod {

    private final List<String> methodCode;
    private final List<TypeProvider> imports = new ArrayList<>();
    private final List<TypeProvider> classImports = new ArrayList<>();
    private final UnionType unionType;

    /**
     * Initializes a new instance of the Single class
     *
     * @param element           the element to get
     * @param locatorParameters the list of parameters for the locator of the element
     * @param isPublic          a value indicating whether the element is public
     * @param implType          the type provider for the implementation type
     * @param description       method description in Json
     */
    public Single(ElementContext element, List<MethodParameter> locatorParameters, boolean isPublic, TypeProvider implType, UtamMethodDescription description) {
      super(getElementGetterMethodName(element.getName(), isPublic), isPublic, element.getGetterReturnType(), description);
      this.methodCode = getElementMethodCode(element, locatorParameters, implType, false);
      this.parametersTracker.setMethodParameters(element.getParameters());
      setInterfaceImports(imports, returnType);
      setClassImports(classImports, returnType, implType);
      this.unionType = asUnionTypeOrNull(implType);
    }

    @Override
    public MethodDeclaration getDeclaration() {
      return new MethodDeclarationImpl(methodName, parametersTracker.getMethodParameters(), returnType, imports, description);
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
    public UnionType getInterfaceUnionType() {
      return asUnionTypeOrNull(returnType);
    }

    @Override
    public UnionType getClassUnionType() {
      return unionType;
    }
  }

  /**
   * represents an element getter method for a list of basic element
   */
  public static final class Multiple extends BasicElementGetterMethod {

    private final List<String> methodCode;
    private final TypeProvider listReturnType;
    private final List<TypeProvider> imports = new ArrayList<>();
    private final List<TypeProvider> classImports = new ArrayList<>();
    private final UnionType unionType;

    /**
     * Initializes a new instance of the Multiple class
     *
     * @param element           the element to get
     * @param locatorParameters the list of parameters for the locator of the element
     * @param isPublic          a value indicating whether the element is public
     * @param implType          the type provider for the implementation type
     * @param description       method description in Json
     */
    public Multiple(ElementContext element, List<MethodParameter> locatorParameters, boolean isPublic, TypeProvider implType, UtamMethodDescription description) {
      super(getElementGetterMethodName(element.getName(), isPublic), isPublic, null, description);
      this.methodCode = getElementMethodCode(element, locatorParameters, implType, true);
      this.parametersTracker.setMethodParameters(element.getParameters());
      this.listReturnType = element.getGetterReturnType();
      setInterfaceImports(imports, listReturnType);
      setClassImports(classImports, listReturnType, implType);
      this.unionType = asUnionTypeOrNull(implType);
    }

    @Override
    public MethodDeclaration getDeclaration() {
      return new MethodDeclarationImpl(
          methodName,
          parametersTracker.getMethodParameters(),
          listReturnType,
          imports, description);
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
    public UnionType getInterfaceUnionType() {
      return asUnionTypeOrNull(listReturnType);
    }

    @Override
    public UnionType getClassUnionType() {
      return unionType;
    }
  }

  /**
   * represents an element getter method for a filtered element
   */
  public static final class Filtered extends BasicElementGetterMethod {

    private final List<String> code = new ArrayList<>();
    private final List<TypeProvider> imports = new ArrayList<>();
    private final List<TypeProvider> classImports = new ArrayList<>();
    private final UnionType unionType;

    /**
     * Initializes a new instance of the Filtered class
     *
     * @param scopeElement      the context of the scope element
     * @param elementName       the name of the element
     * @param elementType       the element type
     * @param implType          the type provider for the implementation type
     * @param locatorParameters the list of parameters for the locator of the element
     * @param isPublic          a value indicating whether the element is public
     * @param applyMethod       the method to apply in filtering the element
     * @param applyParameters   the list of parameters to use in the filter
     * @param matcherType       the type of matcher for the filter
     * @param matcherParameters the list of parameters for the matcher
     * @param isFindFirstMatch  a value indicting whether to only return the first match of the filter
     * @param description       method description in Json
     */
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
        boolean isFindFirstMatch,
        UtamMethodDescription description) {
      super(getElementGetterMethodName(elementName, isPublic), isPublic,
          isFindFirstMatch ? elementType : wrapAsList(elementType), description);
      parametersTracker.setMethodParameters(scopeElement.getParameters());
      parametersTracker.setMethodParameters(locatorParameters);
      parametersTracker.setMethodParameters(applyParameters);
      parametersTracker.setMethodParameters(matcherParameters);
      setInterfaceImports(imports, returnType);
      setClassImports(classImports, returnType, implType);
      setFilterParameterClassImports();
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

    private void setFilterParameterClassImports() {
      // For non-primitive method parameters added by a filter, we must add class imports
      // for those types or the generated code will not be compilable by javac.
      parametersTracker.getMethodParameters()
          .forEach(param -> ParameterUtils.setImport(classImports, param.getType()));
    }

    @Override
    public MethodDeclaration getDeclaration() {
      return new MethodDeclarationImpl(methodName, parametersTracker.getMethodParameters(), returnType, imports, description);
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
    public UnionType getInterfaceUnionType() {
      return asUnionTypeOrNull(returnType);
    }

    @Override
    public UnionType getClassUnionType() {
      return unionType;
    }
  }
}
