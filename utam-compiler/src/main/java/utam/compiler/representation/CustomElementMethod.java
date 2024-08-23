/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.representation;

import static utam.compiler.helpers.TypeUtilities.BASIC_ELEMENT;
import static utam.compiler.helpers.TypeUtilities.wrapAsList;
import static utam.compiler.representation.ElementMethod.getElementLocationCode;
import static utam.compiler.representation.ElementMethod.getPredicateCode;
import static utam.compiler.representation.ElementMethod.setupScopeElement;
import static utam.compiler.translator.TranslationUtilities.getElementGetterMethodName;

import java.util.ArrayList;
import java.util.List;
import utam.compiler.grammar.UtamMethodDescription;
import utam.compiler.helpers.ElementContext;
import utam.compiler.helpers.MatcherType;
import utam.compiler.helpers.ParameterUtils;
import utam.compiler.representation.JavadocObject.MethodJavadoc;
import utam.core.declarative.representation.MethodDeclaration;
import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.PageObjectMethod;
import utam.core.declarative.representation.TypeProvider;

/**
 * generate code of getter method for custom element
 *
 * @author elizaveta.ivanova
 * @since 224
 */
public abstract class CustomElementMethod implements PageObjectMethod {

  /**
   * getter method that returns custom single element
   *
   * @since 224
   */
  public static final class Single implements PageObjectMethod {

    private final List<String> codeLines = new ArrayList<>();
    private final List<TypeProvider> classImports = new ArrayList<>();
    private final List<TypeProvider> interfaceImports = new ArrayList<>();
    private final boolean isPublic;
    private final String methodName;
    private final MethodParametersTracker parametersTracker;
    private final TypeProvider returnType;
    private final UtamMethodDescription description;

    /**
     * Initializes a new instance of the Single class
     *
     * @param isPublic a value indicating whether the method is public
     * @param componentName the name of the component
     * @param locatorParameters the list of parameters of the method
     * @param scopeElement the scope element of the method
     * @param returnType the return type of the method
     * @param description method description in Json
     */
    public Single(
        boolean isPublic,
        String componentName,
        List<MethodParameter> locatorParameters,
        ElementContext scopeElement,
        TypeProvider returnType,
        UtamMethodDescription description) {
      this.isPublic = isPublic;
      this.methodName = getElementGetterMethodName(componentName, isPublic);
      this.parametersTracker =
          new MethodParametersTracker(String.format("method '%s'", methodName));
      codeLines.add(setupScopeElement(scopeElement, parametersTracker));
      String locationCode = getElementLocationCode(componentName, locatorParameters);
      String statement =
          String.format(
              "return custom(%s, %s).build(%s.class)",
              scopeElement.getName(), locationCode, returnType.getSimpleName());
      codeLines.add(statement);
      ParameterUtils.setImport(interfaceImports, returnType);
      ParameterUtils.setImport(classImports, returnType);
      ParameterUtils.setImport(classImports, BASIC_ELEMENT);
      parametersTracker.setMethodParameters(locatorParameters);
      this.returnType = returnType;
      this.description = description;
    }

    @Override
    public List<String> getCodeLines() {
      return codeLines;
    }

    @Override
    public final List<TypeProvider> getClassImports() {
      return classImports;
    }

    @Override
    public MethodDeclaration getDeclaration() {
      List<MethodParameter> parameters = parametersTracker.getMethodParameters();
      JavadocObject javadoc = new MethodJavadoc(methodName, returnType, parameters, description);
      return new MethodDeclarationImpl(
          methodName, parameters, returnType, interfaceImports, javadoc);
    }

    @Override
    public boolean isPublic() {
      return this.isPublic;
    }
  }

  /**
   * getter method that returns custom single element or list using filter
   *
   * @since 224
   */
  public static class Filtered implements PageObjectMethod {

    private final List<String> codeLines = new ArrayList<>();
    private final List<TypeProvider> classImports = new ArrayList<>();
    private final List<TypeProvider> interfaceImports = new ArrayList<>();
    private final boolean isPublic;
    private final String methodName;
    private final MethodParametersTracker parametersTracker;
    private final TypeProvider returnType;
    private final UtamMethodDescription description;

    /**
     * Initializes a new instance of the Filtered class
     *
     * @param isPublic a value indicating whether the method is public
     * @param componentName the name of the component
     * @param locatorParameters the list of parameters of the method
     * @param scopeElement the scope element of the method
     * @param returnType the return type of the method
     * @param applyMethod the method to apply for the filter
     * @param applyParameters the list of parameters to apply for the filter
     * @param matcherType the type of matcher for the filter
     * @param matcherParameters the list of parameters for the matcher
     * @param isFindFirst a value indicating whether the filter applies to only the first element
     *     found
     * @param description method description in Json
     */
    public Filtered(
        boolean isPublic,
        String componentName,
        List<MethodParameter> locatorParameters,
        ElementContext scopeElement,
        TypeProvider returnType,
        String applyMethod,
        List<MethodParameter> applyParameters,
        MatcherType matcherType,
        List<MethodParameter> matcherParameters,
        boolean isFindFirst,
        UtamMethodDescription description) {
      this.returnType = isFindFirst ? returnType : wrapAsList(returnType);
      ParameterUtils.setImport(interfaceImports, this.returnType);
      ParameterUtils.setImport(classImports, this.returnType);
      ParameterUtils.setImport(classImports, BASIC_ELEMENT);
      this.isPublic = isPublic;
      this.methodName = getElementGetterMethodName(componentName, isPublic);
      this.parametersTracker =
          new MethodParametersTracker(String.format("method '%s'", methodName));
      codeLines.add(setupScopeElement(scopeElement, parametersTracker));
      // must be after scope parameters
      parametersTracker.setMethodParameters(locatorParameters);
      parametersTracker.setMethodParameters(applyParameters);
      parametersTracker.setMethodParameters(matcherParameters);
      String locationCode = getElementLocationCode(componentName, locatorParameters);
      String predicate =
          getPredicateCode(applyMethod, applyParameters, matcherType, matcherParameters);
      String methodName = isFindFirst ? "build" : "buildList";
      String statement =
          String.format(
              "return custom(%s, %s).%s(%s.class, %s)",
              scopeElement.getName(),
              locationCode,
              methodName,
              returnType.getSimpleName(),
              predicate);
      codeLines.add(statement);
      this.description = description;
    }

    @Override
    public List<String> getCodeLines() {
      return codeLines;
    }

    @Override
    public final List<TypeProvider> getClassImports() {
      return classImports;
    }

    @Override
    public MethodDeclaration getDeclaration() {
      List<MethodParameter> parameters = parametersTracker.getMethodParameters();
      JavadocObject javadoc = new MethodJavadoc(methodName, returnType, parameters, description);
      return new MethodDeclarationImpl(
          methodName, parameters, returnType, interfaceImports, javadoc);
    }

    @Override
    public boolean isPublic() {
      return this.isPublic;
    }
  }

  /**
   * getter method that returns list of custom elements
   *
   * @since 224
   */
  public static final class Multiple implements PageObjectMethod {

    private final TypeProvider returnType;
    private final List<String> codeLines = new ArrayList<>();
    private final List<TypeProvider> classImports = new ArrayList<>();
    private final List<TypeProvider> interfaceImports = new ArrayList<>();
    private final boolean isPublic;
    private final String methodName;
    private final MethodParametersTracker parametersTracker;
    private final UtamMethodDescription description;

    /**
     * Initializes a new instance of the Multiple class
     *
     * @param isPublic a value indicating whether the method is public
     * @param componentName the name of the component
     * @param locatorParameters the list of parameters of the method
     * @param scopeElement the scope element of the method
     * @param returnType the return type of the method
     * @param description method description in Json
     */
    public Multiple(
        boolean isPublic,
        String componentName,
        List<MethodParameter> locatorParameters,
        ElementContext scopeElement,
        TypeProvider returnType,
        UtamMethodDescription description) {
      this.returnType = wrapAsList(returnType);
      ParameterUtils.setImport(interfaceImports, this.returnType);
      ParameterUtils.setImport(classImports, this.returnType);
      ParameterUtils.setImport(classImports, BASIC_ELEMENT);
      this.methodName = getElementGetterMethodName(componentName, isPublic);
      this.parametersTracker =
          new MethodParametersTracker(String.format("method '%s'", methodName));
      codeLines.add(setupScopeElement(scopeElement, parametersTracker));
      String locationCode = getElementLocationCode(componentName, locatorParameters);
      String statement =
          String.format(
              "return custom(%s, %s).buildList(%s.class)",
              scopeElement.getName(), locationCode, returnType.getSimpleName());
      codeLines.add(statement);
      this.isPublic = isPublic;
      parametersTracker.setMethodParameters(locatorParameters);
      this.description = description;
    }

    @Override
    public MethodDeclaration getDeclaration() {
      List<MethodParameter> parameters = parametersTracker.getMethodParameters();
      JavadocObject javadoc = new MethodJavadoc(methodName, returnType, parameters, description);
      return new MethodDeclarationImpl(
          methodName, parameters, returnType, interfaceImports, javadoc);
    }

    @Override
    public List<TypeProvider> getClassImports() {
      return this.classImports;
    }

    @Override
    public List<String> getCodeLines() {
      return this.codeLines;
    }

    @Override
    public boolean isPublic() {
      return this.isPublic;
    }
  }
}
