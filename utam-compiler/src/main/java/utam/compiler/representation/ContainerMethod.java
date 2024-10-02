/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.representation;

import static utam.compiler.helpers.TypeUtilities.BASIC_ELEMENT;
import static utam.compiler.helpers.TypeUtilities.PAGE_OBJECT_RETURN;
import static utam.compiler.helpers.TypeUtilities.PAGE_OBJECT_RETURN_LIST;
import static utam.compiler.helpers.TypeUtilities.SELECTOR;
import static utam.compiler.helpers.TypeUtilities.T_PAGE_OBJECT_TYPE_PARAMETER;
import static utam.compiler.representation.ElementMethod.setupScopeElement;
import static utam.compiler.translator.TranslationUtilities.getElementGetterMethodName;

import java.util.ArrayList;
import java.util.List;
import utam.compiler.grammar.UtamMethodDescription;
import utam.compiler.helpers.ElementContext;
import utam.compiler.helpers.LocatorCodeGeneration;
import utam.compiler.helpers.ParameterUtils;
import utam.compiler.helpers.ParameterUtils.Regular;
import utam.compiler.representation.JavadocObject.MethodJavadoc;
import utam.core.declarative.representation.MethodDeclaration;
import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.PageObjectMethod;
import utam.core.declarative.representation.TypeProvider;

/**
 * method generated for container element
 *
 * @author elizaveta.ivanova
 * @since 230
 */
public abstract class ContainerMethod implements PageObjectMethod {

  private static final String PAGE_OBJECT_TYPE_PARAMETER_NAME = "pageObjectType";
  public static final MethodParameter PAGE_OBJECT_PARAMETER =
      new Regular(PAGE_OBJECT_TYPE_PARAMETER_NAME, T_PAGE_OBJECT_TYPE_PARAMETER);
  final String methodName;
  final MethodParametersTracker parametersTracker;
  final String locatorVariableName;
  final UtamMethodDescription description;
  private final List<String> codeLines = new ArrayList<>();
  final List<TypeProvider> classImports = new ArrayList<>();
  private final boolean isPublic;

  ContainerMethod(
      ElementContext scopeElement,
      String elementName,
      String selectorBuilderString,
      UtamMethodDescription methodDescription,
      ElementOptions options) {
    this.locatorVariableName = String.format("%sLocator", elementName);
    this.methodName = getElementGetterMethodName(elementName, options.isPublic);
    this.parametersTracker =
        new MethodParametersTracker(String.format("element '%s'", elementName));
    codeLines.add(setupScopeElement(scopeElement, parametersTracker));
    codeLines.add(
        String.format(
            "%s %s = %s",
            SELECTOR.getSimpleName(), this.locatorVariableName, selectorBuilderString));
    String scopeVariableName = scopeElement.getName();
    codeLines.add(buildInvocationString(options, scopeVariableName));
    this.isPublic = options.isPublic;
    this.description = methodDescription;
  }

  private String buildInvocationString(ElementOptions options, String scopeVariableName) {
    StringBuilder code = new StringBuilder();
    code.append(String.format("return this.container(%s)", scopeVariableName));
    if (options.isExpandShadowRoot) {
      code.append(".expandShadowRoot(true)");
    }
    if (options.isNullable) {
      code.append(".nullable(true)");
    }
    code.append(String.format(".build().%s", getContainerMethodInvocationString()));
    return code.toString();
  }

  abstract String getContainerMethodInvocationString();

  @Override
  public boolean isPublic() {
    return isPublic;
  }

  @Override
  public List<String> getCodeLines() {
    return codeLines;
  }

  @Override
  public List<TypeProvider> getClassImports() {
    return classImports;
  }

  /** Represents a container method with a selector that returns a list */
  public static class WithSelectorReturnsList extends ContainerMethod {

    /**
     * Initializes a new instance of the WithSelectorReturnsList class
     *
     * @param scopeElement the scope element
     * @param elementName the name of the element
     * @param selectorContext the context for the selector
     * @param methodDescription the method description
     * @param options options of container element
     */
    public WithSelectorReturnsList(
        ElementContext scopeElement,
        String elementName,
        LocatorCodeGeneration selectorContext,
        UtamMethodDescription methodDescription,
        ElementOptions options) {
      super(
          scopeElement,
          elementName,
          selectorContext.getBuilderString(),
          methodDescription,
          options);
      parametersTracker.setMethodParameters(selectorContext.getParameters());
      parametersTracker.setMethodParameter(PAGE_OBJECT_PARAMETER);
      ParameterUtils.setImport(classImports, BASIC_ELEMENT);
      // because of method that builds selector
      ParameterUtils.setImport(classImports, SELECTOR);
      ParameterUtils.setImports(classImports, getDeclaration().getImports());
    }

    @Override
    public MethodDeclaration getDeclaration() {
      List<MethodParameter> parameters = parametersTracker.getMethodParameters();
      JavadocObject javadoc =
          new MethodJavadoc(methodName, PAGE_OBJECT_RETURN_LIST, parameters, description);
      return new MethodDeclarationImpl(methodName, parameters, PAGE_OBJECT_RETURN_LIST, javadoc);
    }

    @Override
    String getContainerMethodInvocationString() {

      return String.format(
          "loadList(%s, %s)", PAGE_OBJECT_TYPE_PARAMETER_NAME, locatorVariableName);
    }
  }

  /** Represents a container method with a selector */
  public static class WithSelector extends ContainerMethod {

    /**
     * Initializes a new instance of the WithSelector class
     *
     * @param scopeElement the scope element
     * @param elementName the name of the element
     * @param selectorContext the context for the selector
     * @param methodDescription the method description
     * @param options options of container element
     */
    public WithSelector(
        ElementContext scopeElement,
        String elementName,
        LocatorCodeGeneration selectorContext,
        UtamMethodDescription methodDescription,
        ElementOptions options) {
      super(
          scopeElement,
          elementName,
          selectorContext.getBuilderString(),
          methodDescription,
          options);
      parametersTracker.setMethodParameters(selectorContext.getParameters());
      parametersTracker.setMethodParameter(PAGE_OBJECT_PARAMETER);
      ParameterUtils.setImport(classImports, BASIC_ELEMENT);
      // because of method that builds selector
      ParameterUtils.setImport(classImports, SELECTOR);
      ParameterUtils.setImports(classImports, getDeclaration().getImports());
    }

    @Override
    public MethodDeclaration getDeclaration() {
      List<MethodParameter> parameters = parametersTracker.getMethodParameters();
      JavadocObject javadoc =
          new MethodJavadoc(methodName, PAGE_OBJECT_RETURN, parameters, description);
      return new MethodDeclarationImpl(methodName, parameters, PAGE_OBJECT_RETURN, javadoc);
    }

    @Override
    String getContainerMethodInvocationString() {
      return String.format("load(%s, %s)", PAGE_OBJECT_TYPE_PARAMETER_NAME, locatorVariableName);
    }
  }

  public static class ElementOptions {

    final boolean isPublic;
    final boolean isNullable;
    final boolean isExpandShadowRoot;

    public ElementOptions(Boolean isPublic, Boolean isNullable, Boolean isExpandShadowRoot) {
      this.isPublic = Boolean.TRUE == isPublic;
      this.isNullable = Boolean.TRUE == isNullable;
      this.isExpandShadowRoot = Boolean.TRUE == isExpandShadowRoot;
    }
  }
}
