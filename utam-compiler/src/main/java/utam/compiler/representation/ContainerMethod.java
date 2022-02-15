/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.representation;

import static utam.compiler.helpers.TypeUtilities.BASIC_ELEMENT;
import static utam.compiler.helpers.TypeUtilities.BOUNDED_PAGE_OBJECT_PARAMETER;
import static utam.compiler.helpers.TypeUtilities.PAGE_OBJECT;
import static utam.compiler.helpers.TypeUtilities.SELECTOR;
import static utam.compiler.helpers.TypeUtilities.wrapAsList;
import static utam.compiler.representation.ElementMethod.getScopeElementCode;
import static utam.compiler.translator.TranslationUtilities.getElementGetterMethodName;

import java.util.ArrayList;
import java.util.List;
import utam.compiler.grammar.UtamMethodDescription;
import utam.compiler.helpers.ElementContext;
import utam.compiler.helpers.LocatorCodeGeneration;
import utam.compiler.helpers.ParameterUtils;
import utam.compiler.helpers.ParameterUtils.Regular;
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
  static final MethodParameter PAGE_OBJECT_PARAMETER =
      new Regular(PAGE_OBJECT_TYPE_PARAMETER_NAME, BOUNDED_PAGE_OBJECT_PARAMETER);
  private final List<String> codeLines = new ArrayList<>();
  final String methodName;
  private final boolean isPublic;
  final MethodParametersTracker parametersTracker;
  final String locatorVariableName;
  final UtamMethodDescription methodDescription;

  ContainerMethod(
      ElementContext scopeElement,
      boolean isExpandScope,
      String elementName,
      boolean isPublic,
      String selectorBuilderString,
      UtamMethodDescription methodDescription) {
    this.locatorVariableName = String.format("%sLocator", elementName);
    this.methodName = getElementGetterMethodName(elementName, isPublic);
    this.parametersTracker = new MethodParametersTracker(
        String.format("element '%s'", elementName));
    if (scopeElement != null) {
      parametersTracker.setMethodParameters(scopeElement.getParameters());
      String scopeElementLine = getScopeElementCode(scopeElement);
      codeLines.add(scopeElementLine);
    }
    codeLines.add(String.format("%s %s = %s", SELECTOR.getSimpleName(), this.locatorVariableName, selectorBuilderString));
    String scopeVariableName = scopeElement.getName();
    codeLines.add(String.format("return this.container(%s, %s).%s",
        scopeVariableName, isExpandScope, getContainerMethodInvocationString()));
    this.isPublic = isPublic;
    this.methodDescription = methodDescription;
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

  /**
   * Represents a container method with a selector that returns a list
   */
  public static class WithSelectorReturnsList extends ContainerMethod {

    private final List<TypeProvider> classImports = new ArrayList<>();


    /**
     * Initializes a new instance of the WithSelectorReturnsList class
     *
     * @param scopeElement    the scope element
     * @param isExpandScope   a value indicating whether the method introspects into shadow roots
     * @param elementName     the name of the element
     * @param selectorContext the context for the selector
     * @param isPublic        a value indicating whether the method is public
     */
    public WithSelectorReturnsList(ElementContext scopeElement, boolean isExpandScope,
        String elementName, LocatorCodeGeneration selectorContext, boolean isPublic, UtamMethodDescription methodDescription) {
      super(scopeElement, isExpandScope, elementName, isPublic, selectorContext.getBuilderString(), methodDescription);
      parametersTracker.setMethodParameters(selectorContext.getParameters());
      parametersTracker.setMethodParameter(PAGE_OBJECT_PARAMETER);
      ParameterUtils.setImport(classImports, BASIC_ELEMENT);
      // because of method that builds selector
      ParameterUtils.setImport(classImports, SELECTOR);
      ParameterUtils.setImports(classImports, getDeclaration().getImports());
    }

    @Override
    public List<TypeProvider> getClassImports() {
      return classImports;
    }

    @Override
    public MethodDeclaration getDeclaration() {
      return new MethodDeclarationImpl(methodName, parametersTracker.getMethodParameters(), wrapAsList(PAGE_OBJECT), methodDescription) {
        @Override
        String getReturnTypeStr() {
          return String.format("<T extends %s> List<T>", PAGE_OBJECT.getSimpleName());
        }
      };
    }

    @Override
    String getContainerMethodInvocationString() {
      return String
          .format("loadList(%s, %s)", PAGE_OBJECT_TYPE_PARAMETER_NAME, locatorVariableName);
    }
  }

  /**
   * Represents a container method with a selector
   */
  public static class WithSelector extends ContainerMethod {

    private final List<TypeProvider> classImports = new ArrayList<>();

    /**
     * Initializes a new instance of the WithSelector class
     *
     * @param scopeElement    the scope element
     * @param isExpandScope   a value indicating whether the method introspects into shadow roots
     * @param elementName     the name of the element
     * @param selectorContext the context for the selector
     * @param isPublic        a value indicating whether the method is public
     */
    public WithSelector(ElementContext scopeElement,
        boolean isExpandScope,
        String elementName,
        LocatorCodeGeneration selectorContext,
        boolean isPublic,
        UtamMethodDescription methodDescription) {
      super(scopeElement, isExpandScope, elementName, isPublic, selectorContext.getBuilderString(), methodDescription);
      parametersTracker.setMethodParameters(selectorContext.getParameters());
      parametersTracker.setMethodParameter(PAGE_OBJECT_PARAMETER);
      ParameterUtils.setImport(classImports, BASIC_ELEMENT);
      // because of method that builds selector
      ParameterUtils.setImport(classImports, SELECTOR);
      ParameterUtils.setImports(classImports, getDeclaration().getImports());
    }

    @Override
    public List<TypeProvider> getClassImports() {
      return classImports;
    }

    @Override
    public MethodDeclaration getDeclaration() {
      return new MethodDeclarationImpl(methodName, parametersTracker.getMethodParameters(), PAGE_OBJECT, methodDescription) {
        @Override
        String getReturnTypeStr() {
          return String.format("<T extends %s> T", PAGE_OBJECT.getSimpleName());
        }
      };
    }

    @Override
    String getContainerMethodInvocationString() {
      return String.format("load(%s, %s)", PAGE_OBJECT_TYPE_PARAMETER_NAME, locatorVariableName);
    }
  }
}
