/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.representation;

import static utam.compiler.helpers.TypeUtilities.CONTAINER_LIST_RETURN_TYPE;
import static utam.compiler.helpers.TypeUtilities.CONTAINER_RETURN_TYPE;
import static utam.compiler.helpers.TypeUtilities.LIST_IMPORT;
import static utam.compiler.helpers.TypeUtilities.PAGE_OBJECT;
import static utam.compiler.helpers.TypeUtilities.SELECTOR;
import static utam.compiler.representation.ComposeMethod.getElementLocatorString;
import static utam.compiler.translator.TranslationUtilities.getElementGetterMethodName;

import java.util.ArrayList;
import java.util.List;
import utam.compiler.helpers.LocatorCodeGeneration;
import utam.compiler.helpers.ElementContext;
import utam.compiler.helpers.ParameterUtils;
import utam.compiler.helpers.TypeUtilities;
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
      new ParameterUtils.Regular(PAGE_OBJECT_TYPE_PARAMETER_NAME,
          TypeUtilities.BOUNDED_CLASS);
  final List<TypeProvider> classImports = new ArrayList<>();
  final List<TypeProvider> interfaceImports = new ArrayList<>();
  final List<String> implCodeLines = new ArrayList<>();
  final List<MethodParameter> methodParameters = new ArrayList<>();
  final String containerElement;
  private final TypeProvider returnType;
  private final String methodName;

  ContainerMethod(ElementContext scopeElement, boolean isExpandScope, String elementName,
      TypeProvider returnType) {
    this.returnType = returnType;
    this.methodName = getElementGetterMethodName(elementName, true);
    this.methodParameters.addAll(scopeElement.getParameters());
    this.containerElement = String
        .format("this.inContainer(%s, %s)", getElementLocatorString(scopeElement), isExpandScope);
  }

  @Override public MethodDeclaration getDeclaration() {
    return new MethodDeclarationImpl(methodName, methodParameters, returnType, interfaceImports);
  }

  @Override public List<String> getCodeLines() {
    return implCodeLines;
  }

  @Override public List<TypeProvider> getClassImports() {
    return classImports;
  }

  @Override public boolean isPublic() {
    return true;
  }

  public static class WithSelectorReturnsList extends ContainerMethod {

    public WithSelectorReturnsList(ElementContext scopeElement, boolean isExpandScope,
        String elementName, LocatorCodeGeneration selectorContext) {
      super(scopeElement, isExpandScope, elementName, CONTAINER_LIST_RETURN_TYPE);
      interfaceImports.add(PAGE_OBJECT);
      interfaceImports.add(LIST_IMPORT);
      classImports.addAll(interfaceImports);
      // because method that build selector uses Selector.Type
      classImports.add(SELECTOR);
      methodParameters.addAll(selectorContext.getParameters());
      methodParameters.add(PAGE_OBJECT_PARAMETER);
      String selectorValue = selectorContext.getBuilderString();
      implCodeLines.add(String.format("%s.loadList(%s, %s)", containerElement,
          PAGE_OBJECT_TYPE_PARAMETER_NAME, selectorValue));
    }

  }

  public static class WithSelector extends ContainerMethod {

    public WithSelector(ElementContext scopeElement, boolean isExpandScope, String elementName,
        LocatorCodeGeneration selectorContext) {
      super(scopeElement, isExpandScope, elementName, CONTAINER_RETURN_TYPE);
      interfaceImports.add(PAGE_OBJECT);
      classImports.addAll(interfaceImports);
      // because method that build selector uses Selector.Type
      classImports.add(SELECTOR);
      String selectorValue = selectorContext.getBuilderString();
      implCodeLines.add(String.format("%s.load(%s, %s)", containerElement,
          PAGE_OBJECT_TYPE_PARAMETER_NAME, selectorValue));
      methodParameters.addAll(selectorContext.getParameters());
      methodParameters.add(PAGE_OBJECT_PARAMETER);
    }
  }
}
