/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.representation;

import static utam.compiler.helpers.TypeUtilities.PAGE_OBJECT;
import static utam.compiler.helpers.TypeUtilities.SELECTOR;
import static utam.compiler.representation.ComposeMethod.getElementLocatorString;
import static utam.compiler.translator.TranslationUtilities.getElementGetterMethodName;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import utam.compiler.helpers.ElementContext;
import utam.compiler.helpers.LocatorCodeGeneration;
import utam.compiler.helpers.ParameterUtils.Regular;
import utam.compiler.helpers.TypeUtilities.BoundedClass;
import utam.compiler.helpers.TypeUtilities.ListOf;
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
      new Regular(PAGE_OBJECT_TYPE_PARAMETER_NAME,
          new BoundedClass(PAGE_OBJECT, "T"));
  final List<MethodParameter> methodParameters = new ArrayList<>();
  final String codePrefix;
  final String methodName;
  private final boolean isPublic;


  ContainerMethod(ElementContext scopeElement, boolean isExpandScope, String elementName,
      boolean isPublic) {
    this.methodName = getElementGetterMethodName(elementName, isPublic);
    this.methodParameters.addAll(scopeElement.getParameters());
    this.codePrefix = String
        .format("this.inContainer(%s, %s)", getElementLocatorString(scopeElement), isExpandScope);
    this.isPublic = isPublic;
  }

  @Override
  public boolean isPublic() {
    return isPublic;
  }

  public static class WithSelectorReturnsList extends ContainerMethod {

    private final String selectorValue;

    public WithSelectorReturnsList(ElementContext scopeElement, boolean isExpandScope,
        String elementName, LocatorCodeGeneration selectorContext, boolean isPublic) {
      super(scopeElement, isExpandScope, elementName, isPublic);
      methodParameters.addAll(selectorContext.getParameters());
      methodParameters.add(PAGE_OBJECT_PARAMETER);
      selectorValue = selectorContext.getBuilderString();
    }

    @Override
    public List<TypeProvider> getClassImports() {
      List<TypeProvider> imports = new ArrayList<>(getDeclaration().getImports());
      // because of method that builds selector
      imports.add(SELECTOR);
      return imports;
    }

    @Override
    public MethodDeclaration getDeclaration() {
      return new MethodDeclarationImpl(methodName, methodParameters, new ListOf(PAGE_OBJECT)) {
        @Override
        String getReturnTypeStr() {
          return String.format("<T extends %s> List<T>", PAGE_OBJECT.getSimpleName());
        }
      };
    }

    @Override
    public List<String> getCodeLines() {
      String codeLine = String
          .format(".loadList(%s, %s)", PAGE_OBJECT_TYPE_PARAMETER_NAME, selectorValue);
      return Collections.singletonList(codePrefix + codeLine);
    }
  }

  public static class WithSelector extends ContainerMethod {

    private final String selectorValue;

    public WithSelector(ElementContext scopeElement, boolean isExpandScope, String elementName,
        LocatorCodeGeneration selectorContext, boolean isPublic) {
      super(scopeElement, isExpandScope, elementName, isPublic);
      selectorValue = selectorContext.getBuilderString();
      methodParameters.addAll(selectorContext.getParameters());
      methodParameters.add(PAGE_OBJECT_PARAMETER);
    }

    @Override
    public List<TypeProvider> getClassImports() {
      List<TypeProvider> imports = new ArrayList<>(getDeclaration().getImports());
      // because of method that builds selector
      imports.add(SELECTOR);
      return imports;
    }

    @Override
    public MethodDeclaration getDeclaration() {
      return new MethodDeclarationImpl(methodName, methodParameters, PAGE_OBJECT) {
        @Override
        String getReturnTypeStr() {
          return String.format("<T extends %s> T", PAGE_OBJECT.getSimpleName());
        }
      };
    }

    @Override
    public List<String> getCodeLines() {
      String codeLine = String
          .format(".load(%s, %s)", PAGE_OBJECT_TYPE_PARAMETER_NAME, selectorValue);
      return Collections.singletonList(codePrefix + codeLine);
    }
  }
}
