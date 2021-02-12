package utam.compiler.representation;

import utam.core.declarative.representation.MethodDeclaration;
import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.PageObjectMethod;
import utam.core.declarative.representation.TypeProvider;
import utam.compiler.helpers.ElementContext;
import utam.compiler.helpers.ParameterUtils;
import utam.compiler.helpers.TypeUtilities;
import utam.core.selenium.element.Selector;

import java.util.ArrayList;
import java.util.List;

import static utam.compiler.helpers.TypeUtilities.LIST_IMPORT;
import static utam.compiler.helpers.TypeUtilities.PAGE_OBJECT;
import static utam.compiler.representation.ComposeMethod.getElementGetterString;
import static utam.compiler.representation.CustomElementMethod.buildSelectorString;
import static utam.compiler.translator.TranslationUtilities.getElementGetterMethodName;

/**
 * method generated for container element
 *
 * @author elizaveta.ivanova
 * @since 230
 */
public abstract class ContainerMethod implements PageObjectMethod {

  public static final TypeProvider RETURNS_LIST = new TypeUtilities.FromString(
      String.format("<T extends %s> List<T>", PAGE_OBJECT.getSimpleName()));
  static final String PAGE_OBJECT_TYPE_PARAMETER_NAME = "pageObjectType";
  static final TypeProvider RETURNS =
      new TypeUtilities.FromString(String.format("<T extends %s> T", PAGE_OBJECT.getSimpleName()));
  static final MethodParameter PAGE_OBJECT_PARAMETER =
      new ParameterUtils.Regular(PAGE_OBJECT_TYPE_PARAMETER_NAME,
          new TypeUtilities.FromString("Class<T>"));
  private static final TypeProvider SELECTOR_IMPORT = new TypeUtilities.FromClass(Selector.class);
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
        .format("this.inContainer(%s, %s)", getElementGetterString(scopeElement), isExpandScope);
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
        String elementName, Selector injectSelector, List<MethodParameter> selectorParameters) {
      super(scopeElement, isExpandScope, elementName, RETURNS_LIST);
      interfaceImports.add(PAGE_OBJECT);
      interfaceImports.add(LIST_IMPORT);
      classImports.addAll(interfaceImports);
      // because method that build selector uses Selector.Type
      classImports.add(SELECTOR_IMPORT);
      methodParameters.addAll(selectorParameters);
      methodParameters.add(PAGE_OBJECT_PARAMETER);
      String selectorValue = buildSelectorString(injectSelector, selectorParameters);
      implCodeLines.add(String.format("%s.loadList(%s, by(%s, Selector.Type.%s))", containerElement,
          PAGE_OBJECT_TYPE_PARAMETER_NAME, selectorValue, injectSelector.getType()));
    }

  }

  public static class WithSelector extends ContainerMethod {

    public WithSelector(ElementContext scopeElement, boolean isExpandScope, String elementName,
        Selector injectSelector, List<MethodParameter> selectorParameters) {
      super(scopeElement, isExpandScope, elementName, RETURNS);
      interfaceImports.add(PAGE_OBJECT);
      classImports.addAll(interfaceImports);
      // because method that build selector uses Selector.Type
      classImports.add(SELECTOR_IMPORT);
      String selectorValue = buildSelectorString(injectSelector, selectorParameters);
      implCodeLines.add(String.format("%s.load(%s, by(%s, Selector.Type.%s))", containerElement,
          PAGE_OBJECT_TYPE_PARAMETER_NAME, selectorValue, injectSelector.getType()));
      methodParameters.addAll(selectorParameters);
      methodParameters.add(PAGE_OBJECT_PARAMETER);
    }
  }
}
