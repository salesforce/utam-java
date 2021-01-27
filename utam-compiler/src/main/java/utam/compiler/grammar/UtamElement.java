package utam.compiler.grammar;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import utam.compiler.helpers.ElementContext;
import utam.compiler.helpers.ElementUnitTestHelper;
import utam.compiler.helpers.TranslationContext;
import utam.compiler.helpers.TypeUtilities;
import declarative.representation.*;
import utam.compiler.translator.TranslationTypesConfigJava;
import framework.consumer.UtamError;
import utam.compiler.representation.ContainerMethod;
import utam.compiler.representation.CustomElementMethod;
import utam.compiler.representation.ElementField;
import utam.compiler.representation.ElementMethod;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static utam.compiler.helpers.AnnotationUtils.getFindAnnotation;
import static utam.compiler.helpers.ElementContext.EMPTY_SELECTOR;
import static utam.compiler.helpers.ElementContext.ROOT_SCOPE;
import static utam.compiler.helpers.TypeUtilities.Element.actionable;
import static utam.compiler.translator.TranslationUtilities.setHtmlElementComments;

/**
 * Page Object Element
 *
 * @author elizaveta.ivanova
 * @since 228
 */
public final class UtamElement {

  static final String ERR_ELEMENT_OF_UNKNOWN_TYPE = "element '%s' has unknown type";
  static final String ERR_ELEMENT_FILTER_NEEDS_LIST =
      "element '%s': filter can only be set for list";
  static final String ERR_ELEMENT_MISSING_SELECTOR_PROPERTY =
      "element '%s': missing 'selector' property";
  static final String ERR_ELEMENT_REDUNDANT_PROPERTIES =
      "%s element '%s': only properties { %s } are supported";
  static final String ERR_ELEMENT_NESTED_ELEMENTS = "element '%s' can't have nested elements";
  static final String ERR_ELEMENT_EXTERNAL_NOT_ALLOWED =
      "element '%s': external flag is not supported";
  private static final String[] SUPPORTED_BASIC_ELEMENT_PROPERTIES = {
    "name", "public", "selector", "filter", "nullable", "shadow", "elements"
  };
  private static final String[] SUPPORTED_CONTAINER_ELEMENT_PROPERTIES = {"name", "public"};
  private static final String[] SUPPORTED_CUSTOM_ELEMENT_PROPERTIES = {
    "name", "public", "selector", "filter", "nullable", "external"
  };
  UtamSelector selector;
  UtamShadowElement shadow;
  String type;
  Boolean isPublic; // should be nullable as it's redundant for root
  final String name;
  UtamElement[] elements;
  UtamElementFilter filter;
  Boolean isNullable;
  Boolean isExternal;
  Traversal traversalAbstraction;
  // used in tests
  UtamElement(String name) {
    this(null, name, false, null, null, null, null, null, null);
  }
  // used in tests
  public UtamElement(String name, String type, UtamSelector selector) {
    this(type, name, false, null, null, selector, null, null, null);
  }

  // used in tests
  public UtamElement(String name, UtamSelector selector) {
    this(null, name, false, null, null, selector, null, null, null);
  }

  @JsonCreator
  UtamElement(
      @JsonProperty(value = "type") String type, // optional for actionable
      @JsonProperty(value = "name", required = true) String name,
      @JsonProperty(value = "public") Boolean isPublic,
      @JsonProperty(value = "nullable") Boolean isNullable,
      @JsonProperty(value = "external") Boolean isExternal, // to support compatibility
      @JsonProperty(value = "selector") UtamSelector selector,
      @JsonProperty(value = "filter") UtamElementFilter filter,
      @JsonProperty("shadow") UtamShadowElement shadow,
      @JsonProperty("elements") UtamElement[] elements) {
    this.type = type;
    this.name = name;
    this.isPublic = isPublic;
    this.selector = selector;
    this.shadow = shadow;
    this.elements = elements;
    this.filter = filter;
    this.isNullable = isNullable;
    this.isExternal = isExternal;
  }

  String getSupportedPropertiesErr(Type elementType) {
    final String SUPPORTED;
    if (elementType == Type.BASIC) {
      SUPPORTED = String.join(",", SUPPORTED_BASIC_ELEMENT_PROPERTIES);
    } else if (elementType == Type.CUSTOM) {
      SUPPORTED = String.join(",", SUPPORTED_CUSTOM_ELEMENT_PROPERTIES);
    } else {
      SUPPORTED =
              String.join(",", SUPPORTED_CONTAINER_ELEMENT_PROPERTIES);
    }
    return String.format(
        ERR_ELEMENT_REDUNDANT_PROPERTIES, elementType.name().toLowerCase(), name, SUPPORTED);
  }

  final Traversal getAbstraction() {
    if (traversalAbstraction != null) {
      return traversalAbstraction;
    }
    if ("container".equals(type)) {
      traversalAbstraction = new Container();
    } else if (TypeUtilities.isElementType(type)) {
      traversalAbstraction = new Basic();
    } else if (TranslationTypesConfigJava.isPageObjectType(type)) {
      traversalAbstraction = new Custom();
    } else {
      throw new UtamError(String.format(ERR_ELEMENT_OF_UNKNOWN_TYPE, name));
    }
    return traversalAbstraction;
  }

  final boolean isPublic() {
    return Boolean.TRUE.equals(isPublic);
  }

  final boolean isNullable() {
    return Boolean.TRUE.equals(isNullable);
  }

  final void traverse(
      TranslationContext context, ElementContext scopeElement, boolean isExpandScopeShadowRoot) {
    Traversal element = getAbstraction();
    ElementContext nextScope = element.traverse(context, scopeElement, isExpandScopeShadowRoot)[0];
    if (elements != null) {
      for (UtamElement nextElement : elements) {
        nextElement.traverse(context, nextScope, false);
      }
    }
    if (shadow != null) {
      for (UtamElement nextElement : shadow.elements) {
        nextElement.traverse(context, nextScope, true);
      }
    }
  }

  public void testTraverse(TranslationContext context) {
    traverse(context, ROOT_SCOPE, false);
  }

  public enum Type {
    BASIC,
    CUSTOM,
    CONTAINER
  }

  abstract static class Traversal {

    // traverse and return next scope
    // if next scope is null, second element is self
    // returning both for testing purposes
    abstract ElementContext[] traverse(
        TranslationContext context, ElementContext scopeElement, boolean isExpandScopeShadowRoot);

    ElementContext testRootTraverse(TranslationContext context) {
      return traverse(context, ROOT_SCOPE, false)[0];
    }

    ElementContext testRootTraverseComponentOrContainer(TranslationContext context) {
      return traverse(context, ROOT_SCOPE, false)[1];
    }
  }

  class Custom extends Traversal {

    private Custom() {
      if (selector == null) {
        throw new UtamError(String.format(ERR_ELEMENT_MISSING_SELECTOR_PROPERTY, name));
      }
      if (filter != null && !selector.isReturnAll) {
        throw new UtamError(String.format(ERR_ELEMENT_FILTER_NEEDS_LIST, name));
      }
      if (elements != null || shadow != null) {
        throw new UtamError(getSupportedPropertiesErr(Type.CUSTOM));
      }
      if (isExternal != null && selector.isReturnAll) {
        throw new UtamError(String.format(ERR_ELEMENT_EXTERNAL_NOT_ALLOWED, name));
      }
    }

    @Override
    final ElementContext[] traverse(
        TranslationContext translatorContext,
        ElementContext scopeElement,
        boolean isExpandScopeShadowRoot) {
      boolean isReturnList = selector.isReturnAll && (filter == null || !filter.getFindFirst());
      List<MethodParameter> addedParameters = selector.getParameters(name);
      TypeProvider elementType = translatorContext.getType(type);
      // addedParameters should only include selector parameters!
      CustomElementMethod.Root root =
          new CustomElementMethod.Root(
              selector.getSelector(), isExpandScopeShadowRoot, addedParameters);
      if (filter != null) {
        filter.setElementFilter(Type.CUSTOM, elementType, name);
        addedParameters.addAll(filter.getApplyMethodParameters());
        addedParameters.addAll(filter.getMatcherParameters());
      }
      // set element
      ElementContext component =
          new ElementContext.Custom(
              scopeElement,
              name,
              elementType,
              selector.getSelector(),
              isReturnList,
              addedParameters);
      PageObjectMethod method;
      if (filter != null) {
        method =
            new CustomElementMethod.Filtered(
                isPublic(),
                name,
                root,
                scopeElement,
                elementType,
                isNullable(),
                filter.applyMethod,
                filter.getApplyMethodParameters(),
                filter.getMatcherType(),
                filter.getMatcherParameters(),
                filter.getFindFirst());
      } else if (selector.isReturnAll) {
        method =
            new CustomElementMethod.Multiple(
                isPublic(), name, root, scopeElement, elementType, isNullable());
      } else {
        boolean isExternalElement = Boolean.TRUE.equals(isExternal);
        method =
            new CustomElementMethod.Single(
                isPublic(), name, root, scopeElement, isExternalElement, elementType, isNullable());
      }
      translatorContext.setElement(component);
      translatorContext.setMethod(method);
      component.setElementMethod(method);
      // register usage of scope getter
      String methodName = scopeElement.getElementMethod().getDeclaration().getName();
      translatorContext.setPrivateMethodUsage(methodName);
      translatorContext.setTestableElement(name, new ElementUnitTestHelper(
              selector.getSelector().getValue(),
              scopeElement.getName(),
              isExpandScopeShadowRoot,
              isReturnList));
      return new ElementContext[] {null, component};
    }
  }

  class Basic extends Traversal {

    private Basic() {
      if (selector == null) {
        throw new UtamError(String.format(ERR_ELEMENT_MISSING_SELECTOR_PROPERTY, name));
      }
      if (filter != null && !selector.isReturnAll) {
        throw new UtamError(String.format(ERR_ELEMENT_FILTER_NEEDS_LIST, name));
      }
      if (isExternal != null) {
        throw new UtamError(getSupportedPropertiesErr(Type.BASIC));
      }
      if (selector.isReturnAll && (elements != null || shadow != null)) {
        throw new UtamError(String.format(ERR_ELEMENT_NESTED_ELEMENTS, name));
      }
    }

    @Override
    final ElementContext[] traverse(
        TranslationContext context, ElementContext scopeElement, boolean isExpandScopeShadowRoot) {
      TypeProvider elementType = TypeUtilities.getElementType(type, actionable);
      ElementField field =
          new ElementField(
              name,
              elementType,
              Collections.singletonList(
                  getFindAnnotation(selector.getSelector(), scopeElement, isExpandScopeShadowRoot)),
              setHtmlElementComments(selector.getSelector(), isExpandScopeShadowRoot));
      List<MethodParameter> parameters = new ArrayList<>(selector.getParameters(name));
      if (filter != null) {
        filter.setElementFilter(Type.BASIC, elementType, name);
        parameters.addAll(filter.getApplyMethodParameters());
        parameters.addAll(filter.getMatcherParameters());
      }
      boolean isList = selector.isReturnAll && (filter == null || !filter.getFindFirst());
      ElementContext elementContext =
          new ElementContext.Basic(
              scopeElement, name, elementType, selector.getSelector(), isList, parameters);
      final PageObjectMethod method;
      if (filter != null) {
        // element parameters do not include filter or matcher parameters
        List<MethodParameter> elementParameters = new ArrayList<>(scopeElement.getParameters());
        elementParameters.addAll(selector.getParameters(name));
        method =
            new ElementMethod.Filtered(
                name,
                elementType,
                elementParameters,
                isPublic(),
                filter.applyMethod,
                filter.getApplyMethodParameters(),
                filter.getMatcherType(),
                filter.getMatcherParameters(),
                filter.getFindFirst());
      } else if (isList) {
        method = new ElementMethod.Multiple(elementContext, isPublic());
      } else {
        method = new ElementMethod.Single(elementContext, isPublic());
      }
      context.setClassField(field);
      context.setElement(elementContext);
      context.setMethod(method);
      elementContext.setElementField(field);
      elementContext.setElementMethod(method);
      context.setTestableElement(name, new ElementUnitTestHelper(
              selector.getSelector().getValue(),
              scopeElement.getName(),
              isExpandScopeShadowRoot,
              isList
      ));
      return new ElementContext[] {elementContext};
    }
  }

  class Container extends Traversal {

    private Container() {
      if (selector != null
          || filter != null
          || isNullable != null
          || isExternal != null
          || elements != null
          || shadow != null) {
        throw new UtamError(getSupportedPropertiesErr(Type.CONTAINER));
      }
    }

    @Override
    ElementContext[] traverse(
        TranslationContext context, ElementContext scopeElement, boolean isExpandScopeShadowRoot) {
      TypeProvider elementType = TypeUtilities.CONTAINER_ELEMENT;
      PageClassField field =
          new ElementField(
              name,
              elementType,
              Collections.singletonList(
                  getFindAnnotation(EMPTY_SELECTOR, scopeElement, isExpandScopeShadowRoot)),
              "");
      ElementContext elementContext = new ElementContext.Container(scopeElement, name);
      if (!isPublic()) {
        throw new UtamError(
            String.format(
                "Private container is redundant, please mark element '%s' as public",
                elementContext.getName()));
      }
      PageObjectMethod method = new ContainerMethod(scopeElement, elementContext, isPublic());
      elementContext.setElementField(field);
      elementContext.setElementMethod(method);
      context.setElement(elementContext);
      context.setMethod(method);
      context.setClassField(field);
      return new ElementContext[] {null, elementContext};
    }
  }
}
