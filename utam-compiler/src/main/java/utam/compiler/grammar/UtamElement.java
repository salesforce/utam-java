/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import static utam.compiler.helpers.AnnotationUtils.getFindAnnotation;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import utam.compiler.helpers.ElementContext;
import utam.compiler.helpers.ElementUnitTestHelper;
import utam.compiler.helpers.TranslationContext;
import utam.compiler.helpers.TypeUtilities;
import utam.compiler.representation.ContainerMethod;
import utam.compiler.representation.CustomElementMethod;
import utam.compiler.representation.ElementField;
import utam.compiler.representation.ElementMethod;
import utam.compiler.translator.TranslationTypesConfigJava;
import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.PageObjectMethod;
import utam.core.declarative.representation.TypeProvider;
import utam.core.framework.consumer.UtamError;

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
  private static final String ERR_ELEMENT_REDUNDANT_PROPERTIES =
      "%s element '%s': only properties { %s } are supported";
  static final String ERR_ELEMENT_NESTED_ELEMENTS = "element '%s' can't have nested elements";
  static final String ERR_ELEMENT_EXTERNAL_NOT_ALLOWED =
      "element '%s': external flag is not supported";
  static final String ERR_CONTAINER_SHOULD_BE_PUBLIC =
      "element '%s': private container is redundant, please mark element public";
  private static final String[] SUPPORTED_BASIC_ELEMENT_PROPERTIES = {
    "name", "public", "selector", "filter", "nullable", "shadow", "elements"
  };
  private static final String[] SUPPORTED_CONTAINER_ELEMENT_PROPERTIES = {"name", "public"};
  private static final String[] SUPPORTED_CUSTOM_ELEMENT_PROPERTIES = {
    "name", "public", "selector", "filter", "nullable", "external"
  };
  final String name;
  UtamSelector selector;
  UtamShadowElement shadow;
  String[] type;
  Boolean isPublic; // should be nullable as it's redundant for root
  UtamElement[] elements;
  UtamElementFilter filter;
  Boolean isNullable;
  Boolean isExternal;
  private Traversal traversalAbstraction;

  // used in tests
  UtamElement(String name) {
    this(null, name, false, null, null, null, null, null, null);
  }

  // used in tests
  public UtamElement(String name, String[] type, UtamSelector selector) {
    this(type, name, false, null, null, selector, null, null, null);
  }

  // used in tests
  UtamElement(String name, String type, UtamSelector selector, Boolean isNullable) {
    this(type == null ? null : new String[] { type }, name, false, isNullable, null, selector, null, null, null);
  }

  // used in tests
  public UtamElement(String name, UtamSelector selector) {
    this(null, name, false, null, null, selector, null, null, null);
  }

  @JsonCreator
  UtamElement(
      @JsonProperty(value = "type") String[] type, // optional for actionable
      @JsonProperty(value = "name", required = true) String name,
      @JsonProperty(value = "public") Boolean isPublic,
      @JsonProperty(value = "nullable") Boolean isNullable,
      @JsonProperty(value = "external") Boolean isExternal, // to support compatibility
      @JsonProperty(value = "selector") UtamSelector selector,
      @JsonProperty(value = "filter") UtamElementFilter filter,
      @JsonProperty("shadow") UtamShadowElement shadow,
      @JsonProperty("elements") UtamElement[] elements) {
    this.type = type == null ? new String[] {} : type;
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
      SUPPORTED = String.join(",", SUPPORTED_CONTAINER_ELEMENT_PROPERTIES);
    }
    return String.format(
        ERR_ELEMENT_REDUNDANT_PROPERTIES, elementType.name().toLowerCase(), name, SUPPORTED);
  }

  final Traversal getAbstraction() {
    if (traversalAbstraction != null) {
      return traversalAbstraction;
    }
    Type elementType = getElementType();
    if (elementType == Type.CONTAINER) {
      traversalAbstraction = new Container();
    } else if (elementType == Type.BASIC) {
      traversalAbstraction = new Basic();
    } else if (elementType == Type.CUSTOM) {
      traversalAbstraction = new Custom();
    } else {
      throw new UtamError(String.format(ERR_ELEMENT_OF_UNKNOWN_TYPE, name));
    }
    return traversalAbstraction;
  }

  Type getElementType() {
    if (type.length == 1 && "container".equals(type[0])) {
      return Type.CONTAINER;
    } else if (type.length == 1 && TranslationTypesConfigJava.isPageObjectType(type[0])) {
      return Type.CUSTOM;
    } else if (TypeUtilities.Element.isBasicType(type)) {
      return Type.BASIC;
    }
    return Type.UNKNOWN;
  }

  private boolean isPublic() {
    return Boolean.TRUE.equals(isPublic);
  }

  private boolean isNullable() {
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
    traverse(context, null, false);
  }

  public enum Type {
    UNKNOWN,
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
      return traverse(context, null, false)[0];
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
      UtamSelector.Context selectorContext = selector.getContext();
      List<MethodParameter> addedParameters = new ArrayList<>(selectorContext.getParameters());
      TypeProvider elementType = translatorContext.getType(type[0]);
      // addedParameters should only include selector parameters!
      CustomElementMethod.Root root = new CustomElementMethod.Root(selectorContext);
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
              selectorContext.getLocator(),
              isReturnList,
              addedParameters,
              isNullable());
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
                isExpandScopeShadowRoot,
                filter.applyMethod,
                filter.getApplyMethodParameters(),
                filter.getMatcherType(),
                filter.getMatcherParameters(),
                filter.getFindFirst());
      } else if (selector.isReturnAll) {
        method =
            new CustomElementMethod.Multiple(
                isPublic(), name, root, scopeElement, elementType, isNullable(), isExpandScopeShadowRoot);
      } else {
        boolean isExternalElement = Boolean.TRUE.equals(isExternal);
        method =
            new CustomElementMethod.Single(
                isPublic(), name, root, scopeElement, isExternalElement, elementType, isNullable(), isExpandScopeShadowRoot);
      }
      translatorContext.setElement(component);
      translatorContext.setMethod(method);
      component.setElementMethod(method);
      translatorContext.setTestableElement(
          name,
          new ElementUnitTestHelper(
              selectorContext.getLocator().getStringValue(),
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
      TypeProvider elementType = TypeUtilities.Element.asBasicType(name, type);
      UtamSelector.Context selectorContext = selector.getContext();
      List<MethodParameter> addedParameters = new ArrayList<>(selectorContext.getParameters());
      ElementField field =
          new ElementField(
              name, getFindAnnotation(selectorContext.getLocator(), scopeElement,
              isExpandScopeShadowRoot, isNullable()));
      if (filter != null) {
        filter.setElementFilter(Type.BASIC, elementType, name);
        addedParameters.addAll(filter.getApplyMethodParameters());
        addedParameters.addAll(filter.getMatcherParameters());
      }
      boolean isList = selector.isReturnAll && (filter == null || !filter.getFindFirst());
      ElementContext elementContext =
          new ElementContext.Basic(
              scopeElement, name, elementType, selectorContext.getLocator(), isList,
              addedParameters, isNullable());
      final PageObjectMethod method;
      if (filter != null) {
        // element parameters do not include filter or matcher parameters
        List<MethodParameter> elementParameters = new ArrayList<>(scopeElement == null? Collections.EMPTY_LIST: scopeElement.getParameters());
        elementParameters.addAll(selectorContext.getParameters());
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
      elementContext.setElementMethod(method);
      context.setTestableElement(name, new ElementUnitTestHelper(
              selectorContext.getLocator().getStringValue(),
              scopeElement == null? null : scopeElement.getName(),
              isExpandScopeShadowRoot,
              isList
      ));
      return new ElementContext[] {elementContext};
    }
  }

  class Container extends Traversal {

    static final String DEFAULT_CONTAINER_SELECTOR_CSS = ":scope > *:first-child";

    private Container() {
      if (filter != null
          || isNullable != null
          || isExternal != null
          || elements != null
          || shadow != null) {
        throw new UtamError(getSupportedPropertiesErr(Type.CONTAINER));
      }
      if (!isPublic()) {
        throw new UtamError(String.format(ERR_CONTAINER_SHOULD_BE_PUBLIC, name));
      }
      if (selector == null) {
        selector = new UtamSelector(
            DEFAULT_CONTAINER_SELECTOR_CSS,
            null,
            null,
            null,
            false,
            new UtamArgument[] {});
      }
    }

    @Override
    ElementContext[] traverse(
        TranslationContext context, ElementContext scopeElement, boolean isExpandScopeShadowRoot) {
      UtamSelector.Context selectorContext = selector.getContext();
      ElementContext elementContext = new ElementContext.Container(scopeElement, name);
      PageObjectMethod method;
      if (selector.isReturnAll) {
        method = new ContainerMethod.WithSelectorReturnsList(
            scopeElement, isExpandScopeShadowRoot, name, selectorContext);
      } else {
        method = new ContainerMethod.WithSelector(
            scopeElement, isExpandScopeShadowRoot, name, selectorContext);
      }
      elementContext.setElementMethod(method);
      context.setElement(elementContext);
      context.setMethod(method);
      return new ElementContext[]{null, elementContext};
    }
  }
}
