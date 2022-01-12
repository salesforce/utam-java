/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import static utam.compiler.helpers.AnnotationUtils.getFindAnnotation;
import static utam.compiler.types.BasicElementInterface.processBasicTypeNode;
import static utam.compiler.helpers.TypeUtilities.BASIC_ELEMENT_IMPL_CLASS;
import static utam.compiler.helpers.TypeUtilities.CONTAINER_ELEMENT_TYPE_NAME;
import static utam.compiler.helpers.TypeUtilities.FRAME_ELEMENT_TYPE_NAME;
import static utam.compiler.types.BasicElementUnionType.asBasicOrUnionType;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.JsonNode;
import java.util.AbstractMap.SimpleEntry;
import java.util.Map.Entry;
import java.util.function.Supplier;
import utam.compiler.helpers.ElementContext;
import utam.compiler.helpers.ElementUnitTestHelper;
import utam.compiler.helpers.LocatorCodeGeneration;
import utam.compiler.helpers.TranslationContext;
import utam.compiler.representation.ContainerMethod;
import utam.compiler.representation.CustomElementMethod;
import utam.compiler.representation.ElementField;
import utam.compiler.representation.ElementMethod;
import utam.compiler.representation.FrameMethod;
import utam.compiler.representation.MethodParametersTracker;
import utam.compiler.translator.TranslationTypesConfigJava;
import utam.compiler.types.BasicElementUnionTypeImpl;
import utam.core.declarative.representation.PageObjectMethod;
import utam.core.declarative.representation.TypeProvider;
import utam.core.declarative.representation.UnionType;
import utam.core.framework.consumer.UtamError;

/**
 * Page Object Element
 *
 * @author elizaveta.ivanova
 * @since 228
 */
public final class UtamElement {

  /**
   * The default CSS selector for a container
   */
  public static final String DEFAULT_CONTAINER_SELECTOR_CSS = ":scope > *:first-child";
  static final String ERR_ELEMENT_FILTER_NEEDS_LIST =
      "element '%s': filter can only be set for list";
  static final String ERR_ELEMENT_MISSING_SELECTOR_PROPERTY =
      "element '%s': missing 'selector' property";
  static final String ERR_ELEMENT_NESTED_ELEMENTS = "element '%s' can't have nested elements";
  static final String ERR_FRAME_LIST_SELECTOR_NOT_ALLOWED =
      "element '%s': frame selector cannot return all";

  private final String name;
  UtamSelector selector;
  UtamShadowElement shadow;
  String[] type;
  Boolean isPublic; // should be nullable as it's redundant for root
  UtamElement[] elements;
  UtamElementFilter filter;
  private final Boolean isNullable;
  private final Supplier<Traversal> traversalAbstraction;

  @JsonCreator
  UtamElement(
      @JsonProperty(value = "type", defaultValue = "[]") JsonNode type,
      @JsonProperty(value = "name", required = true) String name,
      @JsonProperty(value = "public") Boolean isPublic,
      @JsonProperty(value = "nullable") Boolean isNullable,
      @JsonProperty(value = "selector") UtamSelector selector,
      @JsonProperty(value = "filter") UtamElementFilter filter,
      @JsonProperty("shadow") UtamShadowElement shadow,
      @JsonProperty("elements") UtamElement[] elements) {
    this.name = name;
    this.isPublic = isPublic;
    this.selector = selector;
    this.shadow = shadow;
    this.elements = elements;
    this.filter = filter;
    this.isNullable = isNullable;
    Entry<Supplier<Traversal>, String[]> elementType = processTypeNode(type);
    this.type = elementType.getValue();
    this.traversalAbstraction = elementType.getKey();
  }

  private Entry<Supplier<Traversal>, String[]> processTypeNode(JsonNode typeNode) {
    if (typeNode != null && typeNode.isTextual()) {
      String value = typeNode.textValue();
      if (CONTAINER_ELEMENT_TYPE_NAME.equals(value)) {
        return new SimpleEntry<>(Container::new, new String[] {value});
      }
      if (FRAME_ELEMENT_TYPE_NAME.equals(value)) {
        return new SimpleEntry<>(Frame::new, new String[]{value});
      }
      if (TranslationTypesConfigJava.isPageObjectType(value)) {
        return new SimpleEntry<>(Custom::new, new String[]{value});
      }
    }
    String[] type = processBasicTypeNode(typeNode, name, true);
    return new SimpleEntry<>(Basic::new, type);
  }

  final Traversal getAbstraction() {
    return traversalAbstraction.get();
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

  /**
   * The type of element
   */
  public enum Type {
    /**
     * A basic element
     */
    BASIC(String.join(", ",
        "name", "public", "selector", "type", "filter", "nullable", "shadow", "elements")),

    /**
     * A custom element
     */
    CUSTOM(String.join(", ",
        "name", "public", "selector", "type", "filter", "nullable")),

    /**
     * A container element
     */
    CONTAINER(String.join(", ", "name", "public", "selector", "type")),

    /**
     * A frame element
     */
    FRAME(String.join(", ",
        "name", "public", "selector", "type"));

    private final String supportedProperties;

    Type(String supportedProperties) {
      this.supportedProperties = supportedProperties;
    }

    String getSupportedPropertiesErr(String elementName) {
      return String
          .format("%s element '%s': only properties { %s } are supported", name().toLowerCase(),
              elementName, supportedProperties);
    }
  }

  abstract static class Traversal {

    // traverse and return next scope
    // if next scope is null, second element is self
    // returning both for testing purposes
    abstract ElementContext[] traverse(
        TranslationContext context, ElementContext scopeElement, boolean isExpandScopeShadowRoot);
  }

  class Custom extends Traversal {

    private Custom() {
      if (selector == null) {
        throw new UtamError(String.format(ERR_ELEMENT_MISSING_SELECTOR_PROPERTY, name));
      }
      if (filter != null && !selector.isReturnAll()) {
        throw new UtamError(String.format(ERR_ELEMENT_FILTER_NEEDS_LIST, name));
      }
      if (elements != null || shadow != null) {
        throw new UtamError(Type.CUSTOM.getSupportedPropertiesErr(name));
      }
    }

    @Override
    final ElementContext[] traverse(
        TranslationContext translatorContext,
        ElementContext scopeElement,
        boolean isExpandScopeShadowRoot) {
      boolean isReturnList = selector.isReturnAll() && (filter == null || !filter.getFindFirst());
      LocatorCodeGeneration selectorContext = selector.getCodeGenerationHelper(translatorContext);
      MethodParametersTracker parameters = new MethodParametersTracker(String.format("element '%s' getter", name));
      parameters.setMethodParameters(selectorContext.getParameters());
      TypeProvider elementType = translatorContext.getType(type[0]);
      if (filter != null) {
        filter.setElementFilter(translatorContext, Type.CUSTOM, elementType, name);
        parameters.setMethodParameters(filter.getApplyMethodParameters());
        parameters.setMethodParameters(filter.getMatcherParameters());
      }
      // set element
      ElementContext component = isReturnList ? new ElementContext.CustomReturnsAll(
          scopeElement,
          name,
          elementType,
          selectorContext.getLocator(),
          parameters.getMethodParameters(),
          isNullable()) :
          new ElementContext.Custom(
              scopeElement,
              name,
              elementType,
              selectorContext.getLocator(),
              parameters.getMethodParameters(),
              isNullable());
      PageObjectMethod method;
      if (filter != null) {
        method =
            new CustomElementMethod.Filtered(
                isPublic(),
                name,
                selectorContext.getParameters(),
                scopeElement,
                elementType,
                filter.applyMethod,
                filter.getApplyMethodParameters(),
                filter.getMatcherType(),
                filter.getMatcherParameters(),
                filter.getFindFirst());
      } else if (selector.isReturnAll()) {
        method =
            new CustomElementMethod.Multiple(
                isPublic(),
                name,
                selectorContext.getParameters(),
                scopeElement,
                elementType);
      } else {
        method =
            new CustomElementMethod.Single(
                isPublic(),
                name,
                selectorContext.getParameters(),
                scopeElement,
                elementType);
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
      ElementField field =
          new ElementField(
              name, getFindAnnotation(selectorContext.getLocator(),
              isExpandScopeShadowRoot, isNullable()));
      translatorContext.setClassField(field);
      // scope element method is invoked
      component.getScopeElement().setElementMethodUsage(translatorContext);
      return new ElementContext[] {null, component};
    }
  }

  class Basic extends Traversal {

    private Basic() {
      if (selector == null) {
        throw new UtamError(String.format(ERR_ELEMENT_MISSING_SELECTOR_PROPERTY, name));
      }
      if (filter != null && !selector.isReturnAll()) {
        throw new UtamError(String.format(ERR_ELEMENT_FILTER_NEEDS_LIST, name));
      }
      if (selector.isReturnAll() && (elements != null || shadow != null)) {
        throw new UtamError(String.format(ERR_ELEMENT_NESTED_ELEMENTS, name));
      }
    }

    @Override
    final ElementContext[] traverse(
        TranslationContext context, ElementContext scopeElement, boolean isExpandScopeShadowRoot) {
      boolean isPublicImplementationOnlyElement = isPublic() && context.isImplementationPageObject();
      TypeProvider elementType = asBasicOrUnionType(name, type, isPublicImplementationOnlyElement);
      LocatorCodeGeneration locatorHelper = selector.getCodeGenerationHelper(context);
      MethodParametersTracker addedParameters = new MethodParametersTracker(String.format("element '%s' getter", name));
      addedParameters.setMethodParameters(locatorHelper.getParameters());
      ElementField field =
          new ElementField(
              name, getFindAnnotation(locatorHelper.getLocator(),
              isExpandScopeShadowRoot, isNullable()));
      if (filter != null) {
        filter.setElementFilter(context, Type.BASIC, elementType, name);
        addedParameters.setMethodParameters(filter.getApplyMethodParameters());
        addedParameters.setMethodParameters(filter.getMatcherParameters());
      }
      boolean isList = selector.isReturnAll() && (filter == null || !filter.getFindFirst());
      ElementContext elementContext = isList ?
          new ElementContext.BasicReturnsAll(
              scopeElement, name, elementType, locatorHelper.getLocator(),
              addedParameters.getMethodParameters(), isNullable()) :
          new ElementContext.Basic(
              scopeElement, name, elementType, locatorHelper.getLocator(),
              addedParameters.getMethodParameters(), isNullable());
      final PageObjectMethod method;
      final TypeProvider implType = elementType instanceof UnionType ?
          new BasicElementUnionTypeImpl(elementType) : BASIC_ELEMENT_IMPL_CLASS;
      if (filter != null) {
        method =
            new ElementMethod.Filtered(
                scopeElement,
                name,
                elementType,
                implType,
                locatorHelper.getParameters(),
                isPublic(),
                filter.applyMethod,
                filter.getApplyMethodParameters(),
                filter.getMatcherType(),
                filter.getMatcherParameters(),
                filter.getFindFirst());
      } else if (isList) {
        method = new ElementMethod.Multiple(elementContext, locatorHelper.getParameters(), isPublic(), implType);
      } else {
        method = new ElementMethod.Single(elementContext, locatorHelper.getParameters(), isPublic(), implType);
      }
      context.setClassField(field);
      context.setElement(elementContext);
      context.setMethod(method);
      elementContext.setElementMethod(method);
      // scope element method is invoked
      elementContext.getScopeElement().setElementMethodUsage(context);
      context.setTestableElement(name, new ElementUnitTestHelper(
              locatorHelper.getLocator().getStringValue(),
              scopeElement == null? null : scopeElement.getName(),
              isExpandScopeShadowRoot,
              isList
      ));
      return new ElementContext[] {elementContext};
    }
  }

  class Container extends Traversal {

    private Container() {
      if (filter != null
          || isNullable != null
          || elements != null
          || shadow != null) {
        throw new UtamError(Type.CONTAINER.getSupportedPropertiesErr(name));
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
      LocatorCodeGeneration selectorContext = selector.getCodeGenerationHelper(context);
      ElementContext elementContext = new ElementContext.Container(scopeElement, name);
      PageObjectMethod method;
      if (selector.isReturnAll()) {
        method = new ContainerMethod.WithSelectorReturnsList(
            scopeElement, isExpandScopeShadowRoot, name, selectorContext, isPublic());
      } else {
        method = new ContainerMethod.WithSelector(
            scopeElement, isExpandScopeShadowRoot, name, selectorContext, isPublic());
      }
      elementContext.setElementMethod(method);
      context.setElement(elementContext);
      context.setMethod(method);
      // scope element method is invoked
      elementContext.getScopeElement().setElementMethodUsage(context);
      return new ElementContext[]{null, elementContext};
    }
  }

  class Frame extends Traversal {
    private Frame() {
      if (filter != null
          || isNullable != null
          || elements != null
          || shadow != null) {
        throw new UtamError(Type.FRAME.getSupportedPropertiesErr(name));
      }
      if (selector == null) {
        throw new UtamError(String.format(ERR_ELEMENT_MISSING_SELECTOR_PROPERTY, name));
      }
      if (selector.isReturnAll()) {
        throw new UtamError(String.format(ERR_FRAME_LIST_SELECTOR_NOT_ALLOWED, name));
      }
    }

    @Override
    ElementContext[] traverse(TranslationContext context, ElementContext scopeElement,
        boolean isExpandScopeShadowRoot) {
      LocatorCodeGeneration selectorContext = selector.getCodeGenerationHelper(context);
      ElementField field =
          new ElementField(
              name, getFindAnnotation(selectorContext.getLocator(),
              isExpandScopeShadowRoot, isNullable()));
      ElementContext elementContext = new ElementContext.Frame(scopeElement, name, selectorContext);
      PageObjectMethod method = new FrameMethod(elementContext, isPublic(), selectorContext.getParameters());
      elementContext.setElementMethod(method);
      context.setClassField(field);
      context.setElement(elementContext);
      context.setMethod(method);
      return new ElementContext[]{elementContext};
    }
  }
}
