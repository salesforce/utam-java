/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import static utam.compiler.grammar.UtamElementFilter.processFilterNode;
import static utam.compiler.grammar.UtamMethodDescription.processMethodDescriptionNode;
import static utam.compiler.grammar.UtamPageObject.processElementsNode;
import static utam.compiler.grammar.UtamSelector.processSelectorNode;
import static utam.compiler.grammar.UtamShadowElement.processShadowNode;
import static utam.compiler.helpers.AnnotationUtils.getFindAnnotation;
import static utam.compiler.helpers.TypeUtilities.BASIC_ELEMENT_IMPL_CLASS;
import static utam.compiler.helpers.TypeUtilities.CONTAINER_ELEMENT_TYPE_NAME;
import static utam.compiler.helpers.TypeUtilities.FRAME_ELEMENT_TYPE_NAME;
import static utam.compiler.types.BasicElementInterface.processBasicTypeNode;
import static utam.compiler.types.BasicElementUnionType.asBasicOrUnionType;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.JsonNode;
import java.util.AbstractMap.SimpleEntry;
import java.util.List;
import java.util.Map.Entry;
import utam.compiler.UtamCompilationError;
import utam.compiler.UtamCompilerIntermediateError;
import utam.compiler.helpers.ElementContext;
import utam.compiler.helpers.ElementUnitTestHelper;
import utam.compiler.helpers.LocatorCodeGeneration;
import utam.compiler.helpers.TranslationContext;
import utam.compiler.representation.ContainerMethod;
import utam.compiler.representation.CustomElementMethod;
import utam.compiler.representation.ElementField;
import utam.compiler.representation.ElementMethod;
import utam.compiler.representation.FrameMethod;
import utam.compiler.representation.MatcherObject;
import utam.compiler.representation.MethodParametersTracker;
import utam.compiler.translator.TranslationTypesConfigJava;
import utam.compiler.types.BasicElementUnionTypeImpl;
import utam.core.declarative.representation.PageObjectMethod;
import utam.core.declarative.representation.TypeProvider;
import utam.core.declarative.representation.UnionType;

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
  static final String ERR_FRAME_LIST_SELECTOR_NOT_ALLOWED =
      "element '%s': frame selector cannot return all";

  private final String name;
  private final List<UtamElementProvider> shadow;
  private final List<UtamElementProvider> elements;
  private final Boolean isNullable;
  private final Traversal traversal;
  private final UtamMethodDescription description;
  private UtamSelector selector;
  private final String[] type;
  private final Boolean isPublic; // should be nullable as it's redundant for root
  private final UtamElementFilter filter;

  @JsonCreator
  UtamElement(
      @JsonProperty(value = "type", defaultValue = "[]") JsonNode type,
      @JsonProperty(value = "name", required = true) String name,
      @JsonProperty(value = "public") Boolean isPublic,
      @JsonProperty(value = "nullable") Boolean isNullable,
      @JsonProperty(value = "selector") JsonNode selectorNode,
      @JsonProperty(value = "filter") JsonNode filterNode,
      @JsonProperty("shadow") JsonNode shadowNode,
      @JsonProperty("elements") JsonNode elementsNode,
      @JsonProperty("description") JsonNode descriptionNode) {
    this.name = name;
    String validationContext = String.format("element \"%s\"", name);
    this.isPublic = isPublic;
    this.selector = processSelectorNode(selectorNode, name);
    this.shadow = processShadowNode(shadowNode, validationContext + " shadow");
    this.elements = processElementsNode(elementsNode, validationContext + " elements");
    this.filter = processFilterNode(filterNode, name);
    this.isNullable = isNullable;
    Entry<Traversal, String[]> elementType = processTypeNode(type);
    this.type = elementType.getValue();
    this.traversal = elementType.getKey();
    this.description = processMethodDescriptionNode(descriptionNode, validationContext);
  }

  private Entry<Traversal, String[]> processTypeNode(JsonNode typeNode) {
    if (typeNode != null && typeNode.isTextual()) {
      String value = typeNode.textValue();
      if (CONTAINER_ELEMENT_TYPE_NAME.equals(value)) {
        return new SimpleEntry<>(new Container(), new String[]{value});
      }
      if (FRAME_ELEMENT_TYPE_NAME.equals(value)) {
        return new SimpleEntry<>(new Frame(), new String[]{value});
      }
      if (TranslationTypesConfigJava.isPageObjectType(value)) {
        return new SimpleEntry<>(new Custom(), new String[]{value});
      }
    }
    String typeNodeContent = typeNode == null ? "null" : typeNode.toPrettyString();
    String[] type = processBasicTypeNode(typeNode,
        node -> new UtamCompilerIntermediateError(node, 201, name, typeNodeContent));
    return new SimpleEntry<>(new Basic(), type);
  }

  private boolean isPublic() {
    return Boolean.TRUE.equals(isPublic);
  }

  private boolean isNullable() {
    return Boolean.TRUE.equals(isNullable);
  }

  private void traverse(
      TranslationContext context,
      ElementContext scopeElement,
      JsonNode elementNode,
      boolean isExpandScopeShadowRoot) {
    ElementContext nextScope = traversal
        .traverse(context, scopeElement, elementNode, isExpandScopeShadowRoot)[0];
    for (UtamElementProvider element : elements) {
      element.traverse(context, nextScope);
    }
    for (UtamElementProvider element : shadow) {
      element.traverseShadow(context, nextScope);
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
        TranslationContext context,
        ElementContext scopeElement,
        JsonNode elementNode,
        boolean isExpandScopeShadowRoot);
  }

  /**
   * wraps element object together with json node
   *
   * @author elizaveta.ivanova
   * @since 238
   */
  static class UtamElementProvider {

    private final UtamElement element;
    private final JsonNode elementNode;

    /**
     * instantiate element provider
     *
     * @param element     element
     * @param elementNode json node
     */
    UtamElementProvider(UtamElement element, JsonNode elementNode) {
      this.element = element;
      this.elementNode = elementNode;
    }

    /**
     * traverse elements inside the node
     *
     * @param context      translation context
     * @param scopeElement scope element
     */
    void traverse(TranslationContext context, ElementContext scopeElement) {
      element.traverse(context, scopeElement, elementNode, false);
    }

    /**
     * traverse elements inside the shadow node
     *
     * @param context      translation context
     * @param scopeElement scope element
     */
    void traverseShadow(TranslationContext context, ElementContext scopeElement) {
      element.traverse(context, scopeElement, elementNode, true);
    }
  }

  class Custom extends Traversal {

    private Custom() {
      if (selector == null) {
        throw new UtamCompilerIntermediateError(204, name, "selector");
      }
      if (filter != null && !selector.isReturnAll()) {
        throw new UtamCompilerIntermediateError(302, name);
      }
      if (elements.size() > 0 || shadow.size() > 0) {
        throw new UtamCompilationError(Type.CUSTOM.getSupportedPropertiesErr(name));
      }
    }

    @Override
    final ElementContext[] traverse(
        TranslationContext context,
        ElementContext scopeElement,
        JsonNode elementNode,
        boolean isExpandScopeShadowRoot) {
      boolean isReturnList = selector.isReturnAll() && (filter == null || !filter.getFindFirst());
      LocatorCodeGeneration selectorContext = selector.getElementCodeGenerationHelper(name, context);
      MethodParametersTracker parameters = new MethodParametersTracker(
          String.format("element '%s' getter", name));
      parameters.setMethodParameters(selectorContext.getParameters());
      TypeProvider elementType = context.getType(type[0]);
      MatcherObject filterMatcher = null;
      if (filter != null) {
        filterMatcher = filter.setElementFilter(context, Type.CUSTOM, elementType, name);
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
                filterMatcher.getMatcherType(),
                filter.getMatcherParameters(),
                filter.getFindFirst(),
                description);
      } else if (selector.isReturnAll()) {
        method =
            new CustomElementMethod.Multiple(
                isPublic(),
                name,
                selectorContext.getParameters(),
                scopeElement,
                elementType,
                description);
      } else {
        method =
            new CustomElementMethod.Single(
                isPublic(),
                name,
                selectorContext.getParameters(),
                scopeElement,
                elementType,
                description);
      }
      context.setElement(elementNode, component);
      context.setMethod(method);
      component.setElementMethod(method);
      context.setTestableElement(
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
      context.setClassField(field);
      // scope element method is invoked
      component.getScopeElement().setElementMethodUsage(context);
      return new ElementContext[]{null, component};
    }
  }

  class Basic extends Traversal {

    private Basic() {
      if (selector == null) {
        throw new UtamCompilerIntermediateError(204, name, "selector");
      }
      if (filter != null && !selector.isReturnAll()) {
        throw new UtamCompilerIntermediateError(302, name);
      }
      if (selector.isReturnAll() && (elements.size() > 0 || shadow.size() > 0)) {
        throw new UtamCompilerIntermediateError(205, name, "basic");
      }
    }

    @Override
    final ElementContext[] traverse(TranslationContext context,
        ElementContext scopeElement,
        JsonNode elementNode,
        boolean isExpandScopeShadowRoot) {
      String parserContext = String.format("element \"%s\"", name);
      boolean isPublicImplementationOnlyElement =
          isPublic() && context.isImplementationPageObject();
      TypeProvider elementType = asBasicOrUnionType(name, type, isPublicImplementationOnlyElement);
      LocatorCodeGeneration locatorHelper = selector.getElementCodeGenerationHelper(name, context);
      MethodParametersTracker addedParameters = new MethodParametersTracker(parserContext);
      addedParameters.setMethodParameters(locatorHelper.getParameters());
      ElementField field =
          new ElementField(
              name, getFindAnnotation(locatorHelper.getLocator(),
              isExpandScopeShadowRoot, isNullable()));
      MatcherObject filterMatcher = null;
      if (filter != null) {
        filterMatcher = filter.setElementFilter(context, Type.BASIC, elementType, name);
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
                filterMatcher.getMatcherType(),
                filter.getMatcherParameters(),
                filter.getFindFirst(),
                description);
      } else if (isList) {
        method = new ElementMethod.Multiple(elementContext, locatorHelper.getParameters(),
            isPublic(), implType, description);
      } else {
        method = new ElementMethod.Single(elementContext, locatorHelper.getParameters(), isPublic(),
            implType, description);
      }
      context.setClassField(field);
      context.setElement(elementNode, elementContext);
      context.setMethod(method);
      elementContext.setElementMethod(method);
      // scope element method is invoked
      elementContext.getScopeElement().setElementMethodUsage(context);
      context.setTestableElement(name, new ElementUnitTestHelper(
          locatorHelper.getLocator().getStringValue(),
          scopeElement == null ? null : scopeElement.getName(),
          isExpandScopeShadowRoot,
          isList
      ));
      return new ElementContext[]{elementContext};
    }
  }

  class Container extends Traversal {

    private Container() {
      if (filter != null
          || isNullable != null
          || elements.size() > 0
          || shadow.size() > 0) {
        throw new UtamCompilationError(Type.CONTAINER.getSupportedPropertiesErr(name));
      }
      if (selector == null) {
        selector = new UtamSelector(
            DEFAULT_CONTAINER_SELECTOR_CSS,
            null,
            null,
            null,
            false,
            null);
      }
    }

    @Override
    ElementContext[] traverse(TranslationContext context,
        ElementContext scopeElement,
        JsonNode elementNode,
        boolean isExpandScopeShadowRoot) {
      LocatorCodeGeneration selectorContext = selector.getElementCodeGenerationHelper(name, context);
      ElementContext elementContext = new ElementContext.Container(scopeElement, name);
      PageObjectMethod method;
      if (selector.isReturnAll()) {
        method = new ContainerMethod.WithSelectorReturnsList(
            scopeElement, isExpandScopeShadowRoot, name, selectorContext, isPublic(), description);
      } else {
        method = new ContainerMethod.WithSelector(
            scopeElement, isExpandScopeShadowRoot, name, selectorContext, isPublic(), description);
      }
      elementContext.setElementMethod(method);
      context.setElement(elementNode, elementContext);
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
          || elements.size() > 0
          || shadow.size() > 0) {
        throw new UtamCompilationError(Type.FRAME.getSupportedPropertiesErr(name));
      }
      if (selector == null) {
        throw new UtamCompilerIntermediateError(204, name, "selector");
      }
      if (selector.isReturnAll()) {
        throw new UtamCompilationError(String.format(ERR_FRAME_LIST_SELECTOR_NOT_ALLOWED, name));
      }
    }

    @Override
    ElementContext[] traverse(TranslationContext context,
        ElementContext scopeElement,
        JsonNode elementNode,
        boolean isExpandScopeShadowRoot) {
      LocatorCodeGeneration selectorContext = selector.getElementCodeGenerationHelper(name, context);
      ElementField field =
          new ElementField(
              name, getFindAnnotation(selectorContext.getLocator(),
              isExpandScopeShadowRoot, isNullable()));
      ElementContext elementContext = new ElementContext.Frame(scopeElement, name, selectorContext);
      PageObjectMethod method = new FrameMethod(elementContext, isPublic(),
          selectorContext.getParameters(), description);
      elementContext.setElementMethod(method);
      context.setClassField(field);
      context.setElement(elementNode, elementContext);
      context.setMethod(method);
      return new ElementContext[]{elementContext};
    }
  }
}
