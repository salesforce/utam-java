/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import static utam.compiler.diagnostics.ValidationUtilities.VALIDATION;
import static utam.compiler.grammar.JsonDeserializer.isEmptyNode;
import static utam.compiler.grammar.JsonDeserializer.nodeToString;
import static utam.compiler.grammar.JsonDeserializer.readNode;
import static utam.compiler.grammar.UtamElementFilter.processFilterNode;
import static utam.compiler.grammar.UtamMethodDescription.processMethodDescriptionNode;
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
import java.util.List;
import utam.compiler.UtamCompilationError;
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

  private final String name;
  private final UtamShadowElement shadow;
  private final List<UtamElement> elements;
  private final Boolean isNullable;
  private final Traversal traversal;
  private final UtamMethodDescription description;
  private UtamSelector selector;
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
    VALIDATION.validateNotEmptyString(name, "element", "name");
    String validationContext = String.format("element \"%s\"", name);
    this.isPublic = isPublic;
    this.shadow = processShadowNode(shadowNode, validationContext + " shadow");
    this.elements = processElementsNode(elementsNode, validationContext + " elements");
    this.filter = processFilterNode(filterNode, name);
    this.isNullable = isNullable;
    this.selector = processSelectorNode(selectorNode, name);
    this.traversal = processTypeNode(type);
    this.description = processMethodDescriptionNode(descriptionNode, validationContext);
  }

  /**
   * parse "elements"
   *
   * @param elementsNode  node with elements
   * @param parserContext context of the parser
   * @return list of parsed elements
   */
  static List<UtamElement> processElementsNode(JsonNode elementsNode,
      String parserContext) {
    List<UtamElement> elements = VALIDATION.validateOptionalNotEmptyArray(elementsNode, parserContext, "elements");
    if(isEmptyNode(elementsNode)) {
      return elements;
    }
    for (JsonNode elementNode : elementsNode) {
      VALIDATION.validateNotNullObject(elementNode, parserContext, "element");
      UtamElement element = readNode(elementNode, UtamElement.class, VALIDATION.getErrorMessage(200, parserContext));
      elements.add(element);
    }
    return elements;
  }

  private Traversal processTypeNode(JsonNode typeNode) {
    if (typeNode != null && typeNode.isTextual()) {
      String value = typeNode.textValue();
      if (CONTAINER_ELEMENT_TYPE_NAME.equals(value)) {
        return new Container();
      }
      if (FRAME_ELEMENT_TYPE_NAME.equals(value)) {
        return new Frame();
      }
      if (TranslationTypesConfigJava.isPageObjectType(value)) {
        return new Custom(value);
      }
    }
    String error = VALIDATION.getErrorMessage(201, name, nodeToString(typeNode));
    String[] type = processBasicTypeNode(typeNode, error);
    return new Basic(type);
  }

  private boolean isPublic() {
    return Boolean.TRUE.equals(isPublic);
  }

  private boolean isNullable() {
    return Boolean.TRUE.equals(isNullable);
  }

  void traverse(
      TranslationContext context,
      ElementContext scopeElement,
      boolean isExpandScopeShadowRoot) {
    ElementContext nextScope = traversal
        .traverse(context, scopeElement, isExpandScopeShadowRoot)[0];
    for (UtamElement element : elements) {
      element.traverse(context, nextScope, false);
    }
    if(shadow != null) {
      for (UtamElement element : shadow.elements) {
        element.traverse(context, nextScope, true);
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
  }

  abstract static class Traversal {

    final String[] type;

    Traversal(String[] type) {
      this.type = type;
    }

    Traversal(String type) {
      this.type = new String[] {type};
    }

    // traverse and return next scope
    // if next scope is null, second element is self
    // returning both for testing purposes
    abstract ElementContext[] traverse(
        TranslationContext context,
        ElementContext scopeElement,
        boolean isExpandScopeShadowRoot);
  }


  class Custom extends Traversal {

    private Custom(String type) {
      super(type);
      String validationContext = String.format("element \"%s\"", name);
      VALIDATION.validateRequiredProperty(selector, validationContext, "selector");
      if (filter != null && !selector.isReturnAll()) {
        throw new UtamCompilationError(VALIDATION.getErrorMessage(302, name));
      }
      VALIDATION.validateUnsupportedProperty(elements, validationContext, "elements", Type.CUSTOM.supportedProperties);
      VALIDATION.validateUnsupportedProperty(shadow, validationContext, "shadow", Type.CUSTOM.supportedProperties);
    }

    @Override
    final ElementContext[] traverse(
        TranslationContext context,
        ElementContext scopeElement,
        boolean isExpandScopeShadowRoot) {
      boolean isReturnList = selector.isReturnAll() && (filter == null || !filter.isFindFirst());
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
                filter.isFindFirst(),
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
      context.setElement(component);
      context.setMethod(method);
      component.setElementMethod(method, context);
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
      return new ElementContext[]{null, component};
    }
  }

  class Basic extends Traversal {

    private Basic(String[] type) {
      super(type);
      String validationContext = String.format("element \"%s\"", name);
      VALIDATION.validateRequiredProperty(selector, validationContext, "selector");
      if (filter != null && !selector.isReturnAll()) {
        throw new UtamCompilationError(VALIDATION.getErrorMessage(302, name));
      }
      boolean isReturnsList = selector.isReturnAll() && (filter == null || !filter.isFindFirst());
      if (isReturnsList && (elements.size() > 0 || shadow!= null)) {
        throw new UtamCompilationError(VALIDATION.getErrorMessage(203, name));
      }
    }

    @Override
    final ElementContext[] traverse(TranslationContext context,
        ElementContext scopeElement,
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
      boolean isList = selector.isReturnAll() && (filter == null || !filter.isFindFirst());
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
                filter.isFindFirst(),
                description);
      } else if (isList) {
        method = new ElementMethod.Multiple(elementContext, locatorHelper.getParameters(),
            isPublic(), implType, description);
      } else {
        method = new ElementMethod.Single(elementContext, locatorHelper.getParameters(), isPublic(),
            implType, description);
      }
      context.setClassField(field);
      context.setElement(elementContext);
      context.setMethod(method);
      elementContext.setElementMethod(method, context);
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
      super(CONTAINER_ELEMENT_TYPE_NAME);
      String validationContext = String.format("element \"%s\"", name);
      if (selector == null) {
        selector = new UtamSelector(
            DEFAULT_CONTAINER_SELECTOR_CSS,
            null,
            null,
            null,
            false,
            null);
      }
      VALIDATION.validateUnsupportedProperty(elements, validationContext, "elements",
          Type.CONTAINER.supportedProperties);
      VALIDATION.validateUnsupportedProperty(shadow, validationContext, "shadow",
          Type.CONTAINER.supportedProperties);
      VALIDATION.validateUnsupportedProperty(filter, validationContext, "filter",
          Type.CONTAINER.supportedProperties);
      VALIDATION.validateUnsupportedProperty(isNullable, validationContext, "nullable",
          Type.CONTAINER.supportedProperties);
    }

    @Override
    ElementContext[] traverse(TranslationContext context,
        ElementContext scopeElement,
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
      elementContext.setElementMethod(method, context);
      context.setElement(elementContext);
      context.setMethod(method);
      return new ElementContext[]{null, elementContext};
    }
  }

  class Frame extends Traversal {

    private Frame() {
      super(FRAME_ELEMENT_TYPE_NAME);
      String validationContext = String.format("element \"%s\"", name);
      VALIDATION.validateRequiredProperty(selector, validationContext, "selector");
      VALIDATION.validateUnsupportedProperty(elements, validationContext, "elements",
          Type.FRAME.supportedProperties);
      VALIDATION.validateUnsupportedProperty(shadow, validationContext, "shadow",
          Type.FRAME.supportedProperties);
      VALIDATION.validateUnsupportedProperty(filter, validationContext, "filter",
          Type.FRAME.supportedProperties);
      VALIDATION.validateUnsupportedProperty(isNullable, validationContext, "nullable",
          Type.FRAME.supportedProperties);
      if (selector.isReturnAll()) {
        throw new UtamCompilationError(VALIDATION.getErrorMessage(204, name));
      }
    }

    @Override
    ElementContext[] traverse(TranslationContext context,
        ElementContext scopeElement,
        boolean isExpandScopeShadowRoot) {
      LocatorCodeGeneration selectorContext = selector.getElementCodeGenerationHelper(name, context);
      ElementField field =
          new ElementField(
              name, getFindAnnotation(selectorContext.getLocator(),
              isExpandScopeShadowRoot, isNullable()));
      ElementContext elementContext = new ElementContext.Frame(scopeElement, name, selectorContext);
      PageObjectMethod method = new FrameMethod(elementContext, isPublic(),
          selectorContext.getParameters(), description);
      elementContext.setElementMethod(method, context);
      context.setClassField(field);
      context.setElement(elementContext);
      context.setMethod(method);
      return new ElementContext[]{elementContext};
    }
  }
}
