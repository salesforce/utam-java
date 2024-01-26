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
import static utam.compiler.helpers.ElementContext.ROOT_ELEMENT_NAME;
import static utam.compiler.helpers.TypeUtilities.BASIC_ELEMENT_IMPL_CLASS;
import static utam.compiler.helpers.TypeUtilities.CONTAINER_ELEMENT_TYPE_NAME;
import static utam.compiler.helpers.TypeUtilities.FRAME_ELEMENT_TYPE_NAME;
import static utam.compiler.lint.PageObjectLintingImpl.Element.LINTING_BASIC_TYPE;
import static utam.compiler.lint.PageObjectLintingImpl.Element.LINTING_CONTAINER_TYPE;
import static utam.compiler.lint.PageObjectLintingImpl.Element.LINTING_FRAME_TYPE;
import static utam.compiler.types.BasicElementInterface.processBasicTypeNode;
import static utam.compiler.types.BasicElementUnionType.asBasicOrUnionType;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.JsonNode;
import java.util.Collections;
import java.util.List;
import utam.compiler.UtamCompilationError;
import utam.compiler.grammar.UtamMethodActionWaitFor.UtamMethodActionWaitForElement;
import utam.compiler.helpers.ElementContext;
import utam.compiler.helpers.ElementUnitTestHelper;
import utam.compiler.helpers.LocatorCodeGeneration;
import utam.compiler.helpers.TranslationContext;
import utam.compiler.lint.PageObjectLintingImpl.Element;
import utam.compiler.lint.PageObjectLintingImpl.ElementScope;
import utam.compiler.lint.PageObjectLintingImpl.ElementSelector;
import utam.compiler.lint.PageObjectLintingImpl.Method;
import utam.compiler.representation.ContainerMethod;
import utam.compiler.representation.CustomElementMethod;
import utam.compiler.representation.ElementField;
import utam.compiler.representation.ElementMethod;
import utam.compiler.representation.FrameMethod;
import utam.compiler.representation.MatcherObject;
import utam.compiler.representation.MethodParametersTracker;
import utam.compiler.translator.TranslationTypesConfigJava;
import utam.compiler.types.BasicElementUnionTypeImpl;
import utam.core.declarative.lint.PageObjectLinting.ElementLinting;
import utam.core.declarative.representation.PageObjectMethod;
import utam.core.declarative.representation.TypeProvider;
import utam.core.declarative.representation.UnionType;
import utam.core.element.Locator;

/**
 * Page Object Element
 *
 * @author elizaveta.ivanova
 * @since 228
 */
public final class UtamElement {

  /** The default CSS selector for a container */
  public static final String DEFAULT_CONTAINER_SELECTOR_CSS = ":scope > *:first-child";

  private static final String CONTAINER_SUPPORTED_PROPERTIES =
      String.join(", ", "name", "public", "selector", "type");
  private static final String FRAME_SUPPORTED_PROPERTIES =
      String.join(", ", "name", "public", "selector", "type");

  private final String name;
  private final UtamShadowElement shadow;
  private final List<UtamElement> elements;
  private final Boolean isNullable;
  private final Traversal traversal;
  private final UtamMethodDescription description;
  private final Boolean isPublic;
  private final UtamElementFilter filter;
  private UtamSelector selector;
  private final Boolean isWait;
  private final Boolean isLoad;

  @JsonCreator
  UtamElement(
      @JsonProperty(value = "type", defaultValue = "[]") JsonNode type,
      @JsonProperty(value = "name", required = true) String name,
      @JsonProperty(value = "public") Boolean isPublic,
      @JsonProperty(value = "nullable") Boolean isNullable,
      @JsonProperty(value = "wait") Boolean isWait,
      @JsonProperty(value = "load") Boolean isLoad,
      @JsonProperty(value = "selector") JsonNode selectorNode,
      @JsonProperty(value = "filter") JsonNode filterNode,
      @JsonProperty("shadow") JsonNode shadowNode,
      @JsonProperty("elements") JsonNode elementsNode,
      @JsonProperty("description") JsonNode descriptionNode) {
    this.name = name;
    VALIDATION.validateNotEmptyString(name, "element", "name");
    String validationContext = String.format("element \"%s\"", name);
    this.isPublic = isPublic;
    this.filter = processFilterNode(filterNode, name);
    this.isNullable = isNullable;
    this.selector = processSelectorNode(selectorNode, name);
    this.shadow = processShadowNode(shadowNode, validationContext + " shadow");
    this.elements = processElementsNode(elementsNode, validationContext + " elements");
    this.traversal = processTypeNode(type);
    this.description = processMethodDescriptionNode(descriptionNode, validationContext);
    this.isWait = isWait;
    this.isLoad = isLoad;
  }

  /**
   * parse "elements"
   *
   * @param elementsNode node with elements
   * @param parserContext context of the parser
   * @return list of parsed elements
   */
  static List<UtamElement> processElementsNode(JsonNode elementsNode, String parserContext) {
    List<UtamElement> elements =
        VALIDATION.validateOptionalNotEmptyArray(elementsNode, parserContext, "elements");
    if (isEmptyNode(elementsNode)) {
      return elements;
    }
    for (JsonNode elementNode : elementsNode) {
      VALIDATION.validateNotNullObject(elementNode, parserContext, "element");
      UtamElement element =
          readNode(elementNode, UtamElement.class, VALIDATION.getErrorMessage(200, parserContext));
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
    String[] type = processBasicTypeNode(typeNode, String.format("element \"%s\" type", name));
    if (type == null) {
      throw new UtamCompilationError(
          typeNode, VALIDATION.getErrorMessage(201, name, nodeToString(typeNode)));
    }
    return new Basic(type);
  }

  private boolean isPublic() {
    return Boolean.TRUE.equals(isPublic);
  }

  private boolean isNullable() {
    return Boolean.TRUE.equals(isNullable);
  }

  private boolean isWait() {
    return Boolean.TRUE.equals(isWait);
  }

  private boolean isLoad() {
    return Boolean.TRUE.equals(isLoad);
  }

  private boolean isWaitOrLoad() {
    return isLoad() || isWait();
  }

  private boolean isPrivateWait() {
    return isLoad() && !isWait();
  }

  /**
   * Some functionality in compiler can lead to adjusting JSON itself, for example "wait"
   *
   * @param pageObject de-serialized page object
   */
  void preProcessElement(UtamPageObject pageObject) {
    if (isWaitOrLoad()) {
      UtamComposeMethod composeMethod = buildWaitForComposeMethod();
      pageObject.setComposeMethod(composeMethod);
      if (isLoad()) {
        pageObject.getBeforeLoad().add(buildApplyForLoad(composeMethod.name));
      }
    }
    if (shadow != null) {
      shadow.elements.forEach(utamElement -> utamElement.preProcessElement(pageObject));
    }
    elements.forEach(utamElement -> utamElement.preProcessElement(pageObject));
  }

  /**
   * Build compose method to wait for the element
   *
   * @return UtamComposeMethod object
   */
  private UtamComposeMethod buildWaitForComposeMethod() {
    String methodName = "waitFor" + name.substring(0, 1).toUpperCase() + name.substring(1);
    UtamMethodDescription description =
        new UtamMethodDescription(
            Collections.singletonList(String.format("method that waits for element \"%s\"", name)),
            null,
            null,
            null);
    List<UtamMethodAction> compose =
        Collections.singletonList(new UtamMethodActionWaitForElement(name, isLoad()));
    return new UtamComposeMethod(methodName, description, compose, !isPrivateWait());
  }

  /**
   * get apply to add to beforeload method
   *
   * @param methodName method invocation code to be added to load()
   * @return UtamMethodAction object
   */
  private UtamMethodAction buildApplyForLoad(String methodName) {
    return new UtamMethodActionApply(null, methodName, null, null, null, null, false);
  }

  void traverse(
      TranslationContext context, ElementContext scopeElement, boolean isExpandScopeShadowRoot) {
    ElementContext nextScope =
        traversal.traverse(context, scopeElement, isExpandScopeShadowRoot)[0];
    for (UtamElement element : elements) {
      element.traverse(context, nextScope, false);
    }
    if (shadow != null) {
      if (!name.equals(ROOT_ELEMENT_NAME)) {
        context.getLintingObject().setShadowBoundary(name);
      }
      for (UtamElement element : shadow.elements) {
        element.traverse(context, nextScope, true);
      }
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
        TranslationContext context, ElementContext scopeElement, boolean isExpandScopeShadowRoot);
  }

  class Custom extends Traversal {

    private Custom(String type) {
      super(type);
      String validationContext = String.format("element \"%s\"", name);
      VALIDATION.validateRequiredProperty(selector, validationContext, "selector");
      if (filter != null && !selector.isReturnAll()) {
        throw new UtamCompilationError(VALIDATION.getErrorMessage(302, name));
      }
      if (shadow != null || !elements.isEmpty()) {
        throw new UtamCompilationError(VALIDATION.getErrorMessage(205, name));
      }
    }

    @Override
    final ElementContext[] traverse(
        TranslationContext context, ElementContext scopeElement, boolean isExpandScopeShadowRoot) {
      boolean isReturnList = selector.isReturnAll() && (filter == null || !filter.isFindFirst());
      LocatorCodeGeneration selectorContext =
          selector.getElementCodeGenerationHelper(name, context);
      MethodParametersTracker parameters =
          new MethodParametersTracker(String.format("element '%s' getter", name));
      parameters.setMethodParameters(selectorContext.getParameters());
      TypeProvider elementType = context.getType(type[0]);
      MatcherObject filterMatcher = null;
      if (filter != null) {
        filterMatcher = filter.setElementFilter(context, elementType, name, false);
        parameters.setMethodParameters(filter.getApplyMethodParameters());
        parameters.setMethodParameters(filter.getMatcherParameters());
      }
      Locator locator = selectorContext.getLocator();
      // set element
      ElementContext component =
          isReturnList
              ? new ElementContext.CustomReturnsAll(
                  scopeElement,
                  name,
                  elementType,
                  locator,
                  parameters.getMethodParameters(),
                  isNullable())
              : new ElementContext.Custom(
                  scopeElement,
                  name,
                  elementType,
                  locator,
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
      ElementField field =
          new ElementField(name, getFindAnnotation(locator, isExpandScopeShadowRoot, isNullable()));
      context.setElement(component, field);
      context.setMethod(method);
      component.setElementMethod(method, context);
      context.setTestableElement(
          name,
          new ElementUnitTestHelper(
              locator.getStringValue(),
              scopeElement.getName(),
              isExpandScopeShadowRoot,
              isReturnList));
      ElementSelector lintingSelector =
          new ElementSelector(
              locator,
              selector.isReturnAll(),
              filter != null ? filter.applyMethod : null,
              filter != null ? filterMatcher.getMatcherType() : null);
      ElementLinting lintingContext =
          new Element(
              name,
              elementType.getFullName(),
              lintingSelector,
              new ElementScope(scopeElement.getName(), isExpandScopeShadowRoot));
      context.getLintingObject().setElement(lintingContext);
      context.getLintingObject().setMethod(new Method(method.getDeclaration().getName(), true));
      return new ElementContext[] {null, component};
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
      if (isReturnsList && (elements.size() > 0 || shadow != null)) {
        throw new UtamCompilationError(VALIDATION.getErrorMessage(203, name));
      }
    }

    @Override
    final ElementContext[] traverse(
        TranslationContext context, ElementContext scopeElement, boolean isExpandScopeShadowRoot) {
      String parserContext = String.format("element \"%s\"", name);
      boolean isPublicImplementationOnlyElement =
          isPublic() && context.isImplementationPageObject();
      TypeProvider elementType = asBasicOrUnionType(name, type, isPublicImplementationOnlyElement);
      LocatorCodeGeneration selectorContext =
          selector.getElementCodeGenerationHelper(name, context);
      Locator locator = selectorContext.getLocator();
      MethodParametersTracker addedParameters = new MethodParametersTracker(parserContext);
      addedParameters.setMethodParameters(selectorContext.getParameters());
      ElementField field =
          new ElementField(
              name,
              getFindAnnotation(
                  selectorContext.getLocator(), isExpandScopeShadowRoot, isNullable()));
      MatcherObject filterMatcher = null;
      if (filter != null) {
        filterMatcher = filter.setElementFilter(context, elementType, name, true);
        addedParameters.setMethodParameters(filter.getApplyMethodParameters());
        addedParameters.setMethodParameters(filter.getMatcherParameters());
      }
      boolean isList = selector.isReturnAll() && (filter == null || !filter.isFindFirst());
      ElementContext elementContext =
          isList
              ? new ElementContext.BasicReturnsAll(
                  scopeElement,
                  name,
                  elementType,
                  locator,
                  addedParameters.getMethodParameters(),
                  isNullable())
              : new ElementContext.Basic(
                  scopeElement,
                  name,
                  elementType,
                  locator,
                  addedParameters.getMethodParameters(),
                  isNullable());
      final PageObjectMethod method;
      final TypeProvider implType =
          elementType instanceof UnionType
              ? new BasicElementUnionTypeImpl(elementType)
              : BASIC_ELEMENT_IMPL_CLASS;
      if (filter != null) {
        method =
            new ElementMethod.Filtered(
                scopeElement,
                name,
                elementType,
                implType,
                selectorContext.getParameters(),
                isPublic(),
                filter.applyMethod,
                filter.getApplyMethodParameters(),
                filterMatcher.getMatcherType(),
                filter.getMatcherParameters(),
                filter.isFindFirst(),
                description);
      } else if (isList) {
        method =
            new ElementMethod.Multiple(
                elementContext, selectorContext.getParameters(), isPublic(), implType, description);
      } else {
        method =
            new ElementMethod.Single(
                elementContext, selectorContext.getParameters(), isPublic(), implType, description);
      }
      context.setElement(elementContext, field);
      context.setMethod(method);
      elementContext.setElementMethod(method, context);
      context.setTestableElement(
          name,
          new ElementUnitTestHelper(
              locator.getStringValue(),
              scopeElement == null ? null : scopeElement.getName(),
              isExpandScopeShadowRoot,
              isList));
      ElementSelector lintingSelector =
          new ElementSelector(
              locator,
              selector.isReturnAll(),
              filter != null ? filter.applyMethod : null,
              filter != null ? filterMatcher.getMatcherType() : null);
      ElementLinting lintingContext =
          new Element(
              name,
              LINTING_BASIC_TYPE,
              lintingSelector,
              new ElementScope(scopeElement.getName(), isExpandScopeShadowRoot));
      context.getLintingObject().setElement(lintingContext);
      context.getLintingObject().setMethod(new Method(method.getDeclaration().getName(), true));
      return new ElementContext[] {elementContext};
    }
  }

  class Container extends Traversal {

    private Container() {
      super(CONTAINER_ELEMENT_TYPE_NAME);
      String validationContext = String.format("element \"%s\"", name);
      if (selector == null) {
        selector = new UtamSelector(DEFAULT_CONTAINER_SELECTOR_CSS, null, null, null, false, null);
      }
      if (shadow != null || !elements.isEmpty()) {
        throw new UtamCompilationError(VALIDATION.getErrorMessage(205, name));
      }
      VALIDATION.validateUnsupportedProperty(
          filter, validationContext, "filter", CONTAINER_SUPPORTED_PROPERTIES);
      VALIDATION.validateUnsupportedProperty(
          isNullable, validationContext, "nullable", CONTAINER_SUPPORTED_PROPERTIES);
    }

    @Override
    ElementContext[] traverse(
        TranslationContext context, ElementContext scopeElement, boolean isExpandScopeShadowRoot) {
      LocatorCodeGeneration locator = selector.getElementCodeGenerationHelper(name, context);
      ElementContext elementContext = new ElementContext.Container(scopeElement, name);
      PageObjectMethod method;
      if (selector.isReturnAll()) {
        method =
            new ContainerMethod.WithSelectorReturnsList(
                scopeElement, isExpandScopeShadowRoot, name, locator, isPublic(), description);
      } else {
        method =
            new ContainerMethod.WithSelector(
                scopeElement, isExpandScopeShadowRoot, name, locator, isPublic(), description);
      }
      elementContext.setElementMethod(method, context);
      context.setElement(elementContext, null);
      context.setMethod(method);
      ElementSelector lintingSelector =
          new ElementSelector(locator.getLocator(), selector.isReturnAll(), null, null);
      ElementLinting lintingContext =
          new Element(
              name,
              LINTING_CONTAINER_TYPE,
              lintingSelector,
              new ElementScope(scopeElement.getName(), isExpandScopeShadowRoot));
      context.getLintingObject().setElement(lintingContext);
      context.getLintingObject().setMethod(new Method(method.getDeclaration().getName(), true));
      return new ElementContext[] {null, elementContext};
    }
  }

  class Frame extends Traversal {

    private Frame() {
      super(FRAME_ELEMENT_TYPE_NAME);
      String validationContext = String.format("element \"%s\"", name);
      VALIDATION.validateRequiredProperty(selector, validationContext, "selector");
      if (shadow != null || !elements.isEmpty()) {
        throw new UtamCompilationError(VALIDATION.getErrorMessage(205, name));
      }
      VALIDATION.validateUnsupportedProperty(
          filter, validationContext, "filter", FRAME_SUPPORTED_PROPERTIES);
      VALIDATION.validateUnsupportedProperty(
          isNullable, validationContext, "nullable", FRAME_SUPPORTED_PROPERTIES);
      if (selector.isReturnAll()) {
        throw new UtamCompilationError(VALIDATION.getErrorMessage(204, name));
      }
    }

    @Override
    ElementContext[] traverse(
        TranslationContext context, ElementContext scopeElement, boolean isExpandScopeShadowRoot) {
      LocatorCodeGeneration locator = selector.getElementCodeGenerationHelper(name, context);
      ElementField field =
          new ElementField(
              name, getFindAnnotation(locator.getLocator(), isExpandScopeShadowRoot, isNullable()));
      ElementContext elementContext = new ElementContext.Frame(scopeElement, name, locator);
      PageObjectMethod method =
          new FrameMethod(elementContext, isPublic(), locator.getParameters(), description);
      elementContext.setElementMethod(method, context);
      context.setElement(elementContext, field);
      context.setMethod(method);
      ElementSelector lintingSelector =
          new ElementSelector(locator.getLocator(), selector.isReturnAll(), null, null);
      ElementLinting lintingContext =
          new Element(
              name,
              LINTING_FRAME_TYPE,
              lintingSelector,
              new ElementScope(scopeElement.getName(), isExpandScopeShadowRoot));
      context.getLintingObject().setElement(lintingContext);
      context.getLintingObject().setMethod(new Method(method.getDeclaration().getName(), true));
      return new ElementContext[] {elementContext};
    }
  }
}
