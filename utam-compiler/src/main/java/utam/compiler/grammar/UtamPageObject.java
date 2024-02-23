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
import static utam.compiler.grammar.UtamComposeMethod.getComposeStatements;
import static utam.compiler.grammar.UtamComposeMethod.processComposeNodes;
import static utam.compiler.grammar.UtamElement.processElementsNode;
import static utam.compiler.grammar.UtamMetadata.processMetadataNode;
import static utam.compiler.grammar.UtamMethod.processMethodsNode;
import static utam.compiler.grammar.UtamProfile.processProfileNodes;
import static utam.compiler.grammar.UtamRootDescription.processRootDescriptionNode;
import static utam.compiler.grammar.UtamRootSelector.processRootSelectorNode;
import static utam.compiler.grammar.UtamShadowElement.processShadowNode;
import static utam.compiler.helpers.ElementContext.ROOT_ELEMENT_NAME;
import static utam.compiler.helpers.ReturnType.RETURN_VOID;
import static utam.compiler.helpers.TypeUtilities.BASIC_ELEMENT_IMPL_CLASS;
import static utam.compiler.types.BasicElementInterface.processBasicTypeNode;
import static utam.compiler.types.BasicElementUnionType.asBasicOrUnionType;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.JsonNode;
import java.util.ArrayList;
import java.util.List;
import utam.compiler.UtamCompilationError;
import utam.compiler.helpers.ElementContext;
import utam.compiler.helpers.MethodContext;
import utam.compiler.helpers.TranslationContext;
import utam.compiler.helpers.TypeUtilities.FromClass;
import utam.compiler.lint.PageObjectLintingImpl.ElementSelector;
import utam.compiler.lint.PageObjectLintingImpl.Metadata;
import utam.compiler.lint.PageObjectLintingImpl.Method;
import utam.compiler.lint.PageObjectLintingImpl.Root;
import utam.compiler.representation.BeforeLoadMethod;
import utam.compiler.representation.ComposeMethodStatement;
import utam.compiler.representation.RootElementMethod.PrivateCustomType;
import utam.compiler.representation.RootElementMethod.ProtectedDefaultType;
import utam.compiler.representation.RootElementMethod.PublicCustomType;
import utam.compiler.representation.RootElementMethod.PublicDefaultType;
import utam.core.declarative.lint.PageObjectLinting.RootLinting;
import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.PageObjectMethod;
import utam.core.declarative.representation.TypeProvider;
import utam.core.declarative.representation.UnionType;
import utam.core.element.BasicElement;
import utam.core.element.Locator;
import utam.core.framework.context.PlatformType;

/**
 * mapping for a Page Object JSON
 *
 * @author elizaveta.ivanova
 * @since 228
 */
public final class UtamPageObject {

  public static final String BEFORE_LOAD_METHOD_NAME = "load";
  private static final TypeProvider PUBLIC_DEFAULT_ROOT_ELEMENT_TYPE =
      new FromClass(BasicElement.class);
  private static final String INTERFACE_PROPERTIES =
      String.join(", ", "root", "interface", "methods", "type", "exposeRootElement");
  final String implementsType;
  final boolean isAbstract;
  final Locator rootLocator;
  final boolean isRootPageObject;
  final PlatformType platform;
  final List<UtamProfile> profile;
  final UtamRootDescription description;
  private final JsonNode beforeLoadNode;
  private final List<UtamMethodAction> beforeLoad;
  private final String[] type;
  private final boolean exposeRootElement;
  private final List<UtamMethod> methods;
  private final List<UtamElement> elements;
  private final UtamShadowElement shadow;
  private final UtamMetadata metadata;

  @JsonCreator
  UtamPageObject(
      @JsonProperty(value = "implements") String implementsType,
      @JsonProperty(value = "interface", defaultValue = "false") boolean isAbstract,
      @JsonProperty(value = "platform") String platformString,
      @JsonProperty(value = "profile") JsonNode profilesNode,
      // to expose root element
      @JsonProperty(value = "type", defaultValue = "[]") JsonNode typeNode,
      @JsonProperty(value = "exposeRootElement", defaultValue = "false")
          boolean isExposeRootElement,
      // root selector
      @JsonProperty(value = "root", defaultValue = "false") boolean isRootPageObject,
      @JsonProperty(value = "selector") JsonNode selectorNode,
      // nested nodes
      @JsonProperty("shadow") JsonNode shadowNode,
      @JsonProperty("elements") JsonNode elementsNode,
      @JsonProperty("methods") JsonNode methodsNode,
      @JsonProperty("beforeLoad") JsonNode beforeLoadNode,
      @JsonProperty("description") JsonNode descriptionNode,
      @JsonProperty("metadata") JsonNode metadata) {
    this.implementsType = implementsType;
    this.profile = processProfileNodes(profilesNode);
    if (implementsType == null && !profile.isEmpty()) {
      throw new UtamCompilationError(profilesNode, VALIDATION.getErrorMessage(805));
    }
    this.isAbstract = isAbstract;
    this.methods = processMethodsNode(methodsNode, isAbstract);
    try {
      this.platform = PlatformType.fromString(platformString);
    } catch (Exception e) {
      throw new UtamCompilationError(VALIDATION.getErrorMessage(903, platformString), e);
    }
    this.isRootPageObject = isRootPageObject;
    this.shadow = processShadowNode(shadowNode, "root shadow");
    this.elements = processElementsNode(elementsNode, "root elements");
    this.exposeRootElement = isExposeRootElement;
    String rootElementTypeStructure = "root element type";
    this.type = processBasicTypeNode(typeNode, rootElementTypeStructure);
    if (this.type == null) {
      throw new UtamCompilationError(
          typeNode,
          VALIDATION.getErrorMessage(115, rootElementTypeStructure, nodeToString(typeNode)));
    }
    this.beforeLoadNode = beforeLoadNode;
    this.beforeLoad =
        isEmptyNode(beforeLoadNode)
            ? new ArrayList<>()
            : processComposeNodes(BEFORE_LOAD_METHOD_NAME, beforeLoadNode);
    this.rootLocator = processRootSelectorNode(selectorNode);
    this.description = processRootDescriptionNode(descriptionNode);
    this.metadata = processMetadataNode(metadata);
  }

  private void validateAbstract() {
    String validationContext = "interface";
    VALIDATION.validateUnsupportedProperty(
        rootLocator, validationContext, "selector", INTERFACE_PROPERTIES);
    VALIDATION.validateUnsupportedProperty(
        platform, validationContext, "platform", INTERFACE_PROPERTIES);
    VALIDATION.validateUnsupportedProperty(
        profile, validationContext, "profile", INTERFACE_PROPERTIES);
    VALIDATION.validateUnsupportedProperty(
        implementsType, validationContext, "implements", INTERFACE_PROPERTIES);
    VALIDATION.validateUnsupportedProperty(
        beforeLoad, validationContext, "beforeLoad", INTERFACE_PROPERTIES);
    VALIDATION.validateUnsupportedProperty(
        shadow, validationContext, "shadow", INTERFACE_PROPERTIES);
    VALIDATION.validateUnsupportedProperty(
        elements, validationContext, "elements", INTERFACE_PROPERTIES);
  }

  private void validateRegular(TranslationContext context) {
    if (isRootPageObject && rootLocator == null) {
      throw new UtamCompilationError(VALIDATION.getErrorMessage(902));
    }
    if (!isRootPageObject && rootLocator != null) {
      throw new UtamCompilationError(VALIDATION.getErrorMessage(901));
    }
    if (implementsType != null) {
      VALIDATION.validateNotEmptyString(this.implementsType, "page object root", "implements");
      context.setImplementedType(implementsType);
    }
  }

  /**
   * Some functionality in compiler can lead to adjusting JSON itself. This method runs post process
   */
  void preProcess() {
    if (shadow != null) {
      shadow.elements.forEach(utamElement -> utamElement.preProcessElement(this, methods));
    }
    elements.forEach(utamElement -> utamElement.preProcessElement(this, methods));
  }

  /**
   * As result of post-processing, compiler can add a method
   *
   * @param composeMethod method to add
   */
  void setComposeMethod(UtamComposeMethod composeMethod) {
    this.methods.add(composeMethod);
  }

  void compile(TranslationContext context) {
    if (this.isAbstract) {
      validateAbstract();
    } else {
      validateRegular(context);
    }
    // register element to prevent names collisions
    ElementContext rootElement = setRootElement(context, rootLocator);
    for (UtamElement element : elements) {
      element.traverse(context, rootElement, false);
    }
    if (shadow != null) {
      for (UtamElement element : shadow.elements) {
        element.traverse(context, rootElement, true);
      }
    }
    // should be before processing methods to ensure unique name
    if (!beforeLoad.isEmpty()) {
      context.setMethod(setBeforeLoadMethod(context));
    }
    // set linting information
    methods.forEach(
        declared -> {
          PageObjectMethod method = declared.getMethod(context);
          context
              .getLintingObject()
              .setMethod(new Method(method.getDeclaration().getName(), declared.hasDescription()));
          context.setMethod(method);
        });
  }

  /**
   * depending on root element settings, add root getter and union type
   *
   * @param context translation context
   * @param rootLocator locator of the root element
   * @return root element context
   */
  private ElementContext setRootElement(TranslationContext context, Locator rootLocator) {
    TypeProvider interfaceType = context.getSelfType();
    final TypeProvider elementType;
    final PageObjectMethod rootElementMethod;
    // if type is not set and element should be public - add public getter that returns basic type
    // if type is set - add new type and public or private getter
    // if type is not set and root element is not exposed - use protected method
    // BasePageObject.getRootElement
    if (type.length > 0) {
      elementType = asBasicOrUnionType(ROOT_ELEMENT_NAME, type, false);
      UnionType unionType = (UnionType) elementType;
      rootElementMethod =
          exposeRootElement ? new PublicCustomType(unionType) : new PrivateCustomType(unionType);
      context.setMethod(rootElementMethod);
    } else {
      elementType = exposeRootElement ? PUBLIC_DEFAULT_ROOT_ELEMENT_TYPE : BASIC_ELEMENT_IMPL_CLASS;
      rootElementMethod =
          exposeRootElement
              ? new PublicDefaultType(elementType)
              : new ProtectedDefaultType(elementType);
      if (exposeRootElement) { // for default not exposed root - no need to add method
        context.setMethod(rootElementMethod);
      }
    }
    ElementContext rootElement =
        new ElementContext.Root(interfaceType, rootLocator, elementType, rootElementMethod);
    context.setElement(rootElement, null);
    ElementSelector rootSelector =
        rootLocator != null ? new ElementSelector(rootLocator, false, null, null) : null;
    Metadata metadataLinting =
        this.metadata != null ? new Metadata(this.metadata.getMetadataProperties()) : null;
    RootLinting rootLintingContext =
        new Root(!description.isEmpty(), description.hasAuthor(), rootSelector, metadataLinting);
    context.getLintingObject().setRootContext(rootLintingContext);
    return rootElement;
  }

  private PageObjectMethod setBeforeLoadMethod(TranslationContext context) {
    MethodContext methodContext =
        new MethodContext(BEFORE_LOAD_METHOD_NAME, RETURN_VOID, context, false, false);
    List<ComposeMethodStatement> statements =
        getComposeStatements(context, methodContext, beforeLoad);
    List<MethodParameter> methodParameters = methodContext.getParametersContext().getParameters();
    if (!methodParameters.isEmpty()) {
      throw new UtamCompilationError(beforeLoadNode, VALIDATION.getErrorMessage(904));
    }
    return new BeforeLoadMethod(methodContext, statements);
  }

  List<UtamMethodAction> getBeforeLoad() {
    return beforeLoad;
  }
}
