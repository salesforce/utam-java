/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import static utam.compiler.grammar.JsonDeserializer.isNotArrayOrEmptyArray;
import static utam.compiler.grammar.JsonDeserializer.isEmptyNode;
import static utam.compiler.grammar.JsonDeserializer.readNode;
import static utam.compiler.grammar.UtamComposeMethod.getComposeStatements;
import static utam.compiler.grammar.UtamComposeMethod.processComposeNodes;
import static utam.compiler.grammar.UtamMethod.processMethodsNode;
import static utam.compiler.grammar.UtamRootDescription.processRootDescriptionNode;
import static utam.compiler.grammar.UtamShadowElement.processShadowNode;
import static utam.compiler.helpers.AnnotationUtils.DEPRECATED_ANNOTATION;
import static utam.compiler.helpers.AnnotationUtils.getPageObjectAnnotation;
import static utam.compiler.helpers.AnnotationUtils.getPagePlatformAnnotation;
import static utam.compiler.helpers.ElementContext.ROOT_ELEMENT_NAME;
import static utam.compiler.helpers.TypeUtilities.BASE_PAGE_OBJECT_CLASS;
import static utam.compiler.helpers.TypeUtilities.BASE_ROOT_PAGE_OBJECT_CLASS;
import static utam.compiler.helpers.TypeUtilities.BASIC_ELEMENT_IMPL_CLASS;
import static utam.compiler.helpers.TypeUtilities.PAGE_OBJECT;
import static utam.compiler.helpers.TypeUtilities.ROOT_PAGE_OBJECT;
import static utam.compiler.types.BasicElementInterface.processBasicTypeNode;
import static utam.compiler.types.BasicElementUnionType.asBasicOrUnionType;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.databind.JsonNode;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;
import utam.compiler.UtamCompilationError;
import utam.compiler.UtamCompilerIntermediateError;
import utam.compiler.grammar.UtamElement.UtamElementProvider;
import utam.compiler.grammar.UtamProfile.UtamProfileProvider;
import utam.compiler.helpers.ElementContext;
import utam.compiler.helpers.MethodContext;
import utam.compiler.helpers.ReturnType.MethodReturnType;
import utam.compiler.helpers.TranslationContext;
import utam.compiler.helpers.TypeUtilities.FromClass;
import utam.compiler.representation.BeforeLoadMethod;
import utam.compiler.representation.ComposeMethodStatement;
import utam.compiler.representation.RootElementMethod.PrivateCustomType;
import utam.compiler.representation.RootElementMethod.ProtectedDefaultType;
import utam.compiler.representation.RootElementMethod.PublicCustomType;
import utam.compiler.representation.RootElementMethod.PublicDefaultType;
import utam.core.declarative.representation.AnnotationProvider;
import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.PageObjectMethod;
import utam.core.declarative.representation.TypeProvider;
import utam.core.declarative.representation.UnionType;
import utam.core.element.BasicElement;
import utam.core.element.Locator;
import utam.core.framework.context.PlatformType;
import utam.core.framework.context.Profile;

/**
 * mapping for a Page Object JSON
 *
 * @author elizaveta.ivanova
 * @since 228
 */
final class UtamPageObject {

  static final String BEFORE_LOAD_METHOD_NAME = "load";
  private static final String INTERFACE_PROPERTIES = String.join(", ", "root",
      "interface",
      "methods",
      "type",
      "exposeRootElement");
  final String implementsType;
  final boolean isAbstract;
  private final Locator rootLocator;
  private final JsonNode beforeLoadNode;
  private final List<UtamMethodAction> beforeLoad;
  private final RootElementHelper rootElementHelper;
  private final boolean isRootPageObject;
  private final List<UtamMethod> methods;
  private final PlatformType platform;
  private final UtamProfileProvider profileProvider;
  private final List<UtamElementProvider> elements;
  private final List<UtamElementProvider> shadowElements;
  private final UtamRootDescription description;
  private final List<String> descriptionText = new ArrayList<>();

  @JsonCreator
  UtamPageObject(
      @JsonProperty(value = "implements") String implementsType,
      @JsonProperty(value = "interface", defaultValue = "false") boolean isAbstract,
      @JsonProperty(value = "platform") String platformString,
      @JsonProperty(value = "profile") JsonNode profilesNode,
      // to expose root element
      @JsonProperty(value = "type", defaultValue = "[]") JsonNode typeNode,
      @JsonProperty(value = "exposeRootElement", defaultValue = "false") boolean isExposeRootElement,
      // root selector
      @JsonProperty(value = "root", defaultValue = "false") boolean isRootPageObject,
      @JsonProperty(value = "selector") UtamRootSelector selector,
      // nested nodes
      @JsonProperty("shadow") JsonNode shadowNode,
      @JsonProperty("elements") JsonNode elementsNode,
      @JsonProperty("methods") JsonNode methodsNode,
      @JsonProperty("beforeLoad") JsonNode beforeLoadNode,
      @JsonProperty("description") JsonNode descriptionNode) {
    this.profileProvider = new UtamProfileProvider(profilesNode);
    this.isAbstract = isAbstract;
    this.methods = processMethodsNode(methodsNode, isAbstract);
    try {
      this.platform = PlatformType.fromString(platformString);
    } catch (Exception e) {
      throw new UtamCompilerIntermediateError(903, platformString);
    }
    this.isRootPageObject = isRootPageObject;
    this.implementsType = implementsType;
    if(isAbstract && !isEmptyNode(shadowNode)) {
      throw new UtamCompilerIntermediateError(904, "shadow", INTERFACE_PROPERTIES);
    }
    this.shadowElements = processShadowNode(shadowNode, "root shadow");
    if(isAbstract && !isEmptyNode(elementsNode)) {
      throw new UtamCompilerIntermediateError(904, "elements", INTERFACE_PROPERTIES);
    }
    this.elements = processElementsNode(elementsNode, "root elements");
    this.rootElementHelper = new RootElementHelper(typeNode, isExposeRootElement);
    this.beforeLoadNode = beforeLoadNode;
    this.beforeLoad = processBeforeLoadNodes(isAbstract, beforeLoadNode);
    this.rootLocator = selector == null ? null : selector.getLocator();
    this.description = processRootDescriptionNode(descriptionNode);
  }

  /**
   * process beforeLoad method nodes
   *
   * @param isAbstract boolean to indicate if page object is an interface
   * @param beforeLoadNodes json nodes
   * @return list of statements
   */
  private static List<UtamMethodAction> processBeforeLoadNodes(boolean isAbstract,
      JsonNode beforeLoadNodes) {
    if (isAbstract && !isEmptyNode(beforeLoadNodes)) {
      throw new UtamCompilerIntermediateError(904, "beforeLoad", INTERFACE_PROPERTIES);
    }
    if(isEmptyNode(beforeLoadNodes)) {
      return new ArrayList<>();
    }
    return processComposeNodes(BEFORE_LOAD_METHOD_NAME, beforeLoadNodes);
  }

  /**
   * parse platform node, separated into stand alone method because of error codes
   *
   * @param elementsNode  node with elements
   * @param parserContext context of the parser
   * @return list of parsed elements
   */
  static List<UtamElementProvider> processElementsNode(JsonNode elementsNode,
      String parserContext) {
    List<UtamElementProvider> elements = new ArrayList<>();
    if(isEmptyNode(elementsNode)) {
      return elements;
    }
    if (isNotArrayOrEmptyArray(elementsNode)) {
      throw new UtamCompilerIntermediateError(elementsNode, 12, parserContext, "elements");
    }
    Function<Exception, RuntimeException> parserErrorWrapper = causeErr -> new UtamCompilerIntermediateError(
        causeErr, elementsNode, 200, parserContext, causeErr.getMessage());
    for (JsonNode elementNode : elementsNode) {
      UtamElement element = readNode(elementNode, UtamElement.class, parserErrorWrapper);
      elements.add(new UtamElementProvider(element, elementNode));
    }
    return elements;
  }

  /**
   * get profiles from context
   *
   * @param context translation context
   * @return list of profiles
   */
  List<Profile> getProfiles(TranslationContext context) {
    return this.profileProvider.getProfiles(context);
  }

  List<AnnotationProvider> getAnnotations() {
    List<AnnotationProvider> annotations = new ArrayList<>();
    if (rootLocator != null) {
      annotations.add(getPageObjectAnnotation(rootLocator));
    }
    if (platform != null) {
      annotations.add(getPagePlatformAnnotation(platform));
    }
    if (description.isDeprecated()) {
      annotations.add(DEPRECATED_ANNOTATION);
    }
    return annotations;
  }

  TypeProvider getBaseType() {
    return isRootPageObject ? ROOT_PAGE_OBJECT : PAGE_OBJECT;
  }

  TypeProvider getBaseClass() {
    return isRootPageObject ? BASE_ROOT_PAGE_OBJECT_CLASS : BASE_PAGE_OBJECT_CLASS;
  }

  private void validateAbstract(TranslationContext context, JsonParser parser) {
    /*
    @JsonProperty("shadow") JsonNode shadowNode,
      @JsonProperty("beforeLoad") JsonNode beforeLoadNode,
      @JsonProperty("description") JsonNode descriptionNode
     */
    if (rootLocator != null) {
      throw new UtamCompilationError(parser, context.getErrorMessage(904, "selector", INTERFACE_PROPERTIES));
    }
    if (platform != null) {
      throw new UtamCompilationError(parser, context.getErrorMessage(904, "platform", INTERFACE_PROPERTIES));
    }
    if(profileProvider.isNotEmpty()) {
      throw new UtamCompilationError(parser, context.getErrorMessage(904, "profile", INTERFACE_PROPERTIES));
    }
    if(implementsType != null) {
      throw new UtamCompilationError(parser, context.getErrorMessage(904, "implements", INTERFACE_PROPERTIES));
    }
  }

  private void validateRegular(TranslationContext context, JsonParser parser) {
    if (isRootPageObject && rootLocator == null) {
      throw new UtamCompilationError(parser, context.getErrorMessage(902));
    }
    if (!isRootPageObject && rootLocator != null) {
      throw new UtamCompilationError(parser, context.getErrorMessage(901));
    }
    if (implementsType != null) {
      context.setImplementedType(implementsType);
    } else if(profileProvider.isNotEmpty()){
      throw new UtamCompilationError(profileProvider.node, context.getErrorMessage(805));
    }
  }

  final void compile(TranslationContext context, JsonParser parser) {
    if (this.isAbstract) {
      validateAbstract(context, parser);
    } else {
      validateRegular(context, parser);
    }
    // register element to prevent names collisions
    ElementContext rootElement = rootElementHelper.setRootElementMethod(context, rootLocator);

    for (UtamElementProvider element : elements) {
      element.traverse(context, rootElement);
    }

    for (UtamElementProvider element : shadowElements) {
      element.traverseShadow(context, rootElement);
    }
    // should be before processing methods to ensure unique name
    if (!beforeLoad.isEmpty()) {
      context.setMethod(setBeforeLoadMethod(context));
    }
    methods.forEach(method -> context.setMethod(method.getMethod(context)));
  }

  private PageObjectMethod setBeforeLoadMethod(TranslationContext context) {
    String methodName = BEFORE_LOAD_METHOD_NAME;
    MethodContext methodContext = new MethodContext(methodName, new MethodReturnType(methodName),
        context, null, false);
    List<ComposeMethodStatement> statements = getComposeStatements(context, methodContext,
        beforeLoad);
    List<MethodParameter> methodParameters = methodContext.getParametersContext().getParameters();
    if (!methodParameters.isEmpty()) {
      throw new UtamCompilationError(beforeLoadNode, context.getErrorMessage(905));
    }
    return new BeforeLoadMethod(methodContext, statements);
  }

  /**
   * get page object description
   *
   * @param context translation context
   * @return list of strings
   */
  List<String> getDescription(TranslationContext context) {
    // method can be called twice by interface and impl
    if (this.descriptionText.isEmpty()) {
      this.descriptionText.addAll(
          this.description.getDescription(context.getConfiguredVersion(), context.getJsonPath()));
    }
    return this.descriptionText;
  }

  /**
   * check if page object was marked as deprecated
   *
   * @return boolean
   */
  boolean isDeprecated() {
    return this.description.isDeprecated();
  }

  /**
   * helper class for root element declaration
   *
   * @author elizaveta.ivanova
   * @since 236
   */
  static final class RootElementHelper {

    private static final TypeProvider PUBLIC_DEFAULT_ROOT_ELEMENT_TYPE = new FromClass(
        BasicElement.class);

    private final String[] rootElementType;
    private final boolean isPublic;
    private final JsonNode typeNode;

    RootElementHelper(JsonNode typeNode, boolean isExposeRootElement) {
      String typeNodeValue = typeNode == null ? "null" : typeNode.toPrettyString();
      this.rootElementType = processBasicTypeNode(typeNode,
          node -> new UtamCompilerIntermediateError(node, 101,
              ROOT_ELEMENT_NAME, typeNodeValue));
      this.isPublic = isExposeRootElement;
      this.typeNode = typeNode;
    }

    /**
     * depending on root element settings, add root getter and union type
     *
     * @param context     translation context
     * @param rootLocator locator of the root element
     * @return root element context
     */
    ElementContext setRootElementMethod(TranslationContext context, Locator rootLocator) {
      TypeProvider interfaceType = context.getSelfType();
      final TypeProvider elementType;
      final PageObjectMethod rootElementMethod;
      // if type is not set and element should be public - add public getter that returns basic type
      // if type is set - add new type and public or private getter
      // if type is not set and root element is not exposed - use protected method BasePageObject.getRootElement
      if (rootElementType.length > 0) {
        elementType = asBasicOrUnionType(ROOT_ELEMENT_NAME, rootElementType, false);
        UnionType unionType = (UnionType) elementType;
        rootElementMethod =
            isPublic ? new PublicCustomType(unionType) : new PrivateCustomType(unionType);
        context.setMethod(rootElementMethod);
      } else {
        elementType = isPublic ? PUBLIC_DEFAULT_ROOT_ELEMENT_TYPE : BASIC_ELEMENT_IMPL_CLASS;
        rootElementMethod =
            isPublic ? new PublicDefaultType(elementType) : new ProtectedDefaultType(elementType);
        if (isPublic) { // for default not exposed root - no need to add method
          context.setMethod(rootElementMethod);
        }
      }
      ElementContext rootElement = new ElementContext.Root(interfaceType, rootLocator, elementType);
      context.setElement(typeNode, rootElement);
      rootElement.setElementMethod(rootElementMethod);
      return rootElement;
    }
  }
}
