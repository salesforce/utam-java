/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import static utam.compiler.grammar.UtamMethod.ERR_BEFORE_LOAD_HAS_NO_ARGS;
import static utam.compiler.grammar.UtamMethod.getComposeStatements;
import static utam.compiler.grammar.UtamRootDescription.processRootDescriptionNode;
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
import com.fasterxml.jackson.databind.JsonNode;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Stream;
import utam.compiler.UtamCompilationError;
import utam.compiler.helpers.ElementContext;
import utam.compiler.helpers.MethodContext;
import utam.compiler.helpers.ReturnType;
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

/**
 * mapping for a Page Object JSON
 *
 * @author elizaveta.ivanova
 * @since 228
 */
final class UtamPageObject {

  static final String BEFORE_LOAD_METHOD_NAME = "load";
  static final String ERR_ROOT_PROFILE_HAS_NO_INTERFACE =
      "profile can only be set for a page object that implements an interface";
  static final String ERR_ROOT_MISSING_SELECTOR =
      "root page object requires default selector property";
  static final String ERR_ROOT_REDUNDANT_SELECTOR = "non root page object can't have selector";
  static final String ERR_ROOT_ABSTRACT = "interface declaration can only have 'methods' property";
  static final String ERR_PROFILE_IS_REQUIRED = "implementing class should have profiles";
  String implementsType;
  final Locator rootLocator;
  private final UtamMethodAction[] beforeLoad;
  private final RootElementHelper rootElementHelper;
  boolean isAbstract;
  boolean isRootPageObject;
  UtamMethod[] methods;
  String platform;
  UtamProfile[] profiles;
  UtamShadowElement shadow;
  UtamElement[] elements;
  private final UtamRootDescription description;

  @JsonCreator
  UtamPageObject(
      @JsonProperty(value = "implements") String implementsType,
      @JsonProperty(value = "interface", defaultValue = "false") boolean isAbstract,
      @JsonProperty(value = "platform") String platform,
      @JsonProperty(value = "profile") UtamProfile[] profiles,
      // to expose root element
      @JsonProperty(value = "type", defaultValue = "[]") JsonNode typeNode,
      @JsonProperty(value = "exposeRootElement", defaultValue = "false") boolean isExposeRootElement,
      // root selector
      @JsonProperty(value = "root", defaultValue = "false") boolean isRootPageObject,
      @JsonProperty(value = "selector") UtamRootSelector selector,
      // nested nodes
      @JsonProperty("shadow") UtamShadowElement shadow,
      @JsonProperty("elements") UtamElement[] elements,
      @JsonProperty("methods") UtamMethod[] methods,
      @JsonProperty("beforeLoad") UtamMethodAction[] beforeLoad,
      @JsonProperty("description") JsonNode descriptionNode) {
    this.profiles = profiles;
    this.methods = methods;
    this.isAbstract = isAbstract;
    this.platform = platform;
    this.isRootPageObject = isRootPageObject;
    this.implementsType = implementsType;
    this.shadow = shadow;
    this.elements = elements;
    this.rootElementHelper = new RootElementHelper(typeNode, isExposeRootElement);
    this.beforeLoad = beforeLoad;
    this.rootLocator = selector == null ? null : selector.getLocator();
    this.description = processRootDescriptionNode(descriptionNode);
    validate();
  }

  // used in tests
  UtamPageObject(boolean isRoot, UtamSelector selector) {
    this(null, false, null, null, null, false, isRoot, selector, null, null, null, null, null);
  }

  // used in tests
  UtamPageObject() {
    this(null, false, null, null, null, false, false, null, null, null, null, null, null);
  }

  void validate() {
    if (isAbstract) {
      if (shadow != null || elements != null || rootLocator != null || profiles != null) {
        throw new UtamCompilationError(ERR_ROOT_ABSTRACT);
      }
      return;
    }
    if (isRootPageObject && rootLocator == null) {
      throw new UtamCompilationError(ERR_ROOT_MISSING_SELECTOR);
    }
    if (!isRootPageObject && rootLocator != null) {
      throw new UtamCompilationError(ERR_ROOT_REDUNDANT_SELECTOR);
    }
    if (profiles != null && implementsType == null) {
      throw new UtamCompilationError(ERR_ROOT_PROFILE_HAS_NO_INTERFACE);
    }
    if (profiles == null && implementsType != null) {
      throw new UtamCompilationError(ERR_PROFILE_IS_REQUIRED);
    }
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

  final void compile(TranslationContext context) {
    if (this.isAbstract) {
      context.setAbstract();
    }
    if (implementsType != null) {
      context.setImplementedType(implementsType);
    }
    // register element to prevent names collisions
    ElementContext rootElement = rootElementHelper.setRootElementMethod(context, rootLocator);
    if (elements != null) {
      for (UtamElement nextElement : elements) {
        nextElement.traverse(context, rootElement, false);
      }
    }
    if (shadow != null) {
      for (UtamElement nextElement : shadow.elements) {
        nextElement.traverse(context, rootElement, true);
      }
    }
    // should be before processing methods to ensure unique name
    if (beforeLoad != null) {
      context.setMethod(setBeforeLoadMethod(context));
    }
    if (methods != null) {
      Stream.of(methods).forEach(method -> context.setMethod(method.getMethod(context)));
    }
  }

  private PageObjectMethod setBeforeLoadMethod(TranslationContext context) {
    String name = BEFORE_LOAD_METHOD_NAME;
    MethodContext methodContext = new MethodContext(name, new ReturnType(name));
    List<ComposeMethodStatement> statements = getComposeStatements(context, methodContext,
        beforeLoad);
    List<MethodParameter> methodParameters = methodContext.getMethodParameters();
    if (!methodParameters.isEmpty()) {
      throw new UtamCompilationError(ERR_BEFORE_LOAD_HAS_NO_ARGS);
    }
    return new BeforeLoadMethod(methodContext, statements);
  }

  /**
   * get page object description
   *
   * @return list of strings
   */
  List<String> getDescription() {
    return this.description.getDescription();
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

    RootElementHelper(JsonNode typeNode, boolean isExposeRootElement) {
      this.rootElementType = processBasicTypeNode(typeNode, ROOT_ELEMENT_NAME, true);
      this.isPublic = isExposeRootElement;
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
      context.setElement(rootElement);
      rootElement.setElementMethod(rootElementMethod);
      return rootElement;
    }
  }
}
