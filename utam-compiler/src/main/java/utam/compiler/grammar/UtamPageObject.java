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
import static utam.compiler.helpers.AnnotationUtils.getPageObjectAnnotation;
import static utam.compiler.helpers.AnnotationUtils.getPagePlatformAnnotation;
import static utam.compiler.helpers.AnnotationUtils.getShadowHostAnnotation;
import static utam.compiler.helpers.ElementContext.DOCUMENT_ELEMENT_NAME;
import static utam.compiler.helpers.ElementContext.ROOT_ELEMENT_NAME;
import static utam.compiler.helpers.MethodContext.BEFORE_LOAD_METHOD_NAME;
import static utam.compiler.helpers.TypeUtilities.PAGE_OBJECT;
import static utam.compiler.helpers.TypeUtilities.ROOT_ELEMENT_TYPE;
import static utam.compiler.helpers.TypeUtilities.ROOT_PAGE_OBJECT;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import utam.compiler.UtamCompilationError;
import utam.compiler.helpers.BasicElementUnionType;
import utam.compiler.helpers.ElementContext;
import utam.compiler.helpers.MethodContext;
import utam.compiler.helpers.ReturnType;
import utam.compiler.helpers.TranslationContext;
import utam.compiler.representation.BeforeLoadMethod;
import utam.compiler.representation.ComposeMethodStatement;
import utam.compiler.representation.RootElementMethod;
import utam.core.declarative.representation.AnnotationProvider;
import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.PageObjectMethod;
import utam.core.declarative.representation.TypeProvider;
import utam.core.element.Locator;
import utam.core.framework.consumer.UtamError;

/**
 * mapping for a Page Object JSON
 *
 * @author elizaveta.ivanova
 * @since 228
 */
final class UtamPageObject {

  static final String ERR_ROOT_PROFILE_HAS_NO_INTERFACE =
      "profile can only be set for a page object that implements an interface";
  static final String ERR_ROOT_MISSING_SELECTOR =
      "root page object requires default selector property";
  static final String ERR_ROOT_REDUNDANT_SELECTOR = "non root page object can't have selector";
  static final String ERR_ROOT_ABSTRACT = "interface declaration can only have 'methods' property";
  static final String ERR_UNSUPPORTED_ROOT_ELEMENT_TYPE = "type '%s' is not supported for root element";
  static final String ERR_DISALLOWED_ELEMENT = "Only self, document or root element allowed in beforeLoad method";
  private static final Set<String> BEFORE_LOAD_ELEMENTS = Stream
      .of(DOCUMENT_ELEMENT_NAME, ROOT_ELEMENT_NAME).collect(
          Collectors.toSet());
  final String implementsType;
  final Locator rootLocator;
  private final UtamMethodAction[] beforeLoad;
  boolean isAbstract;
  boolean isRootPageObject;
  UtamMethod[] methods;
  String platform;
  UtamProfile[] profiles;
  UtamShadowElement shadow;
  String[] rootElementType;
  boolean isExposeRootElement;
  UtamElement[] elements;

  @JsonCreator
  UtamPageObject(
      @JsonProperty(value = "implements") String implementsType,
      @JsonProperty(value = "interface", defaultValue = "false") boolean isAbstract,
      @JsonProperty(value = "platform") String platform,
      @JsonProperty(value = "profile") UtamProfile[] profiles,
      // to expose root element
      @JsonProperty(value = "type", defaultValue = "[]") String[] type,
      @JsonProperty(value = "exposeRootElement", defaultValue = "false") boolean isExposeRootElement,
      // root selector
      @JsonProperty(value = "root", defaultValue = "false") boolean isRootPageObject,
      @JsonProperty(value = "selector") UtamRootSelector selector,
      // nested nodes
      @JsonProperty("shadow") UtamShadowElement shadow,
      @JsonProperty("elements") UtamElement[] elements,
      @JsonProperty("methods") UtamMethod[] methods,
      @JsonProperty("beforeLoad") UtamMethodAction[] beforeLoad) {
    this.profiles = profiles;
    this.methods = methods;
    this.isAbstract = isAbstract;
    this.platform = platform;
    this.isRootPageObject = isRootPageObject;
    this.implementsType = implementsType;
    this.isExposeRootElement = isExposeRootElement;
    this.shadow = shadow;
    this.elements = elements;
    this.rootElementType = type;
    this.beforeLoad = beforeLoad;
    if (selector == null) {
      this.rootLocator = null;
    } else {
      this.rootLocator = selector.getLocator();
    }
    validate();
  }

  // used in tests
  UtamPageObject(boolean isRoot, UtamSelector selector) {
    this(null, false, null, null, null, false, isRoot, selector, null, null, null, null);
  }

  // used in tests
  UtamPageObject() {
    this(null, false, null, null, null, false, false, null, null, null, null, null);
  }

  void validate() {
    if (isAbstract) {
      if (shadow != null || elements != null || rootLocator != null || profiles != null) {
        throw new UtamError(ERR_ROOT_ABSTRACT);
      }
      return;
    }
    if (isRootPageObject && rootLocator == null) {
      throw new UtamError(ERR_ROOT_MISSING_SELECTOR);
    }
    if (!isRootPageObject && rootLocator != null) {
      throw new UtamError(ERR_ROOT_REDUNDANT_SELECTOR);
    }
    if (profiles != null && implementsType == null) {
      throw new UtamError(ERR_ROOT_PROFILE_HAS_NO_INTERFACE);
    }
    // check that root element type is one of actionables
    if (rootElementType != null && !BasicElementUnionType.isBasicType(rootElementType)) {
      throw new UtamError(
          String.format(ERR_UNSUPPORTED_ROOT_ELEMENT_TYPE, Arrays.toString(rootElementType)));
    }
  }

  List<AnnotationProvider> getAnnotations() {
    List<AnnotationProvider> annotations = new ArrayList<>();
    if (rootLocator != null) {
      annotations.add(getPageObjectAnnotation(rootLocator));
    }
    if (shadow != null) {
      annotations.add(getShadowHostAnnotation(true));
    }
    if (platform != null) {
      annotations.add(getPagePlatformAnnotation(platform));
    }
    return annotations;
  }

  TypeProvider getBaseType() {
    return isRootPageObject ? ROOT_PAGE_OBJECT : PAGE_OBJECT;
  }

  private ElementContext setRootElementMethod(TranslationContext context) {
    TypeProvider interfaceType = context.getSelfType();
    // TODO: Fix root elements to create correct base type from component interfaces
    TypeProvider elementType = ROOT_ELEMENT_TYPE;
    ElementContext rootElement = new ElementContext.Root(interfaceType, rootLocator);
    // register root element and its method in context
    context.setElement(rootElement);
    PageObjectMethod rootElementMethod;
    if (this.isExposeRootElement) {
      // if "exposeRootElement" is set - declare public method
      rootElementMethod = new RootElementMethod.Public(elementType);
      context.setMethod(rootElementMethod);
    } else if (rootElementType != null && BasicElementUnionType.isBasicType(rootElementType)) {
      // if type for root element is set and it consists of all basic type interfaces,
      // declare private method to typecast because BasePageObject.getRootElement
      // returns RootElement
      rootElementMethod = new RootElementMethod.Private(elementType);
      context.setMethod(rootElementMethod);
    } else {
      // BasePageObject.getRootElement should be registered in context to call from compose
      rootElementMethod = new RootElementMethod.Protected();
    }
    rootElement.setElementMethod(rootElementMethod);
    return rootElement;
  }

  final void compile(TranslationContext context) {
    if (this.isAbstract) {
      context.setAbstract();
    }
    if (implementsType != null) {
      context.setImplementedType(implementsType);
    }
    // register element to prevent names collisions
    ElementContext rootElement = setRootElementMethod(context);
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
    for (UtamMethodAction action : beforeLoad) {
      String elementName = action.elementName;
      if (elementName != null && !BEFORE_LOAD_ELEMENTS.contains(elementName)) {
        throw new UtamCompilationError(ERR_DISALLOWED_ELEMENT);
      }
    }
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
}
