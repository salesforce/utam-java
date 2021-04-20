/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import utam.compiler.helpers.ElementContext;
import utam.compiler.helpers.ElementContext.Document;
import utam.compiler.helpers.TranslationContext;
import utam.compiler.helpers.TypeUtilities;
import utam.core.element.Locator;
import utam.core.framework.consumer.UtamError;
import utam.core.framework.context.Profile;
import utam.compiler.representation.RootElementMethod;
import utam.core.declarative.representation.AnnotationProvider;
import utam.core.declarative.representation.PageObjectMethod;
import utam.core.declarative.representation.TypeProvider;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Stream;

import static utam.compiler.helpers.AnnotationUtils.*;
import static utam.compiler.helpers.TypeUtilities.*;
import static utam.compiler.helpers.TypeUtilities.Element.actionable;

/**
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
  static final String BEFORELOAD_METHOD_MANE = "load";
  boolean isAbstract;
  boolean isRootPageObject;
  UtamMethod[] methods;
  String platform;
  UtamProfile[] profiles;
  String implementsType;
  final String comments = "";
  UtamShadowElement shadow;
  String rootElementType;
  boolean isExposeRootElement; // should be nullable as it's redundant for root
  UtamElement[] elements;
  UtamMethod beforeLoad;
  final Locator rootLocator;

  @JsonCreator
  UtamPageObject(
      @JsonProperty(value = "implements") String implementsType,
      @JsonProperty(value = "interface", defaultValue = "false") boolean isAbstract,
      @JsonProperty(value = "platform") String platform,
      @JsonProperty(value = "profile") UtamProfile[] profiles,
      // to expose root element
      @JsonProperty("type") String type,
      @JsonProperty(value = "exposeRootElement", defaultValue = "false") boolean isExposeRootElement,
      // root selector
      @JsonProperty(value = "root", defaultValue = "false") boolean isRootPageObject,
      @JsonProperty(value = "selector") UtamSelector selector,
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
    if (beforeLoad != null) {
      this.beforeLoad =
          new UtamMethod(BEFORELOAD_METHOD_MANE, beforeLoad, null, null, null, false);
    }

    if(selector == null) {
      this.rootLocator = null;
    } else {
      selector.validateRootSelector();
      this.rootLocator = selector.getContext().getLocator();
    }
    validate();
  }

  // used in tests
  UtamPageObject(boolean isRoot, UtamSelector selector) {
    this(null, false, null, null, null, false, isRoot, selector, null, null, null, null);
  }

  // used in tests
  UtamPageObject() {
    this.profiles = null;
    this.methods = null;
    this.isAbstract = false;
    this.platform = null;
    this.isRootPageObject = false;
    this.implementsType = null;
    this.rootLocator = null;
    this.shadow = null;
    this.elements = null;
    this.isExposeRootElement = false;
    this.rootElementType = null;
    this.beforeLoad = null;
  }

  void validate() {
    if (isAbstract) {
      if (isExposeRootElement
          || rootElementType != null || shadow != null || elements != null || rootLocator != null || profiles != null) {
        throw new UtamError(ERR_ROOT_ABSTRACT);
      }
      return;
    }
    if(isRootPageObject && rootLocator == null) {
      throw new UtamError(ERR_ROOT_MISSING_SELECTOR);
    }
    if(!isRootPageObject && rootLocator != null) {
      throw new UtamError(ERR_ROOT_REDUNDANT_SELECTOR);
    }
    if (profiles != null && implementsType == null) {
      throw new UtamError(ERR_ROOT_PROFILE_HAS_NO_INTERFACE);
    }
    // check that root element type is one of actionables
    if(rootElementType != null && !TypeUtilities.Element.isBasicType(rootElementType)) {
      throw new UtamError(String.format(ERR_UNSUPPORTED_ROOT_ELEMENT_TYPE, rootElementType));
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

  Profile[] getProfiles(TranslationContext context) {
    if (this.profiles == null) {
      return new Profile[0];
    }
    return Stream.of(profiles)
        .map(profile -> profile.getProfile(context))
        .toArray(Profile[]::new);
  }

  private ElementContext setRootElementMethod(TranslationContext context) {
    TypeProvider interfaceType = context.getInterfaceType(implementsType);
    TypeProvider elementType = TypeUtilities.Element.asBasicType(rootElementType);
    ElementContext rootElement = new ElementContext.Root(interfaceType, elementType, rootLocator);
    // register root element and its method in context
    context.setElement(rootElement);
    PageObjectMethod rootElementMethod;
    // if "exposeRootElement" is set - declare public method
    if (this.isExposeRootElement) {
      rootElementMethod = new RootElementMethod.Public(elementType);
      context.setMethod(rootElementMethod);
    }
    // if type for root element is set and it's not actionable
    // declare private method to typecast because BasePageObject.getRootElement returns actionable
    else if(rootElementType != null && !rootElementType.equals(actionable.name())) {
      rootElementMethod = new RootElementMethod.Private(elementType);
      context.setMethod(rootElementMethod);
    }
    // BasePageObject.getRootElement should be registered in context to call from compose
    else {
      rootElementMethod = new RootElementMethod.Protected();
    }
    rootElement.setElementMethod(rootElementMethod);
    return rootElement;
  }

  final void compile(TranslationContext context) {
    if(this.isAbstract) {
      context.setAbstract();
    }
    // register element to prevent names collisions
    context.setElement(new Document());
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
    if (methods != null) {
      Stream.of(methods).forEach(method -> context.setMethod(method.getMethod(context)));
    }
    if (beforeLoad != null) {
      context.setBeforeLoad(true);
      context.setMethod(beforeLoad.getBeforeLoadMethod(context));
    }
  }
}
