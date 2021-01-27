package utam.compiler.grammar;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import utam.compiler.helpers.ElementContext;
import utam.compiler.helpers.TranslationContext;
import utam.compiler.helpers.TypeUtilities;
import declarative.representation.*;
import framework.consumer.UtamError;
import framework.context.Profile;
import selenium.element.Selector;
import utam.compiler.representation.RootElementMethod;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Stream;

import static utam.compiler.helpers.AnnotationUtils.*;
import static utam.compiler.helpers.ElementContext.EMPTY_SELECTOR;
import static utam.compiler.helpers.TypeUtilities.Element.actionable;
import static utam.compiler.helpers.TypeUtilities.PAGE_OBJECT;
import static utam.compiler.helpers.TypeUtilities.ROOT_PAGE_OBJECT;

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
  static final String ERR_ROOT_CANT_USE_DEFAULT_PROFILE =
      "page object can't use default profile";
  static final String ERR_UNSUPPORTED_ROOT_ELEMENT_TYPE = "type '%s' is not supported for root element";
  boolean isAbstract;
  boolean isRootPageObject;
  UtamMethod[] methods;
  String platform;
  UtamProfile[] profiles;
  String implementsType;
  final String comments = "";
  UtamSelector selector;
  UtamShadowElement shadow;
  String rootElementType;
  Boolean isExposeRootElement; // should be nullable as it's redundant for root
  UtamElement[] elements;

  @JsonCreator
  UtamPageObject(
      @JsonProperty(value = "implements") String implementsType,
      @JsonProperty(value = "interface", defaultValue = "false") boolean isAbstract,
      @JsonProperty(value = "platform") String platform,
      @JsonProperty(value = "profile") UtamProfile[] profiles,
      // to expose root element
      @JsonProperty("type") String type,
      @JsonProperty(value = "exposeRootElement", defaultValue = "null") Boolean isExposeRootElement,
      // root selector
      @JsonProperty(value = "root", defaultValue = "false") boolean isRootPageObject,
      @JsonProperty(value = "selector") UtamSelector selector,
      // nested nodes
      @JsonProperty("shadow") UtamShadowElement shadow,
      @JsonProperty("elements") UtamElement[] elements,
      @JsonProperty("methods") UtamMethod[] methods) {
    this.profiles = profiles;
    this.methods = methods;
    this.isAbstract = isAbstract;
    this.platform = platform;
    this.isRootPageObject = isRootPageObject;
    this.implementsType = implementsType;
    this.isExposeRootElement = isExposeRootElement;
    this.selector = selector;
    this.shadow = shadow;
    this.elements = elements;
    this.rootElementType = type;
    validate();
  }

  // used in tests
  UtamPageObject() {
    this.profiles = null;
    this.methods = null;
    this.isAbstract = false;
    this.platform = null;
    this.isRootPageObject = false;
    this.implementsType = null;
    this.selector = null;
    this.shadow = null;
    this.elements = null;
    this.isExposeRootElement = false;
    this.rootElementType = null;
  }

  void validate() {
    if (isAbstract) {
      if (isExposeRootElement != null || rootElementType != null || shadow != null || elements != null || selector != null || profiles != null) {
        throw new UtamError(ERR_ROOT_ABSTRACT);
      }
      return;
    }
    if (isRootPageObject && selector == null) {
      throw new UtamError(ERR_ROOT_MISSING_SELECTOR);
    }
    if (!isRootPageObject && selector != null) {
      throw new UtamError(ERR_ROOT_REDUNDANT_SELECTOR);
    }
    if (selector != null) {
      selector.validateRootSelector();
    }
    if (profiles != null && implementsType == null) {
      throw new UtamError(ERR_ROOT_PROFILE_HAS_NO_INTERFACE);
    }
    // check that root element type is one of actionables
    if(TypeUtilities.getElementType(rootElementType, actionable) == null) {
      throw new UtamError(String.format(ERR_UNSUPPORTED_ROOT_ELEMENT_TYPE, rootElementType));
    }
  }

  List<AnnotationProvider> getAnnotations() {
    List<AnnotationProvider> annotations = new ArrayList<>();
    if (selector != null) {
      annotations.add(getPageObjectAnnotation(selector.getSelector()));
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
        .peek(
            profile -> {
              if (profile.isDefault()) {
                throw new UtamError(ERR_ROOT_CANT_USE_DEFAULT_PROFILE);
              }
            })
        .toArray(Profile[]::new);
  }

  final ElementContext setRootElementMethod(TranslationContext context) {
    TypeProvider interfaceType = context.getInterfaceType(implementsType);
    Selector rootSelector = selector != null ? selector.getSelector() : EMPTY_SELECTOR;
    TypeProvider elementType = TypeUtilities.getElementType(rootElementType, actionable);
    ElementContext rootElement = new ElementContext.Root(interfaceType, elementType, rootSelector);
    // register root element and its method in context
    context.setElement(rootElement);
    PageObjectMethod rootElementMethod;
    // if "exposeRootElement" is set - declare public method
    if (Boolean.TRUE.equals(this.isExposeRootElement)) {
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
  }
}
