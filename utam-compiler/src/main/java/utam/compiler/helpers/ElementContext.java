/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.helpers;

import java.util.Collections;
import utam.compiler.helpers.Validation.ErrorType;
import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.PageObjectMethod;
import utam.core.declarative.representation.TypeProvider;
import utam.core.element.Locator;

import java.util.ArrayList;
import java.util.List;
import utam.core.selenium.element.LocatorBy;

import static utam.compiler.helpers.ParameterUtils.EMPTY_PARAMETERS;
import static utam.compiler.helpers.TypeUtilities.Element.actionable;
import static utam.compiler.helpers.Validation.isLabelHardcoded;
import static utam.compiler.helpers.Validation.isSameSelector;

/**
 * helper class to store element context
 *
 * @author elizaveta.ivanova
 * @since 226
 */
public abstract class ElementContext {

  static final String ROOT_ELEMENT_NAME = "root";
  static final String DOCUMENT_ELEMENT_NAME = "document";
  static final Locator EMPTY_SELECTOR = LocatorBy.byCss("");
  private final Locator selector;
  // parameters from scope + from element itself
  private final List<MethodParameter> parameters;
  private final String name;
  private final TypeProvider type;
  private final boolean isListElement;
  private PageObjectMethod elementGetter;

  ElementContext(
      ElementContext scopeContext,
      String name,
      TypeProvider elementType,
      Locator selector,
      boolean isList, // selector can be list, but element not because of filter
      List<MethodParameter> parameters) {
    this.name = name;
    this.type = elementType;
    this.selector = selector;
    this.parameters = new ArrayList<>();
    if (scopeContext != null) {
      this.parameters.addAll(scopeContext.parameters);
    }
    this.parameters.addAll(parameters);
    this.isListElement = isList;
  }

  public final String getName() {
    return name;
  }

  public final TypeProvider getType() {
    return type;
  }

  public final List<MethodParameter> getParameters() {
    return parameters;
  }

  abstract Validation.ErrorType validate(ElementContext element);

  public final boolean isList() {
    return this.isListElement;
  }

  boolean isRootElement() {
    return false;
  }

  public boolean isCustom() {
    return false;
  }

  public boolean isDocumentElement() {
    return false;
  }

  final Locator getSelector() {
    return selector;
  }

  public PageObjectMethod getElementMethod() {
    if (this.elementGetter == null) {
      throw new NullPointerException(
          String.format("element getter is missing for an element '%s'", getName()));
    }
    return this.elementGetter;
  }

  public void setElementMethod(PageObjectMethod method) {
    if (this.elementGetter != null) {
      throw new NullPointerException(
          String.format("element getter already exists for an element '%s'", getName()));
    }
    this.elementGetter = method;
  }

  public static class Basic extends ElementContext {

    public Basic(
        ElementContext scopeContext,
        String name,
        TypeProvider elementType,
        Locator selector,
        boolean isList, // selector can be list, but element not because of filter
        List<MethodParameter> parameters) {
      super(scopeContext, name, elementType, selector, isList, parameters);
    }

    // used in tests
    public Basic(String name, TypeProvider elementType, Locator selector) {
      super(null, name, elementType, selector, false, EMPTY_PARAMETERS);
    }

    // used in tests
    public Basic(String name, TypeProvider elementType, Locator selector, boolean isList) {
      super(null, name, elementType, selector, isList, EMPTY_PARAMETERS);
    }

    // used in tests
    public Basic(String name) {
      this(name, actionable, EMPTY_SELECTOR);
    }

    @Override
    Validation.ErrorType validate(ElementContext element) {
      if (isLabelHardcoded(getSelector())) {
        return Validation.ErrorType.LABEL_HARDCODED;
      }
      if (element.isRootElement() && isSameSelector(getSelector(), element.getSelector())) {
        return Validation.ErrorType.DUPLICATE_WITH_ROOT_SELECTOR;
      }
      if (element.isCustom() && isSameSelector(getSelector(), element.getSelector())) {
        return Validation.ErrorType.COMPONENT_AND_ELEMENT_DUPLICATE_SELECTOR;
      }
      return Validation.ErrorType.NONE;
    }
  }

  public static class Container extends ElementContext {

    public Container(ElementContext scopeContext, String name) {
      super(
          scopeContext,
          name,
          TypeUtilities.CONTAINER_ELEMENT,
          EMPTY_SELECTOR,
          false,
          EMPTY_PARAMETERS);
    }

    Container(String name) {
      this(null, name);
    }

    @Override
    Validation.ErrorType validate(ElementContext element) {
      return Validation.ErrorType.NONE;
    }
  }

  public static final class Root extends ElementContext {

    private final TypeProvider enclosingPageObjectType;

    public Root(
        TypeProvider enclosingPageObjectType, TypeProvider rootElementType, Locator selector) {
      super(null, ROOT_ELEMENT_NAME, rootElementType, selector, false, EMPTY_PARAMETERS);
      this.enclosingPageObjectType = enclosingPageObjectType;
    }

    // used in tests
    public Root(TypeProvider enclosingPageObjectType) {
      this(enclosingPageObjectType, actionable, EMPTY_SELECTOR);
    }

    boolean isSameEnclosingType(ElementContext element) {
      if (element.isCustom() && element.getType().isSameType(this.enclosingPageObjectType)) {
        return true;
      }
      if (element.isRootElement()) {
        return this.enclosingPageObjectType.isSameType(((Root) element).enclosingPageObjectType);
      }
      return false;
    }

    @Override
    Validation.ErrorType validate(ElementContext element) {
      if (isSameEnclosingType(element)) {
        return Validation.ErrorType.NONE;
      }
      if (isLabelHardcoded(getSelector())) {
        return Validation.ErrorType.LABEL_HARDCODED;
      }
      if (isSameSelector(getSelector(), element.getSelector())) {
        return Validation.ErrorType.DUPLICATE_WITH_ROOT_SELECTOR;
      }
      return Validation.ErrorType.NONE;
    }

    @Override
    public boolean isRootElement() {
      return true;
    }
  }

  public static class Custom extends ElementContext {

    public Custom(
        ElementContext scopeContext,
        String elementName,
        TypeProvider type,
        Locator locator,
        boolean isList, List<MethodParameter> parameters) {
      super(scopeContext, elementName, type, locator, isList, parameters);
    }

    // used in tests
    Custom(String elementName, TypeProvider type, Locator selector) {
      super(null, elementName, type, selector, false, EMPTY_PARAMETERS);
    }

    // used in tests
    Custom(String elementName, TypeProvider type, Locator selector, boolean isList) {
      super(null, elementName, type, selector, isList, EMPTY_PARAMETERS);
    }

    @Override
    final Validation.ErrorType validate(ElementContext element) {
      // this statement should be before next because declared root element is also HTML element
      if (element.isRootElement()) {
        if (getType().isSameType(((Root) element).enclosingPageObjectType)) {
          return Validation.ErrorType.NONE;
        }
        if (isLabelHardcoded(getSelector())) {
          return Validation.ErrorType.LABEL_HARDCODED;
        }
        if (isSameSelector(getSelector(), element.getSelector())) {
          return Validation.ErrorType.DUPLICATE_WITH_ROOT_SELECTOR;
        }
        return Validation.ErrorType.NONE;
      }
      if (element instanceof Basic && isSameSelector(getSelector(), element.getSelector())) {
        return Validation.ErrorType.COMPONENT_AND_ELEMENT_DUPLICATE_SELECTOR;
      }
      // if selector same but type is different - it's error
      if (element.isCustom()
          && !getType().isSameType(element.getType())
          && isSameSelector(getSelector(), element.getSelector())) {
        return Validation.ErrorType.COMPONENTS_WITH_SAME_SELECTOR_BUT_DIFFERENT_TYPES;
      }
      return Validation.ErrorType.NONE;
    }

    @Override
    public boolean isCustom() {
      return true;
    }
  }

  public static class Document extends ElementContext {

    public Document() {
      super(null, DOCUMENT_ELEMENT_NAME, null, EMPTY_SELECTOR, false, Collections.EMPTY_LIST);
    }

    @Override
    ErrorType validate(ElementContext element) {
      return ErrorType.NONE;
    }

    @Override
    public boolean isDocumentElement() {
      return true;
    }
  }
}
