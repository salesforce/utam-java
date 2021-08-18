/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.helpers;

import static utam.compiler.helpers.ParameterUtils.EMPTY_PARAMETERS;
import static utam.compiler.helpers.TypeUtilities.BasicElementInterface.actionable;
import static utam.compiler.helpers.TypeUtilities.CONTAINER_ELEMENT;
import static utam.compiler.helpers.TypeUtilities.FRAME_ELEMENT;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import utam.compiler.representation.ElementMethod;
import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.PageObjectMethod;
import utam.core.declarative.representation.TypeProvider;
import utam.core.element.Locator;
import utam.core.element.RootElement;
import utam.core.selenium.element.LocatorBy;

/**
 * helper class to store element context
 *
 * @author elizaveta.ivanova
 * @since 226
 */
public abstract class ElementContext {

  public static final String SELF_ELEMENT_NAME = "self";
  static final String ROOT_ELEMENT_NAME = "root";
  static final String DOCUMENT_ELEMENT_NAME = "document";
  static final Locator EMPTY_SELECTOR = LocatorBy.byCss("");
  public static final TypeProvider ROOT_ELEMENT_TYPE = new TypeUtilities.FromClass(RootElement.class);
  private final Locator selector;
  // parameters from scope + from element itself
  private final List<MethodParameter> parameters;
  private final String name;
  private final TypeProvider type;
  private final boolean isListElement;
  private final boolean isNullable;
  private PageObjectMethod elementGetter;

  ElementContext(
      ElementContext scopeContext,
      String name,
      TypeProvider elementType,
      Locator selector,
      boolean isList, // selector can be list, but element not because of filter
      List<MethodParameter> parameters,
      boolean isNullable) {
    this.name = name;
    this.type = elementType;
    this.selector = selector;
    this.parameters = new ArrayList<>();
    if (scopeContext != null) {
      this.parameters.addAll(scopeContext.parameters);
    }
    this.parameters.addAll(parameters);
    this.isListElement = isList;
    this.isNullable = isNullable;
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

  public final boolean isList() {
    return this.isListElement;
  }

  public boolean isRootElement() {
    return false;
  }

  public boolean isCustomElement() {
    return false;
  }

  public boolean isSelfElement() {
    return false;
  }

  public boolean isDocumentElement() {
    return false;
  }

  public final Locator getSelector() {
    return selector;
  }

  public final boolean isNullable() {
    return isNullable;
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

  public String getElementGetterName() {
    return getElementMethod().getDeclaration().getName();
  }

  public void setElementMethodUsage(TranslationContext translationContext) {
    translationContext.setMethodUsage(getElementGetterName());
  }

  public static class Basic extends ElementContext {

    public Basic(
        ElementContext scopeContext,
        String name,
        TypeProvider elementType,
        Locator selector,
        boolean isList, // selector can be list, but element not because of filter
        List<MethodParameter> parameters,
        boolean isNullable) {
      super(scopeContext, name, elementType, selector, isList, parameters, isNullable);
    }

    // used in tests
    public Basic(String name, TypeProvider elementType, Locator selector) {
      super(null, name, elementType, selector, false, EMPTY_PARAMETERS, false);
    }

    // used in tests
    public Basic(String name, TypeProvider elementType, Locator selector, boolean isList) {
      super(null, name, elementType, selector, isList, EMPTY_PARAMETERS, false);
    }

    // used in tests
    public Basic(String name) {
      this(name, actionable, EMPTY_SELECTOR);
    }
  }

  public static class Container extends ElementContext {

    public Container(ElementContext scopeContext, String name) {
      super(
          scopeContext,
          name,
          CONTAINER_ELEMENT,
          EMPTY_SELECTOR,
          false,
          EMPTY_PARAMETERS,
          false);
    }

    // used in tests
    public Container(String name) {
      this(null, name);
    }
  }

  public static class Frame extends ElementContext {
    public Frame(ElementContext scopeContext, String name, Locator selector) {
      super(
          scopeContext,
          name,
          FRAME_ELEMENT,
          selector,
          false,
          EMPTY_PARAMETERS,
          false);
    }
  }

  public static final class Root extends ElementContext {

    private final TypeProvider enclosingPageObjectType;

    public Root(TypeProvider enclosingPageObjectType, Locator selector) {
      super(null, ROOT_ELEMENT_NAME, ROOT_ELEMENT_TYPE, selector, false, EMPTY_PARAMETERS, false);
      this.enclosingPageObjectType = enclosingPageObjectType;
    }

    // used in tests
    public Root(TypeProvider enclosingPageObjectType) {
      this(enclosingPageObjectType, EMPTY_SELECTOR);
    }

    public TypeProvider getEnclosingPageObjectType() {
      return enclosingPageObjectType;
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
        boolean isList,
        List<MethodParameter> parameters,
        boolean isNullable) {
      super(scopeContext, elementName, type, locator, isList, parameters, isNullable);
    }

    // used in tests
    public Custom(String elementName, TypeProvider type, Locator selector) {
      super(null, elementName, type, selector, false, EMPTY_PARAMETERS, false);
    }

    @Override
    public boolean isCustomElement() {
      return true;
    }
  }

  public static class Document extends ElementContext {

    public static final ElementContext DOCUMENT_ELEMENT = new Document();

    private Document() {
      super(null,
          DOCUMENT_ELEMENT_NAME,
          null,
          EMPTY_SELECTOR,
          false,
          Collections.emptyList(),
          false);
      setElementMethod(ElementMethod.DOCUMENT_GETTER);
    }

    @Override
    public boolean isDocumentElement() {
      return true;
    }
  }

  public static class Self extends ElementContext {

    public static final ElementContext SELF_ELEMENT = new Self();

    private Self() {
      super(null,
          SELF_ELEMENT_NAME,
          null,
          EMPTY_SELECTOR,
          false,
          Collections.emptyList(),
          false);

    }

    @Override
    public boolean isSelfElement() {
      return true;
    }
  }
}
