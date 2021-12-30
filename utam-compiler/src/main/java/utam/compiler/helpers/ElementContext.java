/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.helpers;

import static utam.compiler.helpers.ParameterUtils.EMPTY_PARAMETERS;
import static utam.compiler.helpers.TypeUtilities.CONTAINER_ELEMENT;
import static utam.compiler.representation.FrameMethod.FRAME_ELEMENT;
import static utam.compiler.helpers.TypeUtilities.wrapAsList;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import utam.compiler.representation.ElementMethod;
import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.PageObjectMethod;
import utam.core.declarative.representation.TypeProvider;
import utam.core.element.Locator;
import utam.core.selenium.element.LocatorBy;

/**
 * helper class to store element context
 *
 * @author elizaveta.ivanova
 * @since 226
 */
public abstract class ElementContext {

  static final String SELF_ELEMENT_NAME = "self";
  public static final String ROOT_ELEMENT_NAME = "root";
  public static final String DOCUMENT_ELEMENT_NAME = "document";
  static final Locator EMPTY_SELECTOR = LocatorBy.byCss("");
  private final Locator selector;
  // parameters from scope + from element itself
  private final List<MethodParameter> parameters;
  private final String name;
  private final TypeProvider type;
  private final boolean isNullable;
  private PageObjectMethod elementGetter;
  private final ElementType elementType;
  private final ElementContext scopeElement;

  ElementContext(
      ElementType elementType,
      ElementContext scopeContext,
      String name,
      TypeProvider type,
      Locator selector,
      List<MethodParameter> parameters,
      boolean isNullable) {
    this.elementType = elementType;
    this.name = name;
    this.type = type;
    this.selector = selector;
    this.parameters = new ArrayList<>();
    if (scopeContext != null) {
      this.parameters.addAll(scopeContext.parameters);
    }
    this.parameters.addAll(parameters);
    this.isNullable = isNullable;
    this.scopeElement = scopeContext;
  }

  public ElementContext getScopeElement() {
    return scopeElement;
  }

  public final String getName() {
    return name;
  }

  public final TypeProvider getType() {
    return type;
  }

  public ElementType getElementNodeType() {
    return elementType;
  }

  // for lists - wrap type into list
  public TypeProvider getGetterReturnType() {
    return getType();
  }

  public final List<MethodParameter> getParameters() {
    return parameters;
  }

  public final Locator getSelector() {
    return selector;
  }

  public final boolean isNullable() {
    return isNullable;
  }

  public final boolean isReturnAll() {
    return this instanceof BasicReturnsAll || this instanceof CustomReturnsAll;
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
        List<MethodParameter> parameters,
        boolean isNullable) {
      super(ElementType.BASIC, scopeContext, name, elementType, selector, parameters, isNullable);
    }

    // used in tests
    public Basic(String name, TypeProvider elementType, Locator selector) {
      this(null, name, elementType, selector, EMPTY_PARAMETERS, false);
    }

    // used in tests
    public Basic(TypeProvider elementType, Locator selector) {
      this(null, "test", elementType, selector, EMPTY_PARAMETERS, false);
    }
  }

  public static class BasicReturnsAll extends Basic {

    public BasicReturnsAll(ElementContext scopeContext, String name,
        TypeProvider elementType, Locator selector,
        List<MethodParameter> parameters, boolean isNullable) {
      super(scopeContext, name, elementType, selector, parameters, isNullable);
    }

    // used in tests
    public BasicReturnsAll(TypeProvider elementType, Locator selector) {
      this(null, "test", elementType, selector, new ArrayList<>(), false);
    }

    @Override
    public TypeProvider getGetterReturnType() {
      return wrapAsList(getType());
    }
  }

  public static class Container extends ElementContext {

    public Container(ElementContext scopeContext, String name) {
      super(
          ElementType.CONTAINER,
          scopeContext,
          name,
          CONTAINER_ELEMENT,
          EMPTY_SELECTOR,
          EMPTY_PARAMETERS,
          false);
    }
  }

  public static class Frame extends ElementContext {

    public Frame(ElementContext scopeContext, String name, LocatorCodeGeneration helper) {
      super(
          ElementType.FRAME,
          scopeContext,
          name,
          FRAME_ELEMENT,
          helper.getLocator(),
          helper.getParameters(),
          false);
    }

    // used in tests
    public Frame(String name, String cssSelector) {
      this(null, name, new LocatorCodeGeneration(cssSelector));
    }
  }

  public static final class Root extends ElementContext {

    private final TypeProvider enclosingPageObjectType;

    public Root(TypeProvider enclosingPageObjectType, Locator selector, TypeProvider rootType) {
      super(ElementType.ROOT, null, ROOT_ELEMENT_NAME, rootType, selector, EMPTY_PARAMETERS, false);
      this.enclosingPageObjectType = enclosingPageObjectType;
    }

    public TypeProvider getEnclosingPageObjectType() {
      return enclosingPageObjectType;
    }
  }

  public static class CustomReturnsAll extends Custom {

    public CustomReturnsAll(ElementContext scopeContext, String elementName,
        TypeProvider type, Locator locator,
        List<MethodParameter> parameters, boolean isNullable) {
      super(scopeContext, elementName, type, locator, parameters, isNullable);
    }

    @Override
    public TypeProvider getGetterReturnType() {
      return wrapAsList(getType());
    }
  }

  public static class Custom extends ElementContext {

    public Custom(
        ElementContext scopeContext,
        String elementName,
        TypeProvider type,
        Locator locator,
        List<MethodParameter> parameters,
        boolean isNullable) {
      super(ElementType.CUSTOM, scopeContext, elementName, type, locator, parameters, isNullable);
    }

    // used in tests
    public Custom(String elementName, TypeProvider type, Locator selector) {
      this(null, elementName, type, selector, EMPTY_PARAMETERS, false);
    }
  }

  public static class Document extends ElementContext {

    public static final ElementContext DOCUMENT_ELEMENT = new Document();

    private Document() {
      super(ElementType.DOCUMENT,
          null,
          DOCUMENT_ELEMENT_NAME,
          null,
          EMPTY_SELECTOR,
          Collections.emptyList(),
          false);
      setElementMethod(ElementMethod.DOCUMENT_GETTER);
    }
  }

  public static class Self extends ElementContext {

    public static final ElementContext SELF_ELEMENT = new Self();

    private Self() {
      super(ElementType.SELF,
          null,
          SELF_ELEMENT_NAME,
          null,
          EMPTY_SELECTOR,
          Collections.emptyList(),
          false);

    }
  }

  public enum ElementType {
    BASIC,
    CUSTOM,
    CONTAINER,
    FRAME,
    ROOT,
    SELF,
    DOCUMENT
  }
}
