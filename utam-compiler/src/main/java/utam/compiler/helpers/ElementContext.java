/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.helpers;

import static utam.compiler.helpers.ParameterUtils.getParametersValuesString;
import static utam.compiler.helpers.PrimitiveType.NUMBER;
import static utam.compiler.helpers.TypeUtilities.BASIC_ELEMENT;
import static utam.compiler.helpers.TypeUtilities.CONTAINER_ELEMENT;
import static utam.compiler.helpers.TypeUtilities.FRAME_ELEMENT;
import static utam.compiler.helpers.TypeUtilities.LIST_TYPE;
import static utam.compiler.helpers.TypeUtilities.wrapAsList;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import utam.compiler.grammar.UtamMethodDescription;
import utam.compiler.helpers.ParameterUtils.Regular;
import utam.compiler.representation.ElementMethod;
import utam.compiler.representation.JavadocObject;
import utam.compiler.representation.JavadocObject.MethodJavadoc;
import utam.compiler.representation.MethodDeclarationImpl;
import utam.core.declarative.representation.MethodDeclaration;
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

  /** The name of the root element */
  public static final String ROOT_ELEMENT_NAME = "root";

  /** The name of the document object */
  public static final String DOCUMENT_ELEMENT_NAME = "document";

  /** The name of the navigation object */
  static final String NAVIGATION_OBJECT_NAME = "navigation";

  static final String SELF_ELEMENT_NAME = "self";
  static final Locator EMPTY_SELECTOR = LocatorBy.byCss("");
  private final Locator selector;
  // parameters from scope + from element itself
  private final List<MethodParameter> parameters = new ArrayList<>();
  // parameters to be used for scoping inside list, including index
  private final List<MethodParameter> parametersForNested = new ArrayList<>();
  private final String name;
  private final TypeProvider type;
  private final boolean isNullable;
  private final ElementType elementType;
  private final ElementContext scopeElement;
  private PageObjectMethod elementGetter;

  /**
   * Initializes a new instance of the ElementContext class
   *
   * @param elementType the kind of element (basic, custom, container, etc.)
   * @param scopeContext the element context containing the scope
   * @param name the name of the element
   * @param type the type of the element
   * @param selector the selector for the element
   * @param parameters the parameters to use to find the element
   * @param isNullable a value indicating whether the element is nullable
   */
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
    if (scopeContext != null) {
      this.parameters.addAll(scopeContext.getParametersForNestedElements());
      this.parametersForNested.addAll(scopeContext.getParametersForNestedElements());
    }
    this.parameters.addAll(parameters);
    this.parametersForNested.addAll(parameters);
    this.isNullable = isNullable;
    this.scopeElement = scopeContext;
  }

  /**
   * Gets the scope element
   *
   * @return the element context of the scope element
   */
  public ElementContext getScopeElement() {
    return scopeElement;
  }

  /**
   * Gets the name of the element
   *
   * @return the element name
   */
  public final String getName() {
    return name;
  }

  /**
   * Gets the type of the element
   *
   * @return the TypeProvider of the element
   */
  public final TypeProvider getType() {
    return type;
  }

  /**
   * Gets the node type of the element (basic, custom, container, frame, etc.)
   *
   * @return the node type of the element
   */
  public ElementType getElementNodeType() {
    return elementType;
  }

  /**
   * Gets the getter return type, wrapping for lists as necessary
   *
   * @return the return type of the element getter
   */
  public TypeProvider getGetterReturnType() {
    return getType();
  }

  /**
   * Gets the parameters for the element
   *
   * @return the list of parameters for the element getter
   */
  public final List<MethodParameter> getParameters() {
    return parameters;
  }

  /**
   * Parameters for nested elements can include index element. Same parameters can't be used inside
   * compose methods
   *
   * @return the list of parameters for a nested element getter
   */
  public final List<MethodParameter> getParametersForNestedElements() {
    return parametersForNested;
  }

  /**
   * Getter can have literal parameter that is HARDCODED in the element. When we invoke getter it
   * should not be used
   *
   * @return list of non-literal parameters
   */
  public final List<MethodParameter> getGetterNonLiteralParameters() {
    return getElementMethod().getDeclaration().getParameters().stream()
        .filter(parameter -> !parameter.isLiteral())
        .collect(Collectors.toList());
  }

  public void setIndexMethod(boolean hasNestedElements, TranslationContext context) {
    // does nothing except for basic list
  }

  /**
   * Gets the selector of the locator
   *
   * @return the locator object containing the selector
   */
  public final Locator getSelector() {
    return selector;
  }

  /**
   * Gets a value indicating whether the element is nullable
   *
   * @return true if the element is nullable; otherwise false
   */
  public final boolean isNullable() {
    return isNullable;
  }

  /**
   * Gets a value indicating whether the element returns all elements matching the selector
   *
   * @return true if the element returns all elements matching the selector; otherwise, false
   */
  public final boolean isReturnAll() {
    return this instanceof BasicReturnsAll || this instanceof CustomReturnsAll;
  }

  /**
   * Gets the element getter method
   *
   * @return the element getter method
   */
  public PageObjectMethod getElementMethod() {
    if (this.elementGetter == null) {
      throw new NullPointerException(
          String.format("element getter is missing for an element '%s'", getName()));
    }
    return this.elementGetter;
  }

  /**
   * Get method to invoke in skope, for list it's index
   *
   * @return method to be used as scope
   */
  public PageObjectMethod getElementScopeMethod() {
    return this.elementGetter;
  }

  /**
   * Register element getter method, throw NPE if method already exists
   *
   * @param method the method to set as the element getter
   */
  void registerElementMethod(PageObjectMethod method) {
    if (this.elementGetter != null) {
      throw new NullPointerException(
          String.format("element getter already exists for an element '%s'", getName()));
    }
    this.elementGetter = method;
  }

  /**
   * Traverse scope elements and for each scope element in hierarchy set that it's used by its child
   * getter. Private methods that are not public and not marked as "used" will not be added to
   * generation
   *
   * @param context translation context
   */
  private void setUsageOfScopeElement(TranslationContext context) {
    ElementContext scopeElement = this.getScopeElement();
    while (scopeElement != null) {
      // index getter uses regular getter, so both should be registered
      String getterName = scopeElement.getElementGetterName();
      String indexGetterName = scopeElement.getElementScopeMethod().getDeclaration().getName();
      context.setMethodUsage(getterName);
      context.setMethodUsage(indexGetterName);
      scopeElement = scopeElement.getScopeElement();
    }
  }

  /**
   * Register element getter method, throw NPE if method already exists. Then traverse scope
   * elements and set their usage.
   *
   * @param method the method to set as the element getter
   * @param context context is used to set scope element method usage
   */
  public void setElementMethod(PageObjectMethod method, TranslationContext context) {
    registerElementMethod(method);
    setUsageOfScopeElement(context);
  }

  /**
   * Gets the name of the element getter method
   *
   * @return the name of the element getter method
   */
  public String getElementGetterName() {
    return getElementMethod().getDeclaration().getName();
  }

  /** The node types of elements */
  public enum ElementType {
    /** A basic element */
    BASIC,

    /** An element that represents another Page Object */
    CUSTOM,

    /** A container element */
    CONTAINER,

    /** An element representing a frame or iframe */
    FRAME,

    /** An element representing the root element */
    ROOT,

    /** An element representing itself */
    SELF,

    /** An element representing the enclosing document */
    DOCUMENT,

    /** An object for access to driver navigation */
    NAVIGATION
  }

  /** Represents a basic element (on of actionable group) */
  public static class Basic extends ElementContext {

    /**
     * Initializes a new instance of the Basic class
     *
     * @param scopeContext the element context containing the scope
     * @param name the name of the element
     * @param elementType the type of the element
     * @param selector the selector for the element
     * @param parameters the parameters to use to find the element
     * @param isNullable a value indicating whether the element is nullable
     */
    public Basic(
        ElementContext scopeContext,
        String name,
        TypeProvider elementType,
        Locator selector,
        List<MethodParameter> parameters,
        boolean isNullable) {
      super(ElementType.BASIC, scopeContext, name, elementType, selector, parameters, isNullable);
    }

    /**
     * Initializes a new instance of the Basic class, used only in unit tests
     *
     * @param name the name of the element
     * @param elementType the type of the element
     * @param selector the selector for the element
     */
    public Basic(String name, TypeProvider elementType, Locator selector) {
      this(null, name, elementType, selector, new ArrayList<>(), false);
    }
  }

  /** Represents a list of basic elements (on of actionable group) */
  public static class BasicReturnsAll extends Basic {

    /** For lists scope getter is different */
    private PageObjectMethod elementIndexGetter;

    /**
     * Initializes a new instance of the BasicReturnsAll class
     *
     * @param scopeContext the element context containing the scope
     * @param name the name of the element
     * @param elementType the type of the element
     * @param selector the selector for the element
     * @param parameters the parameters to use to find the element
     * @param isNullable a value indicating whether the element is nullable
     */
    public BasicReturnsAll(
        ElementContext scopeContext,
        String name,
        TypeProvider elementType,
        Locator selector,
        List<MethodParameter> parameters,
        boolean isNullable) {
      super(scopeContext, name, elementType, selector, parameters, isNullable);
    }

    @Override
    public TypeProvider getGetterReturnType() {
      return wrapAsList(getType());
    }

    @Override
    public PageObjectMethod getElementScopeMethod() {
      if (this.elementIndexGetter != null) {
        return this.elementIndexGetter;
      }
      return super.getElementScopeMethod();
    }

    @Override
    public void setIndexMethod(boolean hasNestedElements, TranslationContext context) {
      if (hasNestedElements) {
        MethodParameter indexParameter =
            new Regular(String.format("_%sIndex", getName()), NUMBER, "index of parent element");
        this.getParametersForNestedElements().add(indexParameter);
        this.elementIndexGetter = buildIndexMethod(indexParameter);
        context.setMethod(this.elementIndexGetter);
      }
    }

    private PageObjectMethod buildIndexMethod(MethodParameter indexParameter) {
      String methodName = String.format("_index_%s", getElementGetterName());
      List<MethodParameter> parameters = new ArrayList<>(getParameters());
      parameters.add(indexParameter);
      String scopeVariableName = getName();
      JavadocObject javadoc =
          new MethodJavadoc(
              methodName,
              BASIC_ELEMENT,
              parameters,
              new UtamMethodDescription(
                  String.format("Get Nth element for \"%s\" list of elements", getName())));
      // imports can be empty
      MethodDeclaration declaration =
          new MethodDeclarationImpl(
              methodName, parameters, BASIC_ELEMENT, new ArrayList<>(), javadoc);
      return new PageObjectMethod() {
        @Override
        public MethodDeclaration getDeclaration() {
          return declaration;
        }

        @Override
        public List<String> getCodeLines() {
          List<String> res = new ArrayList<>();
          res.add(
              String.format(
                  "List<%s> %s = this.%s(%s)",
                  BASIC_ELEMENT.getSimpleName(),
                  scopeVariableName,
                  getElementGetterName(),
                  getParametersValuesString(getParameters())));
          res.add(
              String.format(
                  "if (%s.size() < %s-1) {", scopeVariableName, indexParameter.getValue()));
          res.add(
              String.format(
                  "throw new RuntimeException(\"Can't find scope element '%s' with given index!\")",
                  getName()));
          res.add("}");
          res.add(String.format("return %s.get(%s)", scopeVariableName, indexParameter.getValue()));
          return res;
        }

        @Override
        public List<TypeProvider> getClassImports() {
          return List.of(BASIC_ELEMENT, LIST_TYPE);
        }

        @Override
        public boolean isPublic() {
          return false;
        }
      };
    }
  }

  /** Represents a container element ("type" : "container") */
  public static class Container extends ElementContext {

    /**
     * Initializes a new instance of the Container class
     *
     * @param scopeContext the element context containing the scope
     * @param name the name of the element
     */
    public Container(ElementContext scopeContext, String name) {
      super(
          ElementType.CONTAINER,
          scopeContext,
          name,
          CONTAINER_ELEMENT,
          EMPTY_SELECTOR,
          new ArrayList<>(),
          false);
    }
  }

  /** Represents a frame element ("type" : "frame") */
  public static class Frame extends ElementContext {

    /**
     * Initializes a new instance of the Frame class
     *
     * @param scopeContext the element context containing the scope
     * @param name the name of the element
     * @param helper the helper for generating the locator code
     */
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
  }

  /** Represents a root element */
  public static final class Root extends ElementContext {

    private final TypeProvider enclosingPageObjectType;

    /**
     * Initializes a new instance of the Root class
     *
     * @param enclosingPageObjectType the type of the enclosing Page Object
     * @param selector the selector for the element
     * @param rootType the type of the root element
     * @param rootElementMethod element method to register
     */
    public Root(
        TypeProvider enclosingPageObjectType,
        Locator selector,
        TypeProvider rootType,
        PageObjectMethod rootElementMethod) {
      super(
          ElementType.ROOT, null, ROOT_ELEMENT_NAME, rootType, selector, new ArrayList<>(), false);
      registerElementMethod(rootElementMethod);
      this.enclosingPageObjectType = enclosingPageObjectType;
    }

    /**
     * Gets the type of the enclosing Page Object
     *
     * @return the type of the enclosing Page Object
     */
    public TypeProvider getEnclosingPageObjectType() {
      return enclosingPageObjectType;
    }
  }

  /** Represents a list of "custom" elements: other Page Objects */
  public static class CustomReturnsAll extends Custom {

    /**
     * Initializes a new instance of the CustomReturnsAll class
     *
     * @param scopeContext the element context containing the scope
     * @param elementName the name of the element
     * @param type the type of the element
     * @param locator the selector for the element
     * @param parameters the parameters to use to find the element
     * @param isNullable a value indicating whether the element is nullable
     */
    public CustomReturnsAll(
        ElementContext scopeContext,
        String elementName,
        TypeProvider type,
        Locator locator,
        List<MethodParameter> parameters,
        boolean isNullable) {
      super(scopeContext, elementName, type, locator, parameters, isNullable);
    }

    @Override
    public TypeProvider getGetterReturnType() {
      return wrapAsList(getType());
    }
  }

  /** Represents a "custom" element: another Page Object */
  public static class Custom extends ElementContext {

    /**
     * Initializes a new instance of the Custom class
     *
     * @param scopeContext the element context containing the scope
     * @param elementName the name of the element
     * @param type the type of the element
     * @param locator the selector for the element
     * @param parameters the parameters to use to find the element
     * @param isNullable a value indicating whether the element is nullable
     */
    public Custom(
        ElementContext scopeContext,
        String elementName,
        TypeProvider type,
        Locator locator,
        List<MethodParameter> parameters,
        boolean isNullable) {
      super(ElementType.CUSTOM, scopeContext, elementName, type, locator, parameters, isNullable);
    }

    /**
     * Initializes a new instance of the Custom class, used only in unit tests
     *
     * @param elementName the name of the element
     * @param type the type of the element
     * @param selector the selector for the element
     */
    Custom(String elementName, TypeProvider type, Locator selector) {
      this(null, elementName, type, selector, new ArrayList<>(), false);
    }
  }

  /** Represents the Document */
  public static class Document extends ElementContext {

    /** The document element context */
    public static final ElementContext DOCUMENT_ELEMENT = new Document();

    /** Initializes a new instance of the Document class */
    private Document() {
      super(
          ElementType.DOCUMENT,
          null,
          DOCUMENT_ELEMENT_NAME,
          null,
          EMPTY_SELECTOR,
          Collections.emptyList(),
          false);
      registerElementMethod(ElementMethod.DOCUMENT_GETTER);
    }
  }

  /** Represents self element (or "this") */
  static class Self extends ElementContext {

    /** The self element context */
    static final ElementContext SELF_ELEMENT = new Self();

    /** Initializes a new instance of the Self class */
    private Self() {
      super(
          ElementType.SELF,
          null,
          SELF_ELEMENT_NAME,
          null,
          EMPTY_SELECTOR,
          Collections.emptyList(),
          false);
    }
  }

  /**
   * Represents the navigation object
   *
   * @author elizaveta.ivanova
   * @since 242
   */
  public static class Navigation extends ElementContext {

    /** The document element context */
    public static final ElementContext NAVIGATION_OBJECT = new Navigation();

    /** Initializes a new instance of the Document class */
    private Navigation() {
      super(
          ElementType.NAVIGATION,
          null,
          NAVIGATION_OBJECT_NAME,
          null,
          EMPTY_SELECTOR,
          Collections.emptyList(),
          false);
      registerElementMethod(ElementMethod.NAVIGATION_GETTER);
    }
  }
}
