/*
 * Copyright (c) 2022, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.lint;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Stream;
import utam.core.declarative.lint.PageObjectLinting;
import utam.core.declarative.representation.TypeProvider;
import utam.core.element.Locator;

/**
 * Object with an information needed for linting of a single PO
 *
 * @author elizaveta.ivanova
 * @since 242
 */
public class PageObjectLintingImpl implements PageObjectLinting {

  private final String name;
  private final String type;
  private final Map<String, List<ElementLinting>> locatorsMap = new HashMap<>();
  private final Map<String, MethodLinting> methodsMap = new HashMap<>();
  private final Set<String> shadowRoots = new HashSet<>();
  private RootLinting rootContext;

  public PageObjectLintingImpl(String name, TypeProvider type) {
    this.name = name;
    this.type = type.getFullName();
  }

  static boolean isCustomElement(ElementLinting element) {
    return Stream
        .of(Element.LINTING_BASIC_TYPE, Element.LINTING_CONTAINER_TYPE, Element.LINTING_FRAME_TYPE)
        .noneMatch(type -> type.equals(element.getFullTypeName()));
  }

  @Override
  public String getName() {
    return name;
  }

  @Override
  public Collection<MethodLinting> getMethods() {
    return methodsMap.values();
  }

  @Override
  public Set<String> getAllLocators() {
    return locatorsMap.keySet();
  }

  @Override
  public void setElement(ElementLinting element) {
    if (!locatorsMap.containsKey(element.getLocator())) {
      locatorsMap.put(element.getLocator(), new ArrayList<>());
    }
    locatorsMap.get(element.getLocator()).add(element);
  }

  @Override
  public void setMethod(MethodLinting method) {
    methodsMap.put(method.getName(), method);
  }

  @Override
  public void setShadowBoundary(String elementName) {
    shadowRoots.add(elementName);
  }

  @Override
  public Set<String> getShadowBoundaries() {
    return shadowRoots;
  }

  @Override
  public String getTypeFullName() {
    return type;
  }

  @Override
  public RootLinting getRootContext() {
    return rootContext;
  }

  @Override
  public void setRootContext(RootLinting context) {
    this.rootContext = context;
  }

  @Override
  public List<ElementLinting> getElementsByLocator(String locator) {
    return locatorsMap.getOrDefault(locator, null);
  }

  /**
   * Element information for linting
   *
   * @author elizaveta.ivanova
   * @since 242
   */
  public static class Element implements ElementLinting {

    public static final String LINTING_BASIC_TYPE = "basic";
    public static final String LINTING_CONTAINER_TYPE = "container";
    public static final String LINTING_FRAME_TYPE = "frame";

    private final String locator;
    private final String type;
    private final String parentName;
    private final String name;
    private final boolean isList;

    public Element(String name, String type, Object locator, String parentName, boolean isList) {
      this.type = type;
      this.locator = locator == null ? null : locator.toString();
      this.parentName = parentName;
      this.name = name;
      this.isList = isList;
    }

    @Override
    public String getLocator() {
      return locator;
    }

    @Override
    public String getName() {
      return name;
    }

    @Override
    public String getFullTypeName() {
      return type;
    }

    @Override
    public String getParentScope() {
      return parentName;
    }

    @Override
    public boolean isList() {
      return isList;
    }
  }

  /**
   * Method information for linting
   *
   * @author elizaveta.ivanova
   * @since 242
   */
  public static class Method implements MethodLinting {

    private final boolean hasDescription;
    private final String name;

    public Method(String methodName, boolean hasDescription) {
      this.hasDescription = hasDescription;
      this.name = methodName;
    }

    @Override
    public String getName() {
      return name;
    }

    @Override
    public boolean hasDescription() {
      return hasDescription;
    }
  }

  /**
   * Root information for linting
   *
   * @author elizaveta.ivanova
   * @since 242
   */
  public static class Root implements RootLinting {

    private final boolean hasRootDescription;
    private final String locator;

    public Root(boolean hasRootDescription, Locator locator) {
      this.hasRootDescription = hasRootDescription;
      this.locator = locator == null ? null : locator.getValue().toString();
    }

    @Override
    public boolean hasDescription() {
      return hasRootDescription;
    }

    @Override
    public String getLocator() {
      return locator;
    }
  }
}
