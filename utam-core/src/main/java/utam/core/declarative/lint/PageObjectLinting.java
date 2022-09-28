/*
 * Copyright (c) 2022, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.declarative.lint;

import java.util.Collection;
import java.util.List;
import java.util.Set;

/**
 * Store information for validation/linting of an individual Page Object
 *
 * @author elizaveta.ivanova
 * @since 242
 */
public interface PageObjectLinting {

  /**
   * Get page object name for an error message. Ex: my/page/object
   *
   * @return string with name
   */
  String getName();

  /**
   * Get type (full class name) of the page object. Ex: my.page.Object
   *
   * @return page object class
   */
  String getTypeFullName();

  /**
   * Get root element context
   *
   * @return object
   */
  RootLinting getRootContext();

  /**
   * Add information if root has description set in JSON
   *
   * @param context root linting context
   */
  void setRootContext(RootLinting context);

  /**
   * Get methods to lint
   *
   * @return methods
   */
  Collection<MethodLinting> getMethods();

  /**
   * Add element information to linting context
   *
   * @param element page object element
   */
  void setElement(ElementLinting element);

  /**
   * Add method information for linting
   *
   * @param methodContext linting information
   */
  void setMethod(MethodLinting methodContext);

  /**
   * If element has shadowRoot, add the information. Root not included.
   *
   * @param elementName name of the element
   */
  void setShadowBoundary(String elementName);

  /**
   * Get all element names that have shadowRoot, except root element
   *
   * @return set of element names
   */
  Set<String> getShadowBoundaries();

  /**
   * Get all elements by a certain locator
   *
   * @param locator locator string to find all elements by it
   * @return elements map, key is locator as string
   */
  List<ElementLinting> getElementsByLocator(String locator);

  /**
   * Get all locators to iterate by linting
   *
   * @return set
   */
  Set<String> getAllLocators();

  /**
   * Linting information about the page object root
   *
   * @author elizaveta.ivanova
   * @since 242
   */
  interface RootLinting {

    /**
     * Check if JSON file has root description
     *
     * @return boolean
     */
    boolean hasDescription();

    /**
     * Check if JSON file has root description with author property
     *
     * @return boolean
     */
    boolean hasAuthor();

    /**
     * Get String with root locator, for example "By.css('.css')"
     *
     * @return string
     */
    String getLocator();
  }

  /**
   * Linting information about an element
   *
   * @author elizaveta.ivanova
   * @since 242
   */
  interface ElementLinting {

    /**
     * Get element name
     *
     * @return string
     */
    String getName();

    /**
     * Get String with locator, for example "By.css('.css')"
     *
     * @return string
     */
    String getLocator();

    /**
     * Get full type name
     *
     * @return string
     */
    String getFullTypeName();

    /**
     * Get parent scope to check duplicate selectors
     *
     * @return string
     */
    String getParentScope();

    /**
     * Lists are allowed to have duplicates
     *
     * @return true if element is a list
     */
    boolean isList();
  }

  /**
   * Linting information about a method
   *
   * @author elizaveta.ivanova
   * @since 242
   */
  interface MethodLinting {

    /**
     * Get method name
     *
     * @return string
     */
    String getName();

    /**
     * Check if method has description set in JSON
     *
     * @return boolean
     */
    boolean hasDescription();
  }
}
