/*
 * Copyright (c) 2022, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.declarative.lint;

import java.io.File;
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
   * Get path to UTAM JSON file relative to project path. Used by SARIF.
   *
   * @return string with path
   */
  String getJsonFilePath();

  /**
   * Find code line number for SARIF report
   *
   * @param context search context (can be element, method etc.)
   * @param line string to fund
   * @return number of the line or 1 if no match
   */
  int findCodeLine(FileSearchContext context, String line);

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
  List<MethodLinting> getMethods();

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
   * @return elements map, key is locator as string
   */
  List<ElementLinting> getElements();

  /**
   * Context to search for the line inside the file
   *
   * @author elizaveta.ivanova
   * @since 242
   */
  interface FileSearchContext {

    /**
     * Search for the code line inside the file
     *
     * @param file page object JSON file
     * @param string string to find with partial match
     * @return line number or 1 if no match found
     */
    int find(File file, String string);
  }

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
     * Get root element for root locator
     *
     * @return root element
     */
    ElementLinting getRootElement();

    /**
     * Gets the metadata element from the root of the page object
     *
     * @return metadata element
     */
    MetadataLinting getMetadata();

    /**
     * Check if page object has a metadata element
     *
     * @return true if it does
     */
    boolean hasMetadata();

    /**
     * Check if page object has root selector
     *
     * @return true if it does
     */
    boolean isRoot();
  }

  /**
   * Linting information about the metadata property
   *
   * @author james.evans
   * @since 248
   */
  interface MetadataLinting {

    /**
     * Check if value object of metadata property has a property with the given name
     *
     * @param propertyName the name of the property to check for
     * @return true if the property exists in the value object of the metadata property
     */
    boolean hasMetadataProperty(String propertyName);

    /**
     * Gets the value of the named property from the metadata property object.
     *
     * @param propertyName the name of hte property for which to retrieve the value
     * @return the value of the property if it exists, otherwise null
     */
    Object getMetadataPropertyValue(String propertyName);
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
    String getTypeFullName();

    /**
     * Parent scope can be same for elements inside and outside shadow root
     *
     * @return true if element is inside shadow
     */
    boolean isSameScope(ElementLinting element);

    /**
     * Check if element has same locator as another element
     *
     * @param element element to check against
     * @return true if same
     */
    boolean isSameLocator(ElementLinting element);
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
