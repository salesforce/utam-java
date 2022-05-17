/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.declarative.representation;

/**
 * @author elizaveta.ivanova
 * @since 228
 */
public interface PageObjectDeclaration {

  /**
   * Gets a value indicating whether the Page Object contains only an interface
   *
   * @return true if the Page Object contains only an interface; otherwise false
   */
  boolean isInterfaceOnly();

  /**
   * Gets a value indicating whether the Page Object contains an interface and implementation
   *
   * @return true if the Page Object contains an interface and implementation; otherwise false
   */
  boolean isClassWithInterface();

  /**
   * Gets the implementation of the Page Object
   *
   * @return the implementation of the Page Object
   */
  PageObjectClass getImplementation();

  /**
   * Gets the interface of the Page Object
   *
   * @return the interface of the Page Object
   */
  PageObjectInterface getInterface();
}
