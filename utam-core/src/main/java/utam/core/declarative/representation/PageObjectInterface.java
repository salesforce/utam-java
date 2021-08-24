/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.declarative.representation;

import java.util.Collection;

/**
 * representation of the interface to generate
 *
 * @author elizaveta.ivanova
 * @since 226
 */
public interface PageObjectInterface {

  /**
   * get all public API (methods) exposed by the interface
   *
   * @return collection of declared methods
   */
  Collection<MethodDeclaration> getDeclaredApi();

  /**
   * Gets the set of interfaces of elements declared in this Page Object interface.
   *
   * @return the collection of TypeProvider objects describing the interfaces declared for elements
   * in this Page Object
   */
  Collection<TypeProvider> getNestedInterfaces();

  /**
   * type of the declared interface
   *
   * @return type
   */
  TypeProvider getInterfaceType();

  /**
   * type of the base class, ex. Page Object or RootPageObject
   *
   * @return type
   */
  TypeProvider getBaseInterfaceType();

  /**
   * get string with a generated code
   *
   * @return string with generated code
   */
  String getGeneratedCode();

  /**
   * comments to use in a javadoc
   *
   * @return string
   */
  String getComments();
}
