/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.declarative.representation;

import java.util.List;

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
  List<MethodDeclaration> getDeclaredApi();

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
   * get string comments to use in generated javadoc
   *
   * @return list of strings
   */
  List<String> getDescription();

  /**
   * get all declared public union types
   *
   * @return list of union types
   */
  List<UnionType> getUnionTypes();

  /**
   * page object can be marked as deprecated
   *
   * @return boolean
   */
  boolean isDeprecated();

  /**
   * if configured, page object code can have copyright header
   *
   * @return list of strings
   */
  List<String> getCopyright();
}
