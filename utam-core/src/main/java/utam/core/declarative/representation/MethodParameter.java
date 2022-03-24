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
 * method parameter, can be literal. Has type and name or value.
 *
 * @author elizaveta.ivanova
 * @since 226
 */
public interface MethodParameter {

  /**
   * check if parameter is literal and can be skipped in method signature declaration
   *
   * @return true if passed by value AKA literal
   */
  boolean isLiteral();

  /**
   * get string value for parameter: for literal it can be anything that returns value without
   * referencing it, including "true", "getMyElement()", LocatorBy.css("css") etc. For non literal
   * parameter it's parameter name
   *
   * @return string with value
   */
  String getValue();

  /**
   * string with declaration for method signature. Literal parameters don't have it, non literal
   * example "String myArg" or "Locator myLocator". All parameter will be combined into comma
   * separated format for a method signature like "void myMethod(String myArg, Locator myLocator)"
   *
   * @return string with simple class name + space + name of the parameter
   */
  String getDeclaration();

  /**
   * get type of the parameter, used for imports and declarations
   *
   * @return type instance
   */
  TypeProvider getType();

  /**
   * Literal parameters such as Element can have nested parameters for selector/filter/matcher
   *
   * @return list of nested parameters or null
   */
  List<MethodParameter> getNestedParameters();

  /**
   * parameter description is used in generated javadoc
   *
   * @return string with description, can be null
   */
  String getDescription();
}
