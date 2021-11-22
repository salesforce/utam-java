/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.representation;

import utam.core.declarative.representation.PageObjectMethod;
import utam.core.declarative.representation.UnionType;

/**
 * page element getter method
 *
 * @author elizaveta.ivanova
 * @since 236
 */
public abstract class BasicElementGetterMethod implements PageObjectMethod {

  /**
   * union type declared for a basic element. Can be null if element does not have one
   *
   * @return instance of a union type
   */
  public UnionType getInterfaceUnionType() {
    return null;
  }

  /**
   * union type declared for a basic element. Can be null if element does not have one
   *
   * @return instance of a union type
   */
  public UnionType getClassUnionType() {
    return null;
  }
}
