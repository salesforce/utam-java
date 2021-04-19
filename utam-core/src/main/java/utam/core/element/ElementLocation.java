/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.element;

import java.util.List;
import utam.core.driver.Driver;

/**
 * chain of locators to find an element, locator can be either selector or already existing/found
 * element
 *
 * @author elizaveta.ivanova
 * @since 234
 */
public interface ElementLocation {

  /**
   * scope locator at the end of the chain
   *
   * @param locator       locator
   * @param finderContext how to scope
   * @return new location instance
   */
  ElementLocation scope(Locator locator, FindContext finderContext);

  /**
   * set parameters in selectors, returns copy of the location element
   *
   * @param parameters parameters to apply to all selectors
   * @return location with applied parameters
   */
  ElementLocation setParameters(Object... parameters);

  String getLocatorChainString();

  Element findElement(Driver driver);

  List<Element> findElements(Driver driver);

  boolean isNullable();
}
