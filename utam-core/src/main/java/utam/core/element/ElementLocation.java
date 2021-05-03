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

  /**
   * get full chain of locators for logging and reporting, ex. "driver > By.cssSelector: css1 >>
   * By.cssSelector: css2"
   *
   * @return string with full chain
   */
  String getLocatorChainString();

  /**
   * find element starting from driver with all locators chain
   *
   * @param driver instance of the driver
   * @return instance of the element or null if element is nullable
   */
  Element findElement(Driver driver);

  /**
   * find elements starting from driver with all locators chain
   *
   * @param driver instance of the driver
   * @return list of found elements or null if element is nullable
   */
  List<Element> findElements(Driver driver);

  /**
   * marker of possible element absence
   *
   * @return true if element might ot be present
   */
  boolean isNullable();
}
