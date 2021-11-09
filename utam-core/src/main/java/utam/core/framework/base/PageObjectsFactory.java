/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.base;

import utam.core.driver.Driver;
import utam.core.element.ElementLocation;
import utam.core.framework.consumer.PageObjectContext;

/**
 * Page Objects factory builds instance of the Page Object and bootstraps its elements
 *
 * @author elizaveta.ivanova
 * @since 216
 */
public interface PageObjectsFactory {

  /**
   * Get Page Objects Context
   *
   * @return reference to PO context
   */
  PageObjectContext getPageContext();

  /**
   * Get Driver instance
   *
   * @return instance of the driver implementation
   */
  Driver getDriver();

  /**
   * bootstrap Page Object using provided parameters
   *
   * @param instance instance to bootstrap
   * @param root     scoped root locator
   */
  void bootstrap(PageObject instance, ElementLocation root);

  <T extends RootPageObject> T create(Class<T> rootPageObjectType);

}
