/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.base;

import java.util.List;
import utam.core.driver.Driver;
import utam.core.driver.DriverContext;
import utam.core.element.Element;
import utam.core.element.ElementLocation;
import utam.core.framework.consumer.PageObjectContext;

/**
 * Page Objects factory is used to build instance of the Page Object
 *
 * @author elizaveta.ivanova
 * @since 216
 */
public interface PageObjectsFactory {

  /**
   * get current context <br> used to access parameter passed through context from PO - url, scope
   * etc.
   *
   * @return reference to PO context
   */
  PageObjectContext getPageContext();

  /**
   * get driver context
   *
   * @return instance of context
   */
  DriverContext getDriverContext();

  /**
   * get driver abstraction
   *
   * @return instance of the driver implementation
   */
  Driver getDriver();

  /**
   * bootstrap Page Object using provided parameters
   *
   * @param instance   instance to bootstrap
   * @param root scoped root locator
   */
  void bootstrap(PageObject instance, ElementLocation root);

  <T extends RootPageObject> T create(Class<T> rootPageObjectType);

  Element findElement(ElementLocation location);

  List<Element> findElements(ElementLocation location);
}
