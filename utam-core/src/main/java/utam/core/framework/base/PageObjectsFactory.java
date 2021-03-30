/*
 * Copyright, 1999-2018, salesforce.com
 * All Rights Reserved
 * Company Confidential
 * Project LPOP
 */

package utam.core.framework.base;

import utam.core.driver.Driver;
import utam.core.driver.DriverContext;
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
}
