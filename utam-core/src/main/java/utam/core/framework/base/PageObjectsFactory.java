/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.base;

import utam.core.driver.Document;
import utam.core.driver.Driver;
import utam.core.element.Element;
import utam.core.element.Locator;
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
   * @param element page object root or null for root page object
   * @param locator root locator
   */
  void bootstrap(PageObject instance, Element element, Locator locator);

  /**
   * Get Document instance
   *
   * @return instance of the document implementation
   */
  Document getDocument();

  /**
   * create instance of a Root Page Object
   *
   * @param rootPageObjectType Page Object type
   * @param <T> bound type
   * @return instance of the page object
   */
  <T extends RootPageObject> T create(Class<T> rootPageObjectType);
}
