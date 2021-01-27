/*
 * Copyright, 1999-2018, salesforce.com
 * All Rights Reserved
 * Company Confidential
 * Project LPOP
 */

package utam.core.framework.base;

import utam.core.framework.consumer.PageObjectContext;
import utam.core.selenium.context.SeleniumContext;

/**
 * Page Objects factory
 *
 * @author elizaveta.ivanova
 * @since 216
 */
public interface PageObjectsFactory {

  /**
   * get current context <br>
   * used to access parameter passed through context from PO - url, scope etc.
   *
   * @return reference to PO context
   */
  PageObjectContext getContext();

  /**
   * get current selenium context
   * @return instance of context
   */
  SeleniumContext getSeleniumContext();

  /**
   * bootstrap Page Object using provided parameters
   *
   * @param instance instance to bootstrap
   * @param parameters map with passed parameters
   */
  void bootstrap(PageObject instance, BootstrapParameters parameters);
}
