package utam.core.framework.consumer;

import utam.core.framework.base.PageObjectsFactory;
import utam.core.selenium.context.SeleniumContext;

/**
 * UTAM integration starting point to use in downstream projects
 * @deprecated since 230, use UtamLoaderConfig and UtamLoader instead
 * @author elizaveta.ivanova
 * @since 226
 */
@Deprecated
public interface PageObjectsProvider extends UtamLoaderConfig, UtamLoader {

  /**
   * get instance of selenium context
   * @return instance
   */
  SeleniumContext getSeleniumContext();

  /**
   * get instance of PO factory
   * @return instance
   */
  PageObjectsFactory getFactory();
}
