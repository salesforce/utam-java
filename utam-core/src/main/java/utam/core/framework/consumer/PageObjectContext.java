package utam.core.framework.consumer;

import utam.core.framework.base.PageObject;

/**
 * context to build page object from interface class
 *
 * @author elizaveta.ivanova
 * @since 226
 */
public interface PageObjectContext {

  <T extends PageObject> T getBean(Class<T> type);
}
