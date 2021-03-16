package utam.core.framework.context;

import utam.core.framework.base.PageObject;

import java.util.Collection;
import java.util.Properties;

/**
 * context of the configured profile </br>
 * defines beans overrides per profile
 * @author elizaveta.ivanova
 * @since 226
 */
public interface ProfileContext {

  /**
   * get class name override for the given PO class
   * @param pageObjectType PO type
   * @param <T> type bound for a page object
   * @return string with class name or null if class is default
   */
  <T extends PageObject> String getBeanName(Class<T> pageObjectType);

  /**
   * set custom bean definition
   * @param pageObjectType PO type
   * @param implClassName class name to inject instance in runtime
   * @param <T> type bound for a page object
   */
  <T extends PageObject> void setBean(Class<? extends T> pageObjectType, String implClassName);

  /**
   * read bean definitions from properties
   * format: key - full PO type name, value - full class name to inject
   * @param properties configuration as key values pairs
   */
  void setBeans(Properties properties);

  /**
   * get all configured beans
   * @return all beans types in random order
   */
  Collection<Class<? extends PageObject>> getConfiguredBeans();
}
