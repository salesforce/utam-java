package framework.consumer;

import framework.base.PageObject;
import selenium.element.Selector;

/**
 * page object element that can be used as scope
 *
 * @author elizaveta.ivanova
 * @since 228
 */
public interface ContainerElement {

  /**
   * inject scope into page object
   *
   * @param object page object
   */
  void setScope(Contained object);

  /**
   * load UTAM Page object using current element as scope
   *
   * @param utamType type to load
   * @param injectCss inject root
   * @return UTAM Page Object instance
   * @deprecated use method with Selector parameter
   */
  @Deprecated
  <T extends PageObject> T load(Class<T> utamType, String injectCss);

  /**
   * load UTAM Page object using current element as scope
   *
   * @param utamType type to load
   * @param injectSelector inject root, use Web.byCss or Mobile.by
   * @return UTAM Page Object instance
   */
  <T extends PageObject> T load(Class<T> utamType, Selector injectSelector);

  /**
   * returns true if loaded PO will be expanding its parent shadow
   * @return boolean
   */
  boolean isExpandScopeShadow();
}
