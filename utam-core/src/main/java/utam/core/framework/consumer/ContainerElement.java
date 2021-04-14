/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.consumer;

import java.util.List;
import utam.core.element.Locator;
import utam.core.framework.base.PageObject;

/**
 * page object element that can be used as scope, applicable only for LPOP compatibility mode,
 * supported only for Selenium
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
   * @param utamType  type to load
   * @param injectCss inject root
   * @return Page Object instance
   * @deprecated use methods with locator as a parameter
   */
  @Deprecated
  <T extends PageObject> T load(Class<T> utamType, String injectCss);

  /**
   * load UTAM Page object using current element as scope
   *
   * @param utamType       type to load
   * @param injectSelector inject root, use Web.byCss or Mobile.by
   * @return UTAM Page Object instance
   */
  <T extends PageObject> T load(Class<T> utamType, Locator injectSelector);

  /**
   * load UTAM Page objects using current element as scope
   *
   * @param utamType       type to load
   * @param injectSelector inject root, use Web.byCss or Mobile.by
   * @return UTAM Page Object instance
   */
  <T extends PageObject> List<T> loadList(Class<T> utamType, Locator injectSelector);
}
