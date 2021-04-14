/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.consumer;

import utam.core.framework.base.PageObject;
import utam.core.selenium.element.Selector;

import java.util.List;

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
   * @param <T> the Page Object type
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
   * @param <T> the Page Object type
   * @return UTAM Page Object instance
   */
  <T extends PageObject> T load(Class<T> utamType, Selector injectSelector);

  /**
   * load UTAM Page objects using current element as scope
   *
   * @param utamType type to load
   * @param injectSelector inject root, use Web.byCss or Mobile.by
   * @param <T> the Page Object type
   * @return UTAM Page Object instance
   */
  <T extends PageObject> List<T> loadList(Class<T> utamType, Selector injectSelector);

  /**
   * returns true if loaded PO will be expanding its parent shadow
   * @return boolean
   */
  boolean isExpandScopeShadow();
}
