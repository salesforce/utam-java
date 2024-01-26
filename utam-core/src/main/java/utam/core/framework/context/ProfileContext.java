/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.context;

import java.util.Collection;
import utam.core.framework.base.PageObject;

/**
 * Profile Context is used to configure dependency injection for a certain profile <br>
 * It can provide information about implementing class when a certain profile is active <br>
 * Profile Context has 1:1 relations with a Profile
 *
 * @see Profile
 * @author elizaveta.ivanova
 * @since 226
 */
public interface ProfileContext {

  /**
   * get class name override for the given PO class <br>
   * if null returned, then default definition will be used by a provider
   *
   * @param pageObjectType PO type
   * @param <T> type bound for a page object
   * @return string with class name or null if bean is not defined
   */
  <T extends PageObject> String getBeanName(Class<T> pageObjectType);

  /**
   * set custom bean definition
   *
   * @param pageObjectType PO type
   * @param implClassName class name to inject instance in runtime
   * @param <T> type bound for a page object
   */
  <T extends PageObject> void setBean(Class<? extends T> pageObjectType, String implClassName);

  /**
   * Get all configured types
   *
   * @return all beans types in random order
   */
  Collection<Class<? extends PageObject>> getConfiguredBeans();
}
