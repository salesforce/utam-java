/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
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

  /**
   * build external instance
   * @param type type
   * @param parameters optional parameters
   * @deprecated compatibility is deprecated
   * @return externally injected Page Object
   */
  @Deprecated
  <T extends PageObject> T getExternalBean(Class<T> type, Object[] parameters);
}
