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

  /**
   * create instance of the given type based on config
   *
   * @param type PO type
   * @param <T> type bound
   * @return instance of the given type depending on profiles config
   */
  <T extends PageObject> T getBean(Class<T> type);
}
