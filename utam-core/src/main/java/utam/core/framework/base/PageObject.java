/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.base;

/**
 * base interface for all utam page objects <br>
 * methods have default implementations to mock external POs
 *
 * @author elizaveta.ivanova
 * @since 218
 */
public interface PageObject extends UtamBase {

  /**
   * actions in this method will be performed when page object is loaded from loader <br>
   * by default it's checking for visibility of its root element
   *
   * @return An Object that can be cast to the appropriate PageObject type
   */
  Object load();
}
