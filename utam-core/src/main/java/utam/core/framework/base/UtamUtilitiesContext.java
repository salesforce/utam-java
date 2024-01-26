/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.base;

/**
 * Context object passed as first argument to utilities. This context object can be used by
 * utilities authors when they need to interact with the current PO instance. For instance if they
 * need to do some actions or implement some custom logic around basic/custom elements in the
 * current page.
 *
 * @author olivier.martin
 * @since 234
 */
public class UtamUtilitiesContext {
  private final PageObject pageObject;

  /**
   * Creates a context object for utilities that gives access to the PO context within the utility.
   *
   * @param pageObjectInstance instance of the PO in which the utility has been declared
   */
  public UtamUtilitiesContext(PageObject pageObjectInstance) {
    this.pageObject = pageObjectInstance;
  }

  /**
   * @return the current PO instance
   */
  public PageObject getPageObject() {
    return pageObject;
  }
}
