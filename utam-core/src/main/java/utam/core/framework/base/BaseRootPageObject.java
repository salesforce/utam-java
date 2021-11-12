/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.base;

/**
 * base class for a Root Page Objects
 *
 * @author elizaveta.ivanova
 * @since 236
 */
public class BaseRootPageObject extends BasePageObject implements RootPageObject {

  @Override
  public Object load() {
    log("wait for a root element to be found");
    this.waitFor(() -> getElement());
    return this;
  }
}
