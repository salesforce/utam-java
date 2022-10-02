/*
 * Copyright (c) 2022, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.element;

import utam.core.driver.Driver;
import utam.core.driver.Navigation;

/**
 * Navigation implementation
 *
 * @author elizaveta.ivanova
 * @since 242
 */
public class NavigationImpl implements Navigation {

  private final Driver driverAdapter;

  public NavigationImpl(Driver driverAdapter) {
    this.driverAdapter = driverAdapter;
  }

  @Override
  public void back() {
    this.driverAdapter.back();
  }

  @Override
  public void forward() {
    this.driverAdapter.forward();
  }
}
