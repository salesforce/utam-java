/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.selenium.element;

import org.openqa.selenium.By;

/**
 * CSS locator
 *
 * @author elizaveta.ivanova
 * @since 230
 */
class LocatorByCss extends LocatorBy {

  /**
   * Initializes a new instance of the LocatorByCss class
   *
   * @param stringValue selector string to use
   */
  LocatorByCss(String stringValue) {
    super(stringValue);
  }

  @Override
  public By getValue() {
    return By.cssSelector(stringValue);
  }

  @Override
  public LocatorBy getCopy(String valueWithParameters) {
    return new LocatorByCss(valueWithParameters);
  }
}
