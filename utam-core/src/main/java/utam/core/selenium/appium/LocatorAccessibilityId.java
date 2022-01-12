/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.selenium.appium;

import io.appium.java_client.MobileBy;
import org.openqa.selenium.By;
import utam.core.selenium.element.LocatorBy;

/**
 * owned by mobile team
 *
 * @since 230
 */
public class LocatorAccessibilityId extends LocatorBy {

  /**
   * Initializes a new instance of the LocatorAccessibilityId class
   *
   * @param selectorString selector string to use
   */
  public LocatorAccessibilityId(String selectorString) {
    super(selectorString);
  }

  @Override
  public By getValue() {
    return MobileBy.AccessibilityId(stringValue);
  }

  @Override
  public LocatorBy getCopy(String valueWithParameters) {
    return new LocatorAccessibilityId(valueWithParameters);
  }
}
