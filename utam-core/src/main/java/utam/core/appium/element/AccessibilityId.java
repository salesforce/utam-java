/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.appium.element;

import utam.core.selenium.element.LocatorNodeImpl;
import utam.core.selenium.element.LocatorUtilities;

/**
 * @author elizaveta.ivanova
 * @since 230
 */
public class AccessibilityId extends LocatorNodeImpl {

  public AccessibilityId(String selector, String selectorString, Filter filter) {
    super(Mobile.byAccessibilityId(selector), selectorString, LocatorUtilities.DEFAULT_TRANSFORMER, filter);
  }

  AccessibilityId(String selector) {
    this(selector, selector, LocatorUtilities.EMPTY_FILTER);
  }

  @Override
  protected LocatorNodeImpl getCopy() {
    return new AccessibilityId(getSelector().getValue(), getSelectorString(), getFilterCopy());
  }
}
