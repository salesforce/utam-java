/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.selenium.appium;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;

import io.appium.java_client.MobileBy;
import org.testng.annotations.Test;

/**
 * tests for mobile locators
 *
 * @author Qingchun Ren
 * @since 228
 */
public class LocatorAccessibilityIdTests {

  @Test
  public void testCreation() {
    String selector = "selectorString";
    LocatorAccessibilityId locator = new LocatorAccessibilityId(selector);
    assertThat(locator.getValue(), is(equalTo(MobileBy.AccessibilityId(selector))));
    assertThat(locator.getStringValue(), is(equalTo(selector)));
    assertThat(locator.getCopy(selector), is(equalTo(locator)));
    assertThat(locator.setParameters("parameters"), is(sameInstance(locator)));
  }
}
