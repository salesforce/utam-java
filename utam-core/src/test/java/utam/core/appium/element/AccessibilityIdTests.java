/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.appium.element;

import io.appium.java_client.MobileBy;
import org.testng.annotations.Test;
import utam.core.selenium.element.LocatorParameters;
import utam.core.selenium.element.Selector;
import utam.core.selenium.element.LocatorUtilities;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;

/**
 * tests for mobile locators
 *
 * @author Qingchun Ren
 * @since 228
 */
public class AccessibilityIdTests {

  @Test
  public void testCreation() {
    String selector = "selectorString";
    AccessibilityId locator = new AccessibilityId(selector);
    assertThat(locator.by(), is(equalTo(MobileBy.AccessibilityId(selector))));
    assertThat(locator.getSelectorString(), is(equalTo(selector)));
    assertThat(locator.getSelector().getType(), is(equalTo(Selector.Type.ACCESSID)));
    assertThat(locator.getCopy(), is(equalTo(locator)));
    assertThat(locator.getScopeTransformer(), is(equalTo(LocatorUtilities.DEFAULT_TRANSFORMER)));
    assertThat(locator.getFilter(), is(equalTo(LocatorUtilities.EMPTY_FILTER)));
    assertThat(locator.getNext(), is(nullValue()));
    locator.setParameters(new LocatorParameters("parameters"));
  }
}
