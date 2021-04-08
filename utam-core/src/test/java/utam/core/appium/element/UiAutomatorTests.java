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
import utam.core.selenium.element.LocatorNode;
import utam.core.selenium.element.LocatorNodeImpl;
import utam.core.selenium.element.LocatorParameters;
import utam.core.selenium.element.Selector;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;

/**
 * Android UIAutomator selector based locator
 *
 * @author Qingchun Ren
 * @since 228
 */
public class UiAutomatorTests {

  /** An ElementLocatorClassChain object should be able to be created */
  @Test
  public void testElementLocatorUIAutomatorCreation() {
    String selector = "fakeSelector()";
    LocatorNodeImpl locator = new UIAutomator(selector);
    assertThat(
        locator.by(), is(equalTo(MobileBy.AndroidUIAutomator("new UiSelector()." + selector))));
    assertThat(locator.getSelectorString(), is(equalTo("new UiSelector()." + selector)));
    assertThat(locator.getSelector().getType(), is(equalTo(Selector.Type.UIAUTOMATOR)));
  }

  /** The getSelfCopy method should return a new copy of the ElementLocatorUIAutomator object */
  @Test
  public void testGetSelfCopy() {
    String selector = "fakeSelector";
    UIAutomator locator = new UIAutomator(selector);
    LocatorNodeImpl copy = locator.getCopy();
    assertThat(copy.by(), is(equalTo(MobileBy.AndroidUIAutomator("new UiSelector()." + selector))));
    assertThat(copy.getSelectorString(), is(equalTo("new UiSelector()." + selector)));
  }

  /** The applyParameters method should throw a unsupport exception */
  @Test
  public void testApplyParametersThrow() {
    String selector = "fakeSelector";
    LocatorNode locator = new UIAutomator(selector);
    locator.setParameters(new LocatorParameters("parameters"));
  }
}
