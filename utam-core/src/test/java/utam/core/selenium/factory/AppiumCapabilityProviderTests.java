/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.selenium.factory;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;

import org.openqa.selenium.remote.DesiredCapabilities;
import org.testng.annotations.Test;

/**
 * Tests for AppiumCapabilityProvider
 *
 * @author qren
 * @since 230
 */
public class AppiumCapabilityProviderTests {
  /** The setDesiredCapability method should set the target Appium capability to a string value */
  @Test
  public void testSetDesiredCapabilityString() {
    AppiumCapabilityProvider appiumCapabilityProvider = new AppiumCapabilityProvider();
    String capabilityName = "fakeCapabilityName";
    String capabilityValue = "fakeValue";
    appiumCapabilityProvider.setDesiredCapability(capabilityName, capabilityValue);
    DesiredCapabilities desiredCapabilities = appiumCapabilityProvider.getDesiredCapabilities();
    assertThat(desiredCapabilities.getCapability(capabilityName), is(equalTo(capabilityValue)));
  }

  /** The setDesiredCapability method should set the target Appium capability to a boolean value */
  @Test
  public void testSetDesiredCapabilityBoolean() {
    AppiumCapabilityProvider appiumCapabilityProvider = new AppiumCapabilityProvider();
    String capabilityName = "fakeCapabilityName";
    appiumCapabilityProvider.setDesiredCapability(capabilityName, true);
    DesiredCapabilities desiredCapabilities = appiumCapabilityProvider.getDesiredCapabilities();
    assertThat(desiredCapabilities.getCapability(capabilityName), is(equalTo(true)));
  }

  /** The setDesiredCapability method should set the target Appium capability to a integer value */
  @Test
  public void testSetDesiredCapabilityInteger() {
    AppiumCapabilityProvider appiumCapabilityProvider = new AppiumCapabilityProvider();
    String capabilityName = "fakeCapabilityName";
    int capabilityValue = 10;
    appiumCapabilityProvider.setDesiredCapability(capabilityName, capabilityValue);
    DesiredCapabilities desiredCapabilities = appiumCapabilityProvider.getDesiredCapabilities();
    assertThat(desiredCapabilities.getCapability(capabilityName), is(equalTo(capabilityValue)));
  }
}
