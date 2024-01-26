/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.selenium.factory;

import org.openqa.selenium.remote.DesiredCapabilities;

/**
 * The provider to set and get desired Appium capabilities besides default ones
 *
 * @author qren
 * @since 230
 */
@SuppressWarnings("WeakerAccess")
public class AppiumCapabilityProvider {

  private final DesiredCapabilities desiredCapabilities = new DesiredCapabilities();

  /**
   * set up an Appium capability for creating a new session
   *
   * @param capabilityName the name of Appium capability that try to setup
   * @param value the value of Appium capability configured by capabilityName
   */
  public void setDesiredCapability(String capabilityName, Object value) {
    desiredCapabilities.setCapability(capabilityName, value);
  }

  /**
   * retrieve Appium capabilities that used by current active session
   *
   * @return DesiredCapabilities
   */
  public DesiredCapabilities getDesiredCapabilities() {
    return desiredCapabilities;
  }
}
