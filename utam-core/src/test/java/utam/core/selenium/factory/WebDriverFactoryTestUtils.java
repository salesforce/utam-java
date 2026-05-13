/*
 * Copyright (c) 2026, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.selenium.factory;

import static utam.core.driver.DriverConfig.TEST_SIMULATOR_DRIVER_CONFIG;

import org.openqa.selenium.WebDriver;
import utam.core.driver.Driver;

/**
 * Test utilities for WebDriverFactory
 *
 * @author elizaveta.ivanova
 * @since 264
 */
public class WebDriverFactoryTestUtils {

  /**
   * Creates a driver adapter for testing/mocking purposes
   *
   * @param driver the WebDriver instance
   * @return instance of Driver
   */
  public static Driver getAdapterMock(WebDriver driver) {
    return WebDriverFactory.getAdapter(driver, TEST_SIMULATOR_DRIVER_CONFIG);
  }
}
