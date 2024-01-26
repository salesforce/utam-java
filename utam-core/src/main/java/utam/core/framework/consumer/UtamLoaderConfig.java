/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.consumer;

import java.time.Duration;
import utam.core.driver.DriverConfig;
import utam.core.framework.context.Profile;

/**
 * configuration of the UTAM integration by consumer, configuration should be created every time
 * Driver is created
 *
 * @author elizaveta.ivanova
 * @since 230
 */
public interface UtamLoaderConfig {

  /**
   * set active profile with intention to correctly pick implementing class if there are overrides
   * <br>
   * for each jar with dependencies it will try to find dependencies config and add overrides
   * injected class
   *
   * @param profile active profile
   */
  void setProfile(Profile profile);

  /**
   * create page objects context for dependency injection for each Jar and each profile, search for
   * override config and remember in context
   *
   * @return page objects context for factory
   */
  PageObjectContext getPageContext();

  /**
   * get configured Driver properties
   *
   * @return instance of driver config
   */
  DriverConfig getDriverConfig();

  /**
   * set implicit wait timeout
   *
   * @param timeout timeout duration
   */
  void setImplicitTimeout(Duration timeout);

  /**
   * set explicit wait timeout
   *
   * @param timeout timeout duration
   */
  void setExplicitTimeout(Duration timeout);

  /**
   * set polling interval for UI element interactions <br>
   *
   * @param pollingInterval timeout duration
   */
  void setPollingInterval(Duration pollingInterval);

  /**
   * used for mobile integration: set bridge app title
   *
   * @param title title of the bridge app
   */
  void setBridgeAppTitle(String title);
}
