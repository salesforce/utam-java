/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.consumer;

import java.time.Duration;
import utam.core.driver.DriverContext;
import utam.core.framework.base.PageObject;
import utam.core.framework.context.Profile;

/**
 * configuration of the UTAM integration by consumer </br> configuration should be created every
 * time Driver is created
 *
 * @author elizaveta.ivanova
 * @since 230
 */
public interface UtamLoaderConfig {

  /**
   * set active profile with intention to correctly pick implementing class if there are overrides
   * <br/> for each jar with dependencies it will try to find dependencies config and add overrides
   * injected class
   *
   * @param profile active profile
   */
  void setProfile(Profile profile);

  /**
   * allows consumer to override a bean definition for profile from a test
   *
   * @param profile     profile for which we override, if null use default profile
   * @param poInterface page object interface
   * @param poClass     page object implementing class to inject in tunrime
   * @param <T>         bounding type for page objects
   */
  <T extends PageObject> void setProfileOverride(Profile profile, Class<T> poInterface,
      Class<? extends T> poClass);

  /**
   * create page objects context for dependency injection <br/> for each Jar and each profile,
   * search for override config and remember in context
   *
   * @return page objects context for factory
   */
  PageObjectContext getPageContext();

  /**
   * builds new Driver context based on config parameters
   *
   * @return new instance of config
   */
  DriverContext getDriverContext();

  /**
   * set timeout to find UI element <br>
   *
   * @param findTimeout timeout duration
   */
  void setFindTimeout(Duration findTimeout);

  /**
   * set polling timeout for UI element interactions <br>
   *
   * @param waitForTimeout timeout duration
   */
  void setWaitForTimeout(Duration waitForTimeout);

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
