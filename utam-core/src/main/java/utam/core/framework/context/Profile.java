/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.context;

/**
 * Profile for a Page Objects dependency injection configuration <br>
 * Any profile can have name and possible values, ex. profile "platform" can have values "web", "ios", "android"
 *
 * @author elizaveta.ivanova
 * @since 228
 */
public interface Profile {

  /**
   * Profile name, ex. "platform"
   *
   * @return name of the profile
   */
  String getName();

  /**
   * Profile value, ex. "web" or "ios"
   *
   * @return value of the profile
   */
  String getValue();

  /**
   * Each profile can have associated dependencies injection config
   *
   * @return name of the config to further use for UTAM Loader Configuration
   */
  String getConfigName();
}
