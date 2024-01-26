/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.declarative.translator;

import java.util.Set;
import utam.core.framework.context.Profile;

/**
 * profile configuration for translator <br>
 * each profile has its own configuration
 *
 * @author elizaveta.ivanova
 * @since 228
 */
public interface ProfileConfiguration {

  /**
   * name of the JSON property used to identify profile value
   *
   * @return string with JSON key
   */
  String getPropertyKey();

  /**
   * get Profile from string property value in JSON
   *
   * @param value property value
   * @return profile object
   */
  Profile getFromString(String value);

  /**
   * supported values for the profiles string
   *
   * @return set of possible values
   */
  Set<String> getSupportedValues();
}
