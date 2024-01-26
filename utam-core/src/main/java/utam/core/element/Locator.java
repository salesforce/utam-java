/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.element;

/**
 * Locator for an element. Depending on the integration, T can be any type of the selector presented
 * in the framework. For example By in selenium
 *
 * @author elizaveta.ivanova
 * @since 234
 */
public interface Locator<T> {

  /** string parameter indicator */
  String SELECTOR_STRING_PARAMETER = "%s";

  /** integer parameter indicator */
  String SELECTOR_INTEGER_PARAMETER = "%d";

  /**
   * get locator value, for example Locator with type By returns By
   *
   * @return locator value
   */
  T getValue();

  /**
   * String value of the locator
   *
   * @return string representation
   */
  String getStringValue();

  /**
   * set parameters to the locator string (replace %d and %s by values)
   *
   * @param values array of parameter values
   * @return pair of index of the last applied parameter and locator with applied parameters
   */
  Locator<T> setParameters(Object... values);
}
