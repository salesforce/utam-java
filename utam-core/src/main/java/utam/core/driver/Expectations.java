/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.driver;

import utam.core.element.Element;

/**
 * expectations act as a function parameter for waits
 *
 * @author elizaveta.ivanova
 * @since 234
 */
public interface Expectations<T> {

  /**
   * provides log message when expectations are called
   *
   * @return log message to use in logs and in error
   */
  String getLogMessage();

  /**
   * apply action and return value
   *
   * @param driver  instance of a driver, can be null
   * @param element instance of an element, can be null
   * @return result of the applied method
   */
  T apply(Driver driver, Element element);
}
