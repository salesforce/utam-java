/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.selenium.expectations;

import org.openqa.selenium.WebDriver;

import java.util.function.Function;

/**
 * expectations for PageObject actions such as url navigation
 *
 * @author elizaveta.ivanova
 * @since 222
 */
public interface DriverExpectations<T> {

  /**
   * do Not use boolean as T because webDriver wait considers false same as null
   *
   * @return T
   */
  Function<WebDriver, T> getter();
}
