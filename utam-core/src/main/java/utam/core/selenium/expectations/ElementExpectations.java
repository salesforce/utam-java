/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
/*
 * @Copyright, 1999-2018, salesforce.com
 *  All Rights Reserved
 *  Company Confidential
 *  Project LPOP
 */

package utam.core.selenium.expectations;

import org.openqa.selenium.SearchContext;
import utam.core.selenium.context.WebDriverUtilities;

import java.util.function.Function;

/**
 * element expectations
 *
 * @author elizaveta.ivanova
 * @since 226
 */
public interface ElementExpectations<T> {

  T returnIfNothingFound();

  /**
   * provides log message when expectations are called
   *
   * @return log message to use in logs and in error
   */
  String getLogMessage();

  /**
   * find single element and returns value
   *
   * @return function to apply
   */
  Function<SearchContext, T> apply(WebDriverUtilities utilities);
}
