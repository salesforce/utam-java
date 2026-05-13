/*
 * Copyright (c) 2026, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.selenium.wrapped;

import org.openqa.selenium.WebDriver;

/**
 * WrappedDriverDecorator interface allows to customize driver methods for different selectors
 *
 * @author elizaveta.ivanova
 * @since 264
 */
public interface WrappedDriverDecorator extends WebDriver {

  /**
   * Unwrap the decorated WebDriver
   *
   * @return the underlying WebDriver instance
   */
  WebDriver unwrap();
}
