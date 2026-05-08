/*
 * Copyright (c) 2026, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.selenium.wrapped;

import org.openqa.selenium.WebElement;
import org.openqa.selenium.WrapsElement;

/**
 * WrappedElementDecorator interface allows to customize element methods for different selectors
 *
 * @author elizaveta.ivanova
 * @since 264
 */
public interface WrappedElementDecorator extends WebElement {

  /**
   * Unwrap the element to get the underlying WebElement. Recursively unwraps all WrapsElement
   * layers until reaching the base element.
   *
   * @return the unwrapped WebElement
   */
  default WebElement unwrap() {
    WebElement element = this;
    while (element instanceof WrapsElement) {
      WebElement inner = ((WrapsElement) element).getWrappedElement();
      if (inner == null || inner == element) {
        break;
      }
      element = inner;
    }
    return element;
  }
}
