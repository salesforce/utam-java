/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.element;

/**
 * interaction methods for clickable element
 *
 * @author elizaveta.ivanova
 * @since 228
 */
public interface Clickable extends BasicElement {

  /**
   * clicks on the element using Selenium WebElement.click. Method is wrapped in fluent wait to find
   * the element. <br>
   * Throws exception if nothing found within timeout. <br>
   */
  void click();

  /** double-clicks on the element using the Selenium Actions class. */
  void doubleClick();

  /**
   * click and hold the element for the duration specified, using the Selenium Actions class.
   *
   * @param holdDurationSec duration in seconds for holding the click on the element
   */
  void clickAndHold(int holdDurationSec);

  /**
   * clicks on the element with the secondary mouse button, sometimes referred to as a
   * "right-click," using the Selenium Actions class.
   */
  void rightClick();
}
