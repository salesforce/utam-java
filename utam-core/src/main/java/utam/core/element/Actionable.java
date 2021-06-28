/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.element;

/**
 * interaction methods for actionable element
 *
 * @author elizaveta.ivanova
 * @since 226
 */
public interface Actionable extends BasicElement {

  /**
   * performs Actions.moveToElement from Selenium, <br>
   * which "Moves the mouse to the middle of the element. The element is scrolled into view". <br>
   * Throws exception if element not found within timeout or element could not be moved to
   */
  void moveTo();

  /**
   * scrolls current element to the center of the screen <br>
   * executes javascript `arguments[0].scrollIntoView({block:'center'})` <br>
   * Throws exception if element not found within timeout or element could not be scrolled to center
   */
  void scrollToCenter();

  /**
   * scroll to the element <br>
   * executes javascript `return arguments[0].scrollIntoView(true);` <br>
   * Throws exception if element not found within timeout
   */
  void scrollToTop();

  /**
   * focus on the element <br>
   * executes javascript `arguments[0].focus();` <br>
   * Throws exception if element not found within timeout
   */
  void focus();

  /**
   * blurs the current element <br>
   * executes javascript `arguments[0].blur()` <br>
   * Throws exception if element not found within timeout or element could not be scrolled to center
   */
  void blur();
}
