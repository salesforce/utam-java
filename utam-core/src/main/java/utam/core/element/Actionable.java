/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.element;

import utam.core.framework.base.UtamBase;

/**
 * interaction methods for UI element
 *
 * @author elizaveta.ivanova
 * @since 226
 */
public interface Actionable extends UtamBase {

  /**
   * returns true if element is found AND enabled <br>
   * it's an immediate check, no waiting is involved. Never throws any exceptions, just returns
   * true/false
   *
   * @return true if element is present and enabled
   */
  boolean isEnabled();

  /**
   * Returns string value of a given attribute, returns the value of the attribute matching the
   * name. <br>
   * Throws exception if element not found within timeout or could not return attribute value or
   * returned null value.
   *
   * @param attribute name of the attribute
   * @return string value of the attribute
   */
  String getAttribute(String attribute);

  /**
   * same as getAttribute("class")
   *
   * @return string value of the value attribute
   */
  String getClassAttribute();

  /**
   * Returns string with the innerText of an element. <br>
   * Throws exception if element not found within timeout or could not return innerText or returned
   * null value.
   *
   * @return inner text from the element
   */
  String getText();

  /**
   * same as getAttribute("title")
   *
   * @return string value of title attribute
   */
  String getTitle();

  /**
   * same as getAttribute("value")
   *
   * @return string value of the value attribute
   */
  String getValue();

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
   * checks if current element has focus <br>
   * uses Selenium WebDriver.switchTo().activeElement().equals(WebDriver) <br>
   * Throws exception if element not found within timeout
   * @return true if current element has focus
   */
  boolean isFocused();

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
