/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.element;

/**
 * element that is editable
 *
 * @author elizaveta.ivanova
 * @since 226
 */
public interface Editable extends BasicElement {

  /**
   * Apply WebElement.sendKeys from Selenium - "simulate typing into an element, which may set its
   * value". <br>
   * Method is wrapped in fluent wait to find the element. Throws exception if nothing found within
   * timeout.
   *
   * @param text text to enter
   */
  void setText(String text);

  /**
   * Applies Selenium standard method WebElement.clear (from selenium docs: If this element is a
   * text entry element, this will clear the value). Method is wrapped in fluent wait to find the
   * element. Throws exception if nothing found within timeout.
   */
  void clear();

  /**
   * clear then setText
   *
   * @param text text to enter
   */
  void clearAndType(String text);

  /**
   * press one key on the keyboard
   *
   * @see org.openqa.selenium.Keys for possible string values
   * @param key string representing a key like "Enter" or "Space"
   */
  void press(CharSequence key);
}
