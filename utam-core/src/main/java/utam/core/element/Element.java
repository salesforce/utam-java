/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.element;

import java.util.List;

/**
 * wrapper around element instance from the integrated framework, ex. WebElement from Selenium
 *
 * @author elizaveta.ivanova
 * @since 234
 */
public interface Element {

  /**
   * find and element with a given locator inside current element
   *
   * @param by                 locator
   * @return instance of the found element or "null" element (with isNull returning true) for
   * nullable context
   */
  Element findElement(Locator by);

  /**
   * find all elements with a given locator inside current element
   *
   * @param by            locator
   * @return list of found elements or empty list for nullable context
   */
  List<Element> findElements(Locator by);

  /**
   * get number of elements with a given locator inside current element
   *
   * @param by                 locator
   * @return number of elements or 0 if none found
   */
  int containsElements(Locator by);

  /**
   * check if element is "displayed" according to underlying implementing library
   *
   * @return boolean, true if element is displayed
   */
  boolean isDisplayed();

  /**
   * check if element is "enabled" according to underlying implementing library
   *
   * @return boolean, true if element is enabled
   */
  boolean isEnabled();

  /**
   * check if element is present on the page. It returns false if it's a null or became stale
   *
   * @return boolean
   */
  boolean isExisting();

  /**
   * clear text inside the element, usually for an input box
   */
  void clear();

  /**
   * click on the element
   */
  void click();

  /**
   * provided as temporary workaround when regular click does not work
   *
   * @deprecated when all browsers work as expected, will be removed
   */
  @Deprecated
  void deprecatedClick();

  /**
   * get value of the attribute
   *
   * @param attrName name of the attribute
   * @return returned value
   */
  String getAttribute(String attrName);

  /**
   * get text inside an element
   *
   * @return string
   */
  String getText();

  /**
   * type in text inside the element, does not clear it first
   *
   * @param text string to enter
   */
  void setText(String text);

  /**
   * scroll the element into view
   *
   * @param options scroll into view options
   */
  void scrollIntoView(ScrollOptions options);

  /**
   * move cursor to the element
   */
  void moveTo();

  /**
   * Check if element has focus (is active) inside document or its parent shadow root
   *
   * @return true if current element is active
   */
  boolean hasFocus();

  /**
   * blur the element
   */
  void blur();

  /**
   * focus on the element
   */
  void focus();

  /**
   * only for mobile elements - flick action based on offset coordinates
   *
   * @param xOffset horizontal offset
   * @param yOffset vertical offset
   */
  void flick(int xOffset, int yOffset);

  /**
   * drag and drop an element to the target location
   *
   * @param options location where to drop as element or coordinates offset and optional hold
   *                duration
   */
  void dragAndDrop(DragAndDropOptions options);

  /**
   * types of scroll action
   */
  enum ScrollOptions {
    /**
     * scroll element to the top of the viewport
     */
    TOP,
    /**
     * scroll element to the center of the viewport
     */
    CENTER
  }
}
