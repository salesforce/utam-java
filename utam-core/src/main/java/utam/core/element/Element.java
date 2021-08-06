/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.element;

import java.awt.Point;
import java.time.Duration;
import java.util.List;

/**
 * wrapper around element instance from the integrated framework, ex. WebElement from Selenium
 *
 * @author elizaveta.ivanova
 * @since 234
 */
public interface Element {

  /**
   * determines if find methods returned null
   *
   * @return true if element was not found
   */
  boolean isNull();

  /**
   * find and element with a given locator inside current element
   *
   * @param by            locator
   * @param finderContext context can be expanding shadowRoot and be nullable
   * @return instance of the found element or "null" element (with isNull returning true) for
   * nullable context
   */
  Element findElement(Locator by, FindContext finderContext);

  /**
   * find all elements with a given locator inside current element
   *
   * @param by            locator
   * @param finderContext context can be expanding shadowRoot and be nullable
   * @return list of found elements or empty list for nullable context
   */
  List<Element> findElements(Locator by, FindContext finderContext);

  /**
   * get number of elements with a given locator inside current element
   *
   * @param by                 locator
   * @param isExpandShadowRoot if true, search inside element's shadowRoot
   * @return number of elements or 0 if none found
   */
  int containsElements(Locator by, boolean isExpandShadowRoot);

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
   * check if element has focus
   *
   * @return boolean
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
   * only for mobile elements - flick action based on direction
   *
   * @param direction direction of the flick
   * @return boolean
   */
  boolean flickItems(GestureDirection direction);

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
    TOP,
    CENTER
  }

  /**
   * Enumeration of gesture directions.
   */
  enum GestureDirection {
    DOWN,
    UP,
    LEFT,
    RIGHT
  }

  /**
   * options for a drag and drop actions: element, offset and duration.
   *
   * @since 236
   */
  interface DragAndDropOptions {

    /**
     * Duration of hold
     *
     * @return null by default
     */
    default Duration getHoldDuration() {
      return null;
    }

    /**
     * Target position for drop, as an element
     *
     * @return null by default
     */
    default Element getTargetElement() {
      return null;
    }

    /**
     * Target position for drop, as offset coordinates
     *
     * @return null by default
     */
    default Point getOffset() {
      return null;
    }

  }
}
