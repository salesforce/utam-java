/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.element;

import java.util.List;
import utam.core.driver.Driver;

/**
 * wrapper around element instance from the integrated framework, ex. WebElement from Selenium
 *
 * @author elizaveta.ivanova
 * @since 234
 */
public interface Element {

  /**
   * nullable json element can return null
   *
   * @return true if element was not found
   */
  boolean isNull();

  Element findElement(Locator by, FindContext finderContext);

  List<Element> findElements(Locator by, FindContext finderContext);

  int containsElements(Locator by, boolean isExpandShadowRoot);

  boolean isDisplayed();

  boolean isEnabled();

  boolean isExisting();

  void clear();

  void click();

  /**
   * provided as temporary workaround when regular click does not work
   * @param driver Driver instance used in deprecated click
   * @deprecated when all browsers work as expected, will be removed
   */
  @Deprecated
  void deprecatedClick(Driver driver);

  String getAttribute(String attrName);

  String getText();

  void setText(String text);

  void scrollIntoView(Driver driver, ScrollOptions options);

  void moveTo(Driver driver);

  boolean hasFocus(Driver driver);

  void blur(Driver driver);

  void focus(Driver driver);

  void flick(Driver driver, int xOffset, int yOffset);

  boolean flickItems(GestureDirection direction);

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
}
