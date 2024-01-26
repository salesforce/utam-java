/*
 * Copyright (c) 2022, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.driver;

import utam.core.selenium.element.Rect;

/**
 * Window interface exposing methods for manipulating a single browser window or tab
 *
 * @author william.sandy
 */
public interface Window {

  /**
   * Gets the rectangle containing the size and position of the window with the current command
   * focus
   *
   * @return Rectangle of the window
   */
  Rect getRect();

  /**
   * Sets the rectangle containing the size and position of the window with the current command
   * focus
   *
   * @param rect the rectangle of the window
   */
  void setRect(Rect rect);

  /** Closes the window with the current command focus */
  void close();

  /**
   * Gets the document object associated with this window
   *
   * @return document object associated with this window
   */
  Document getDocument();
}
