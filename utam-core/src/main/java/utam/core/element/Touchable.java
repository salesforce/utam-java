/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.element;

/**
 * interaction methods for touchable element
 *
 * @author r.rajasekaran
 * @since 232
 */
public interface Touchable extends BasicElement {

  /**
   * Flick on the touch screen using finger motion events. Start point is middle of the element. End
   * point is determined by x and y offset coordinates. For vertical flick, use xOffset as 0 and
   * yOffset as pixels to flick by. For horizontal flick, use yOffset as 0 and xOffset as pixels to
   * flick by.
   *
   * @param xOffset Offset for x
   * @param yOffset Offset for y
   */
  void flick(int xOffset, int yOffset);
}
