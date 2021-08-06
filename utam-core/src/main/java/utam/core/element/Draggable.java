/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.element;

/**
 * interaction methods for an element that can be "dragged"
 *
 * @author elizaveta.ivanova
 * @since 236
 */
public interface Draggable extends BasicElement {

  /**
   * drag and drop an element to the target location
   *
   * @param target location where to drop
   * @param holdDurationSec after clicking, hold for few seconds (emulates real user)
   */
  void dragAndDrop(BasicElement target, int holdDurationSec);

  /**
   * drag and drop an element to the target location
   *
   * @param target location where to drop
   */
  void dragAndDrop(BasicElement target);

  /**
   * drag and drop an element by offset
   *
   * @param xOffset horizontal offset
   * @param yOffset vertical offset
   * @param holdDurationSec after clicking, hold for few seconds (emulates real user)
   */
  void dragAndDropByOffset(int xOffset, int yOffset, int holdDurationSec);

  /**
   * drag and drop an element by offset
   *
   * @param xOffset horizontal offset
   * @param yOffset vertical offset
   */
  void dragAndDropByOffset(int xOffset, int yOffset);
}
