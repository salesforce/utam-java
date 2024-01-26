/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.element;

import java.time.Duration;

/**
 * options for a drag and drop actions: element, offset and duration.
 *
 * @author elizaveta.ivanova
 * @since 236
 */
public interface DragAndDropOptions {

  /**
   * Duration of hold
   *
   * @return null by default
   */
  Duration getHoldDuration();

  /**
   * Target position for drop, as an element
   *
   * @return null by default
   */
  Element getTargetElement();

  /**
   * Target position for drop, as offset coordinates, horizontal
   *
   * @return null by default
   */
  Integer getXOffset();

  /**
   * Target position for drop, as offset coordinates, vertical
   *
   * @return null by default
   */
  Integer getYOffset();

  /** drag by element options helper */
  class ByElement implements DragAndDropOptions {

    private final Element targetElement;
    private final Duration holdDuration;

    /**
     * Initializes a new instance of the DragAndDropOptions class
     *
     * @param targetElement the target element
     * @param holdDurationSec the duration to hold the gesture, in seconds
     */
    public ByElement(Element targetElement, int holdDurationSec) {
      this.holdDuration =
          holdDurationSec == 0 ? Duration.ZERO : Duration.ofSeconds(holdDurationSec);
      this.targetElement = targetElement;
    }

    /**
     * Initializes a new instance of the DragAndDropOptions class
     *
     * @param targetElement the target element
     */
    public ByElement(Element targetElement) {
      this(targetElement, 0);
    }

    @Override
    public Duration getHoldDuration() {
      return holdDuration;
    }

    @Override
    public Element getTargetElement() {
      return targetElement;
    }

    @Override
    public Integer getXOffset() {
      return null;
    }

    @Override
    public Integer getYOffset() {
      return null;
    }
  }

  /** drag by offset options helper */
  class ByOffset implements DragAndDropOptions {

    private final int x, y;
    private final Duration holdDuration;

    /**
     * Initializes a new instance of the ByOffset class
     *
     * @param xOffset the horizontal offset
     * @param yOffset the vertical offset
     * @param holdDurationSec the duration to hold the gesture, in seconds
     */
    public ByOffset(int xOffset, int yOffset, int holdDurationSec) {
      this.holdDuration =
          holdDurationSec == 0 ? Duration.ZERO : Duration.ofSeconds(holdDurationSec);
      this.x = xOffset;
      this.y = yOffset;
    }

    /**
     * Initializes a new instance of the ByOffset class
     *
     * @param xOffset the horizontal offset
     * @param yOffset the vertical offset
     */
    public ByOffset(int xOffset, int yOffset) {
      this(xOffset, yOffset, 0);
    }

    @Override
    public Duration getHoldDuration() {
      return holdDuration;
    }

    @Override
    public Integer getXOffset() {
      return x;
    }

    @Override
    public Integer getYOffset() {
      return y;
    }

    @Override
    public Element getTargetElement() {
      return null;
    }
  }
}
