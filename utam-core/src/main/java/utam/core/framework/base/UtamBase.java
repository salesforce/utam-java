/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.base;

import java.util.function.Supplier;
import utam.core.element.Locator;

/**
 * base class for Page Element and Page Object <br>
 * combines capabilities of locator and fluent wait based actions
 *
 * @author elizaveta.ivanova
 * @since 222
 */
public interface UtamBase {

  /**
   * check if element can be found, throws if can't and not marked as nullable
   * @return true if element is found, false if element is nullable and not found,
   */
  boolean isPresent();

  /**
   * waits for element to be absent within timeout <br>
   * Throws exception if element still present after timeout
   */
  void waitForAbsence();

  /**
   * waits for element to become visible (present AND displayed) within timeout <br>
   * Throws exception if element still visible after timeout
   */
  void waitForVisible();

  /**
   * waits for element to become not present OR not displayed within timeout <br>
   * Throws exception if element still present or visible after timeout
   */
  void waitForInvisible();

  /**
   * returns true if element is found AND displayed. <br>
   * it's an immediate check, no waiting is involved. Never throws any exceptions, just returns
   * true/false
   *
   * @return true if element is present and visible
   */
  boolean isVisible();

  /**
   * check if current element contains another element with the given selector
   * @param locator value of the locator
   * @param isExpandShadow if set to true, search inside shadow root
   * @return true if element found
   */
  boolean containsElement(Locator locator, boolean isExpandShadow);

  /**
   * same as containsElement(Selector selector, boolean isExpandShadow); with isExpandShadow set to  false
   * @param locator value of the locator
   * @return true if element found
   */
  boolean containsElement(Locator locator);

  /**
   * polling wait repeatedly applies expectations until truthy value is return (not null or boolean
   * true)
   *
   * @param condition condition to wait for
   * @param <T>       return type
   * @return result of the applied expectations
   */
  <T> T waitFor(Supplier<T> condition);
}
