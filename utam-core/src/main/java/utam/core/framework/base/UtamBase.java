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
 * base class for a Page Element and a Page Object
 *
 * @author elizaveta.ivanova
 * @since 234
 */
public interface UtamBase {

  /**
   * check if element can be found, throws if can't and not marked as nullable, if element is not
   * marked as nullable and not present, method throws because there is no point to check for
   * presence of the element that is always expected to be present and hence not marked as nullable
   *
   * @return true if element is found, false if element is nullable and not found, throws otherwise
   */
  boolean isPresent();

  /**
   * waits for the element to be absent within wait timeout, throws exception if element still
   * present after timeout. Note: this method throws an error if element was never present.
   */
  void waitForAbsence();

  /**
   * waits for the element to become visible (present AND displayed) within timeout, throws
   * exception if element still visible after timeout. Note: this method throws an error if element
   * was never present.
   */
  void waitForVisible();

  /**
   * waits for element to become not present OR not displayed within timeout <br>
   * Throws exception if element still present or visible after timeout. Note: this method throws an
   * error if element was never present.
   */
  void waitForInvisible();

  /**
   * returns true if element is found AND displayed. <br>
   * it's an immediate check, no waiting is involved. Note: this method throws an error if element
   * was never present.
   *
   * @return true if element is present and visible
   */
  boolean isVisible();

  /**
   * check if current element contains another element with the given selector
   *
   * @param locator value of the locator
   * @param isExpandShadow if set to true, search inside shadow root
   * @return true if element found
   */
  boolean containsElement(Locator locator, boolean isExpandShadow);

  /**
   * same as containsElement(Selector selector, false)
   *
   * @param locator value of the locator
   * @return true if element found
   */
  boolean containsElement(Locator locator);

  /**
   * polling wait that repeatedly applies expectations until truthy value is return (not null or
   * boolean true)
   *
   * @param condition condition to wait for
   * @param <T> return type
   * @return result of the applied expectations
   */
  <T> T waitFor(Supplier<T> condition);

  /**
   * polling wait that repeatedly applies expectations until truthy value is return (not null or
   * boolean true)
   *
   * @param condition condition to wait for
   * @param <T> return type
   * @param errorMessage message used in the thrown timeout exception
   * @return result of the applied expectations
   */
  <T> T waitFor(Supplier<T> condition, String errorMessage);
}
