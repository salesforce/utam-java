/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.element;

/**
 * context of finding an element inside its parent, can be nullable (element does not exist) or
 * expand parent shadow
 *
 * @author elizaveta.ivanova
 * @since 234
 */
public interface FindContext {

  /**
   * when searching for an element, check if scope element shadow root should be expanded
   *
   * @return true if need to expand
   */
  boolean isExpandScopeShadowRoot();

  /**
   * if element is marked as nullable, it can be not found inside its parent
   *
   * @return true is element can be non-existing
   */
  boolean isNullable();

  /**
   * currently known four types of context - nullable or not and in or outside shadow
   *
   * @author elizaveta.ivanova
   */
  enum Type implements FindContext {

    /** element is existing */
    EXISTING,

    /** element is nullable */
    NULLABLE,

    /** element exists in shadow root */
    EXISTING_IN_SHADOW,

    /** element is nullable in shadow root */
    NULLABLE_IN_SHADOW;

    /**
     * Builds the find context type value
     *
     * @param isNullable a value indicating whether the element is nullable
     * @param isExpandsShadowRoot a value indicating whether the element is in the shadow root
     * @return the appropriate type value
     */
    public static Type build(boolean isNullable, boolean isExpandsShadowRoot) {
      if (isNullable) {
        return isExpandsShadowRoot ? NULLABLE_IN_SHADOW : NULLABLE;
      }
      return isExpandsShadowRoot ? EXISTING_IN_SHADOW : EXISTING;
    }

    @Override
    public boolean isNullable() {
      return this == NULLABLE || this == NULLABLE_IN_SHADOW;
    }

    @Override
    public boolean isExpandScopeShadowRoot() {
      return this == EXISTING_IN_SHADOW || this == NULLABLE_IN_SHADOW;
    }
  }
}
