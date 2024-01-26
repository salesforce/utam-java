/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.base;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import utam.core.element.FindContext;
import utam.core.element.FindContext.Type;
import utam.core.element.Locator;
import utam.core.selenium.element.LocatorBy;

/**
 * container for annotations
 *
 * @author elizaveta.ivanova
 * @since 228
 */
public abstract class ElementMarker {

  /**
   * Build locator from annotation
   *
   * @param annotation field annotation
   * @return locator built from annotation
   */
  static Locator getLocator(Find annotation) {
    if (!annotation.accessid().isEmpty()) {
      return LocatorBy.byAccessibilityId(annotation.accessid());
    } else if (!annotation.classchain().isEmpty()) {
      return LocatorBy.byClassChain(annotation.classchain());
    } else if (!annotation.uiautomator().isEmpty()) {
      return LocatorBy.byUiAutomator(annotation.uiautomator());
    } else {
      return LocatorBy.byCss(annotation.css());
    }
  }

  /**
   * Build find context from annotation
   *
   * @param annotation field annotation
   * @return context built from annotation
   */
  static FindContext getFinderContext(Find annotation) {
    return Type.build(annotation.nullable(), annotation.expand());
  }

  /**
   * Build element location from annotation
   *
   * @param annotation field annotation
   * @return element location built from annotation
   */
  static ElementLocation getElementLocation(Find annotation) {
    return new ElementLocation(getLocator(annotation), getFinderContext(annotation));
  }

  /** Annotation marker for finding elements */
  @Target({ElementType.FIELD})
  @Retention(RetentionPolicy.RUNTIME)
  public @interface Find {

    /**
     * Gets the CSS selector to find the element
     *
     * @return the CSS selector to find the element
     */
    String css() default "";

    /**
     * Gets the mobile accessibility ID to find the element
     *
     * @return the mobile accessibility ID to find the element
     */
    String accessid() default "";

    /**
     * Gets the iOS class chain to find the element
     *
     * @return the iOS class chain to find the element
     */
    String classchain() default "";

    /**
     * Gets the Android UI Automator locator to find the element
     *
     * @return theAndroid UI Automator locator to find the element
     */
    String uiautomator() default "";

    /**
     * Gets a value indicating whether the parent scope shadow root should be expanded
     *
     * @return true if the parent scope shadow root should be expanded; otherwise, false
     */
    // defines if parent scope shadow root should be expanded
    boolean expand() default false;

    /**
     * Gets a value indicating whether the element is nullable
     *
     * @return true if the element is nullable; otherwise, false
     */
    boolean nullable() default false;
  }
}
