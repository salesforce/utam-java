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
import utam.core.framework.context.PlatformType;
import utam.core.element.Locator;
import utam.core.selenium.element.LocatorBy;

/**
 * container for Page Objects annotations
 *
 * @author elizaveta.ivanova
 * @since 228
 */
public abstract class PageMarker {

  static Locator getRootLocatorFromAnnotation(PageMarker.Find annotation) {
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
   * root selector annotation <br> can contain %s
   */
  @Target(ElementType.TYPE)
  @Retention(RetentionPolicy.RUNTIME)
  public @interface Find {

    String css() default "";

    String accessid() default "";

    String classchain() default "";

    String uiautomator() default "";
  }

  /**
   * marks shadow root <br> if marked it means PO has shadow root
   */
  @Target(ElementType.TYPE)
  @Retention(RetentionPolicy.RUNTIME)
  public @interface isShadowHost {

  }

  /**
   * page context type annotation
   */
  @Target(ElementType.TYPE)
  @Retention(RetentionPolicy.RUNTIME)
  public @interface Switch {

    PlatformType value();
  }
}
