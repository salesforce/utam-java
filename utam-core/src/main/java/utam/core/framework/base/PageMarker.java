/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.base;

import utam.core.framework.context.PlatformType;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * container for Page Objects annotations
 * @author elizaveta.ivanova
 * @since 228
 */
public abstract class PageMarker {

    /**
     * root selector annotation <br>
     * can contain %s
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
     * marks shadow root <br>
     * if marked it means PO has shadow root
     */
    @Target(ElementType.TYPE)
    @Retention(RetentionPolicy.RUNTIME)
    public @interface isShadowHost {}

    /** page context type annotation */
    @Target(ElementType.TYPE)
    @Retention(RetentionPolicy.RUNTIME)
    public @interface Switch {

      PlatformType value();
    }

}
