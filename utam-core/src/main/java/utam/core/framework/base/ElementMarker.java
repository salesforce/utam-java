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
 * @author elizaveta.ivanova
 * @since 228
 */
public abstract class ElementMarker {

    @Target({ElementType.FIELD})
    @Retention(RetentionPolicy.RUNTIME)
    public @interface Find {

        String css() default "";

        String accessid() default "";

        String classchain() default "";

        String uiautomator() default "";

        String scope() default "";

        // defines if parent scope shadow root should be expanded
        boolean expand() default false;

        boolean nullable() default false;
    }

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

    static FindContext getFinderContext(Find annotation) {
        return Type.build(annotation.nullable(), annotation.expand());
    }
}
