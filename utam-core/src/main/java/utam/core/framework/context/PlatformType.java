/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.context;

import utam.core.framework.base.BasePageObject;
import utam.core.framework.base.PageMarker;

/**
 * mobile driver context can be native or web to switch inside a page object. same as
 * MobileContextType in utam-js
 *
 * @author elizaveta.ivanova
 * @since 230
 */
public enum PlatformType {

  /** platform is the web */
  WEB,

  /** platform is a native app */
  NATIVE;

  /**
   * Gets a platform type from a string value
   *
   * @param platformStr the platform type string
   * @return the platform type for the specified string
   */
  public static PlatformType fromString(String platformStr) {
    if (platformStr == null || platformStr.isEmpty()) {
      return null;
    }
    return PlatformType.valueOf(platformStr.toUpperCase());
  }

  /**
   * get platform type or WEB from page object class annotation
   *
   * @param pageObjectClass page object implementing class
   * @return platform type from annotation or default value (WEB)
   */
  public static PlatformType from(Class<? extends BasePageObject> pageObjectClass) {
    if (pageObjectClass.isAnnotationPresent(PageMarker.Switch.class)) {
      return pageObjectClass.getAnnotation(PageMarker.Switch.class).value();
    }
    // for mobile platform default context is WEB
    return PlatformType.WEB;
  }

  /**
   * Gets the annotation string for the platform type
   *
   * @return the annotation for the platform type
   */
  public String getAnnotation() {
    return String.format("%s.%s", getClass().getSimpleName(), name());
  }
}
