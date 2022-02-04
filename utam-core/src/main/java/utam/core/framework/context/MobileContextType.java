/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.context;

/**
 * mobile driver context can be native or web to switch inside a page object
 *
 * @author elizaveta.ivanova
 * @since 230
 */
public enum MobileContextType {
  /**
   * no platform
   */
  NONE(""),

  /**
   * platform is the web
   */
  WEB("web"),

  /**
   * platform is a native app
   */
  NATIVE("native");

  private final String name;

  MobileContextType(String name) {
    this.name = name;
  }

  /**
   * Gets a platform type from a string value
   * @param string the platform type string
   * @return the platform type for the specified string
   */
  public static MobileContextType fromString(String string) {
    if (string == null || string.isEmpty()) {
      return NONE;
    }
    for (MobileContextType type : MobileContextType.values()) {
      if (type.name.equals(string)) {
        return type;
      }
    }
    throw new IllegalArgumentException(String.format("Unknown platform type '%s'", string));
  }

  /**
   * Gets the annotation for the platform type
   * @return the annotation for the platform type
   */
  public String getAnnotation() {
    return String.format("%s.%s", getClass().getSimpleName(), name());
  }
}
