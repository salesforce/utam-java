/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.context;

/**
 * platform type can be native or web to switch driver context inside a page object
 *
 * @author elizaveta.ivanova
 * @since 230
 */
public enum PlatformType {
  NONE(""),
  WEB("web"),
  NATIVE("native");

  private final String name;

  PlatformType(String name) {
    this.name = name;
  }

  public static PlatformType fromString(String string) {
    if (string == null || string.isEmpty()) {
      return NONE;
    }
    for (PlatformType type : PlatformType.values()) {
      if (type.name.equals(string)) {
        return type;
      }
    }
    throw new IllegalArgumentException(String.format("Unknown platform type '%s'", string));
  }

  public String getAnnotation() {
    return String.format("%s.%s", getClass().getSimpleName(), name());
  }
}
