/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.context;

import utam.core.framework.consumer.UtamError;

/**
 * string based profile
 *
 * @author elizaveta.ivanova
 * @since 230
 */
public class StringValueProfile implements Profile {

  static final String ERR_NAME_REQUIRED = "profile name must not be null or the empty string";
  static final String ERR_VALUE_REQUIRED = "profile value must not be null or the empty string";
  private final String name;
  private final String value;

  /**
   * Initializes a new instance of the StringValueProfile class
   *
   * @param profileName name of the profile
   * @param value value for the profile
   */
  public StringValueProfile(String profileName, String value) {
    if (profileName == null || profileName.isEmpty()) {
      throw new UtamError(ERR_NAME_REQUIRED);
    }

    if (value == null || value.isEmpty()) {
      throw new UtamError(ERR_VALUE_REQUIRED);
    }

    this.name = profileName;
    this.value = value;
  }

  @Override
  public String getName() {
    return name;
  }

  @Override
  public String getValue() {
    return value;
  }

  @Override
  public int hashCode() {
    return getKey().hashCode();
  }

  @Override // without this can't use Profile as a key in map inside Runner
  public boolean equals(Object obj) {
    if (obj instanceof Profile) {
      return ((Profile) obj).getKey().equals(this.getKey());
    }
    return false;
  }
}
