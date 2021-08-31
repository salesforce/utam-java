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

  // profile with default implementations for interfaces in case if modules are not set
  public static final Profile DEFAULT_PROFILE = new StringValueProfile("default", "impl") {
    @Override
    public boolean isDefault() {
      return true;
    }
  };
  private static final String ERR_NAME_REQUIRED =
      "profile name must not be null or the empty string";
  private static final String ERR_VALUES_REQUIRED =
      "profile value must not be null or the empty string";
  private static final String PROFILE_CONFIG_PATTERN = "%s_%s_config";
  private final String name;
  private final String value;

  public StringValueProfile(String profileName, String value) {
    if (profileName == null || profileName.isEmpty()) {
      throw new UtamError(ERR_NAME_REQUIRED);
    }

    if (value == null || value.isEmpty()) {
      throw new UtamError(ERR_VALUES_REQUIRED);
    }

    this.name = profileName;
    this.value = value;
  }

  private static String getKey(Profile profile) {
    return String.format(PROFILE_CONFIG_PATTERN, profile.getName(), profile.getValue());
  }

  static String getProfileConfigName(Profile profile, String moduleName) {
    if (moduleName == null || moduleName.isEmpty()) {
      return getKey(profile);
    }
    return moduleName + "_" + getKey(profile);
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
  public String getConfigName(String moduleName) {
    return getProfileConfigName(this, moduleName);
  }

  @Override
  public int hashCode() {
    return getKey(this).hashCode();
  }

  @Override //without this can't use Profile as a key in map inside Runner
  public boolean equals(Object obj) {
    if (obj instanceof StringValueProfile) {
      return getKey((StringValueProfile) obj).equals(getKey(this));
    }
    return false;
  }
}
