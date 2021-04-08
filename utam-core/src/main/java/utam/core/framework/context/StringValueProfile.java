/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.context;

import utam.core.framework.consumer.UtamError;

public class StringValueProfile implements Profile {
  
  private static final String ERR_NAME_REQUIRED = 
      "profile name must not be null or the empty string";
  private static final String ERR_VALUES_REQUIRED = 
      "profile value must not be null or the empty string";
  private static final String PROFILE_CONFIG_PATTERN = "%s_%s_config";

  // profile with default implementations for interfaces
  public static final Profile DEFAULT_PROFILE = new StringValueProfile("default", "impl");

  private final String name;
  private final String value;

  public StringValueProfile(String name, String value) {
    if (name == null || name.isEmpty()) {
      throw new UtamError(ERR_NAME_REQUIRED);
    }
    
    if (value == null || value.isEmpty()) {
      throw new UtamError(ERR_VALUES_REQUIRED);
    }
    
    this.name = name;
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
    return getConfigName().hashCode();
  }

  @Override //without this can't use Profile as a key in map inside Runner
  public boolean equals(Object obj) {
    if(obj instanceof StringValueProfile) {
      return ((StringValueProfile) obj).getConfigName().equals(this.getConfigName());
    }
    return false;
  }

  @Override
  public String getConfigName() {
    return String.format(PROFILE_CONFIG_PATTERN, getName(), getValue());
  }
}
