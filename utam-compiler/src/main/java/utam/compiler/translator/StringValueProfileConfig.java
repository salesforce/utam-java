/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.translator;

import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import utam.core.declarative.translator.ProfileConfiguration;
import utam.core.framework.context.Profile;
import utam.core.framework.context.StringValueProfile;

/**
 * Profile configuration
 *
 * @author elizaveta.ivanova
 * @since 230
 */
public class StringValueProfileConfig implements ProfileConfiguration {

  private final String jsonKey;
  private final Set<String> values;

  /**
   * Initializes a new instance of the StringValueProfileConfig class
   *
   * @param profileName the name of the profile
   * @param values the list of values making up the profile
   */
  public StringValueProfileConfig(String profileName, String[] values) {
    this.jsonKey = profileName;
    this.values = Stream.of(values).collect(Collectors.toSet());
  }

  /**
   * Initializes a new instance of the StringValueProfileConfig class
   *
   * @param name the name of the profile
   * @param profiles the list of profiles
   */
  public StringValueProfileConfig(String name, Profile... profiles) {
    this(name, Stream.of(profiles).map(Profile::getValue).toArray(String[]::new));
  }

  /**
   * Initializes a new instance of the StringValueProfileConfig class, only used in unit tests
   *
   * @param name the name of the profile
   * @param value the value making up the profile
   */
  public StringValueProfileConfig(String name, String value) {
    this(name, new String[] {value});
  }

  /**
   * initializes profile configuration from profile. used in tests
   *
   * @param profile profile instance
   */
  public StringValueProfileConfig(Profile profile) {
    this(profile.getName(), profile.getValue());
  }

  @Override
  public String getPropertyKey() {
    return jsonKey;
  }

  @Override
  public Profile getFromString(String value) {
    if (!this.values.contains(value)) {
      // error should be handled by caller because of different context
      return null;
    }
    return new StringValueProfile(jsonKey, value);
  }

  @Override
  public Set<String> getSupportedValues() {
    return values;
  }

  @Override // for tests
  public boolean equals(Object obj) {
    if (obj instanceof StringValueProfileConfig) {
      return ((StringValueProfileConfig) obj).jsonKey.equals(jsonKey)
          && ((StringValueProfileConfig) obj).getSupportedValues().size()
              == getSupportedValues().size()
          && ((StringValueProfileConfig) obj)
              .getSupportedValues()
              .containsAll(getSupportedValues());
    }
    return false;
  }
}
