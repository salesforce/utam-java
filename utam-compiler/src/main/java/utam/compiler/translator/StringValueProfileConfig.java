/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.translator;

import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import utam.core.declarative.translator.ProfileConfiguration;
import utam.core.framework.consumer.UtamError;
import utam.core.framework.context.Profile;
import utam.core.framework.context.StringValueProfile;

/**
 * Profile configuration
 *
 * @author elizaveta.ivanova
 * @since 230
 */
public class StringValueProfileConfig implements ProfileConfiguration {

  static final String ERR_NAME_REQUIRED =
      "profile must contain a name that is not null or empty";
  static final String ERR_VALUES_REQUIRED =
      "profile must contain at least one non-null, non-empty value";
  static final String ERR_PROFILE_VALUE_INCORRECT =
          "profile '%s' does not support value '%s'";

  private final String jsonKey;
  private final Function<String, Profile> profileValueProvider;
  private final Set<String> values;

  public StringValueProfileConfig(String profileName, String[] values) {
    if (profileName == null || profileName.isEmpty()) {
      throw new UtamError(ERR_NAME_REQUIRED);
    }

    if (values == null || values.length == 0) {
      throw new UtamError(ERR_VALUES_REQUIRED);
    }

    Set<String> profileValues = Stream.of(values)
        .filter(value -> value != null && !value.isEmpty())
        .collect(Collectors.toSet());

    if (profileValues.size() == 0) {
      throw new UtamError(ERR_VALUES_REQUIRED);
    }

    this.jsonKey = profileName;
    this.values = profileValues;
    this.profileValueProvider =
        string -> profileValues.stream()
            .filter(string::equals)
            .map(value -> new StringValueProfile(profileName, value))
            .findAny()
            .orElseThrow(
                () -> new UtamError(
                    String.format(
                        ERR_PROFILE_VALUE_INCORRECT,
                        jsonKey,
                        string)));
  }

  public StringValueProfileConfig(String name, Profile...profiles) {
    this(name, Stream.of(profiles).map(Profile::getValue).toArray(String[]::new));
  }

  // used in tests
  public StringValueProfileConfig(String name, String value) {
    this(name, new String[] { value } );
  }

  @Override
  public String getPropertyKey() {
    return jsonKey;
  }

  @Override
  public Profile getFromString(String value) {
    return profileValueProvider.apply(value);
  }

  @Override
  public Set<String> getSupportedValues() {
    return values;
  }

  @Override //for tests
  public boolean equals(Object obj) {
    if(obj instanceof StringValueProfileConfig) {
      return ((StringValueProfileConfig) obj).jsonKey.equals(jsonKey)
          && ((StringValueProfileConfig) obj).getSupportedValues().size() == getSupportedValues().size()
          && ((StringValueProfileConfig) obj).getSupportedValues().containsAll(getSupportedValues());
    }
    return false;
  }
}
