package utam.compiler.translator;

import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import declarative.translator.ProfileConfiguration;
import framework.consumer.UtamError;
import framework.context.Profile;
import framework.context.StringValueProfile;

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

  public StringValueProfileConfig(String name, Profile...profiles) {
    this(name, Stream.of(profiles).map(Profile::getValue).toArray(String[]::new));
  }

  // used in tests
  StringValueProfileConfig(String name, String value) {
    this(name, new String[] { value } );
  }

  public StringValueProfileConfig(String name, String[] values) {
    if (name == null || name.isEmpty()) {
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
    
    this.jsonKey = name;
    this.values = profileValues;
    this.profileValueProvider =
        string -> profileValues.stream()
            .filter(string::equals)
            .map(value -> new StringValueProfile(name, value))
            .findAny()
            .orElseThrow(
                () -> new UtamError(
                    String.format(
                        ERR_PROFILE_VALUE_INCORRECT,
                        jsonKey,
                        string)));
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

}
