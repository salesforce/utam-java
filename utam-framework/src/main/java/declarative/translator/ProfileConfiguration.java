package declarative.translator;

import framework.context.Profile;

import java.util.Set;

/**
 * profile configuration for translator <br>
 * each profile has its own configuration
 *
 * @author elizaveta.ivanova
 * @since 228
 */
public interface ProfileConfiguration {

  /**
   * name of the JSON property used to identify profile value
   *
   * @return string with JSON key
   */
  String getPropertyKey();

  /**
   * get Profile from string property value in JSON
   *
   * @param value property value
   * @return profile object
   */
  Profile getFromString(String value);

  /**
   * supported values for the profiles string
   *
   * @return set of possible values
   */
  Set<String> getSupportedValues();
}
