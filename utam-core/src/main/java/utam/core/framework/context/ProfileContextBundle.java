package utam.core.framework.context;

import java.util.MissingResourceException;
import java.util.Properties;
import java.util.ResourceBundle;
import java.util.function.Function;
import utam.core.framework.UtamLogger;

/**
 * Context reads configuration from a resource bundle; can only be used if there is single jar with
 * POs dependencies
 *
 * @author elizaveta.ivanova
 * @since 232
 */
public class ProfileContextBundle extends AbstractProfileContext implements ProfileContext {

  /**
   * reads configuration from resource bundle; can only be used if there is a single jar with POs
   * dependencies
   *
   * @param profile                profile to create config name
   * @param resourceBundleProvider should be ResourceBundle.getBundle from the invoking module
   */
  public ProfileContextBundle(Profile profile,
      Function<String, ResourceBundle> resourceBundleProvider) {
    setBeans(getBeansFromResourceBundle(profile, resourceBundleProvider));
  }

  private static Properties getBeansFromResourceBundle(Profile profile,
      Function<String, ResourceBundle> resourceBundleProvider) {
    String configName = profile.getConfigName();
    Properties properties = new Properties();
    try {
      ResourceBundle resourceBundle = resourceBundleProvider.apply(configName);
      resourceBundle
          .keySet()
          .forEach(key -> properties.setProperty(key, resourceBundle.getString(key)));
    } catch (MissingResourceException e) {
      UtamLogger.warning(String.format(ERR_PROFILE_FILE, configName));
      UtamLogger.warning(e.getMessage());
    }
    return properties;
  }
}
