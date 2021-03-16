package utam.core.framework.context;

import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.Properties;
import utam.core.framework.UtamLogger;

/**
 * Context reads configuration from properties file that can only be loaded with the right class
 * loader
 *
 * @author elizaveta.ivanova
 * @since 232
 */
public class ProfileContextModule extends AbstractProfileContext implements ProfileContext {

  /**
   * reads configuration from properties file that can only be loaded with the right class loader
   *
   * @param profile     profile to create config name
   * @param classLoader any class from Page Objects jar that can be used to get right class loader
   *                    for a module
   */
  public ProfileContextModule(Profile profile, Class classLoader) {
    setBeans(getBeansFromResource(profile, classLoader));
  }

  private static Properties getBeansFromResource(Profile profile, Class classLoader) {
    String configName = profile.getConfigName() + ".properties";
    Properties properties = new Properties();
    URL url = classLoader.getClassLoader().getResource(configName);
    if (url == null) {
      UtamLogger.warning(String.format(ERR_PROFILE_FILE, configName));
      return properties;
    }
    try {
      InputStream in = url.openStream();
      properties.load(in);
    } catch (IOException e) {
      UtamLogger.warning(String.format(ERR_PROFILE_FILE, configName));
      UtamLogger.warning(e.getMessage());
    }
    return properties;
  }
}
