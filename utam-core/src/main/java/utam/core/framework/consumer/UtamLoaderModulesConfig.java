package utam.core.framework.consumer;

import java.util.stream.Stream;
import org.openqa.selenium.WebDriver;
import utam.core.framework.context.Profile;
import utam.core.framework.context.ProfileContextModule;

/**
 * This config should be used for setup with multiple dependencies (JARs with page objects)
 *
 * @author elizaveta.ivanova
 * @since 232
 */
public final class UtamLoaderModulesConfig extends AbstractUtamLoaderConfig implements UtamLoaderConfig {

  private final Class[] configLoaders;

  /**
   * each module with page objects has config with same names like platform_ios_config <br/> unless
   * classLoader is specified, cannot read config from multiple jars
   *
   * @param pageObjectLoaders ClassLoader from each jar with page objects
   */
  public UtamLoaderModulesConfig(WebDriver driver, Class... pageObjectLoaders) {
    super(driver);
    if (pageObjectLoaders == null || pageObjectLoaders.length == 0) {
      this.configLoaders = new Class[0];
    } else {
      this.configLoaders = Stream.of(pageObjectLoaders).toArray(Class[]::new);
    }
    setDefaultProfile();
  }

  @Override
  public void setProfile(Profile profile) {
    for (Class jarLoader : configLoaders) {
      String key = String.format("%s_%s", jarLoader.getName(), profile.getConfigName());
      if (activeProfilesContext.containsKey(key)) {
        throw new UtamError(
            String.format("duplicate profile '%s' for loader '%s'", profile.getConfigName(),
                jarLoader.getName()));
      }
      // add to list to maintain order of overrides
      activeProfiles.add(key);
      activeProfilesContext.put(key, new ProfileContextModule(profile, jarLoader));
    }
    resetNotNullFactory();
  }
}
