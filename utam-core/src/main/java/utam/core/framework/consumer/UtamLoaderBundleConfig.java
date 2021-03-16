package utam.core.framework.consumer;

import java.util.ResourceBundle;
import java.util.function.Function;
import org.openqa.selenium.WebDriver;
import utam.core.framework.context.Profile;
import utam.core.framework.context.ProfileContextBundle;

/**
 * This config can only be used if consumer has a single POs dependency
 *
 * @author elizaveta.ivanova
 * @since 232
 */
public class UtamLoaderBundleConfig extends AbstractUtamLoaderConfig implements UtamLoaderConfig {

  private final Function<String, ResourceBundle> resourceBundleProvider;

  /**
   * creates an instance
   *
   * @param driver                 driver instance
   * @param resourceBundleProvider should be ResourceBundle.getBundle from the invoking module
   */
  public UtamLoaderBundleConfig(WebDriver driver,
      Function<String, ResourceBundle> resourceBundleProvider) {
    super(driver);
    this.resourceBundleProvider = resourceBundleProvider;
    setDefaultProfile();
  }

  @Override
  public void setProfile(Profile profile) {
    if (resourceBundleProvider != null) {
      String key = profile.getConfigName();
      if (activeProfilesContext.containsKey(key)) {
        throw new UtamError(String.format("duplicate profile '%s'", profile.getConfigName()));
      }
      // add to list to maintain order of overrides
      activeProfiles.add(key);
      activeProfilesContext.put(key, new ProfileContextBundle(profile, resourceBundleProvider));
    }
    resetNotNullFactory();
  }
}
