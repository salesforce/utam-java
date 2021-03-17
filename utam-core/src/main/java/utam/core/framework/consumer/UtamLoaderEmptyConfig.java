package utam.core.framework.consumer;

import org.openqa.selenium.WebDriver;
import utam.core.framework.context.Profile;

/**
 * This config only allows manual overrides such as config.setProfileOverride
 *
 * @author elizaveta.ivanova
 * @since 232
 */
public class UtamLoaderEmptyConfig extends AbstractUtamLoaderConfig {

  public UtamLoaderEmptyConfig(WebDriver driver) {
    super(driver);
  }

  @Override
  public void setProfile(Profile profile) {
    throw new UtamError("UTAM Loader configuration can only be used to set bean explicitely");
  }
}
