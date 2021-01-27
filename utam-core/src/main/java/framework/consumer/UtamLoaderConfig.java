package framework.consumer;

import framework.context.Profile;
import framework.context.ProfileContext;

import java.time.Duration;

/**
 * configuration of the UTAM integration
 *
 * @author elizaveta.ivanova
 * @since 230
 */
public interface UtamLoaderConfig {

  /**
   * used for mobile integration: set bridge app title
   *
   * @param title title of the bridge app
   */
  void setBridgeAppTitle(String title);

  /**
   * set active profile with intention to correctly pick implementing class if there are overrides
   *
   * @param profile active profile
   */
  void setActiveProfile(Profile profile);

  /**
   * set profile context, profile context sets dependencies overrides per profile
   *
   * @param profileContext context used for profile
   */
  void setProfileContext(ProfileContext profileContext);

  /**
   * only supported for utam java: sets location policy at the test level
   *
   * @param policy could be either chained location (default) or javascript based
   */
  void setLocationPolicy(LocationPolicy policy);

  /**
   * set polling timeout for UI element interactions <br>
   *
   * @param timeout timeout duration
   */
  void setTimeout(Duration timeout);
}
