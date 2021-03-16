package utam.core.framework.consumer;

import utam.core.framework.base.PageObject;
import utam.core.framework.context.Profile;

import java.time.Duration;

/**
 * configuration of the UTAM integration by consumer </br>
 * configuration should be created every time Driver is created
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
   * set active profile with intention to correctly pick implementing class if there are overrides <br/>
   * for each jar with dependencies it will try to find dependencies config and add overrides injected class
   *
   * @param profile active profile
   */
  void setProfile(Profile profile);

  /**
   * allows consumer to override a bean definition for profile from a test
   *
   * @param profile profile for which we override, if null use default profile
   * @param poInterface page object interface
   * @param poClass page object implementing class to inject in tunrime
   * @param <T> bounding type for page objects
   */
  <T extends PageObject> void setProfileOverride(Profile profile, Class<T> poInterface, Class<? extends T> poClass);

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
