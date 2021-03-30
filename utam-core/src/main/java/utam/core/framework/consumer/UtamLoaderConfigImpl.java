package utam.core.framework.consumer;

import static utam.core.framework.consumer.PageObjectContextImpl.getClassFromName;
import static utam.core.framework.context.StringValueProfile.DEFAULT_PROFILE;

import java.time.Duration;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import utam.core.framework.context.DefaultProfileContext;
import utam.core.framework.context.Profile;
import utam.core.framework.context.ProfileContext;
import utam.core.driver.DriverContext;
import utam.core.driver.DriverTimeouts;
import utam.core.framework.base.PageObject;

/**
 * This config can be used if consumer has a single POs dependency <br/> In this case we do not need
 * multiple class loaders because there is only one set of property files <br/> Config will be using
 * System Class Loader
 *
 * @author elizaveta.ivanova
 * @since 232
 */
public class UtamLoaderConfigImpl implements UtamLoaderConfig {

  final List<String> overrideProfiles = new ArrayList<>();
  final Map<String, ProfileContext> overrideProfilesContext = new HashMap<>();
  // dependencies
  private final List<String> activeProfiles = new ArrayList<>();
  private final Map<String, ProfileContext> activeProfilesContext = new HashMap<>();
  // driver
  private DriverTimeouts timeouts = new DriverTimeouts();
  private String bridgeAppTitle;

  public UtamLoaderConfigImpl() {
    setProfile(DEFAULT_PROFILE);
  }

  public UtamLoaderConfigImpl(DriverTimeouts timeouts) {
    this();
    this.timeouts = timeouts;
  }

  private static void setBean(ProfileContext profileContext,
      Map<Class<? extends PageObject>, Class<? extends PageObject>> overrides) {
    profileContext
        .getConfiguredBeans()
        .forEach(
            bean -> {
              String name = profileContext.getBeanName(bean);
              try {
                overrides.put(bean, getClassFromName(name));
              } catch (ClassNotFoundException e) {
                throw new UtamError(String.format("error configuring bean %s", name), e);
              }
            });
  }

  @Override
  public void setProfile(Profile profile) {
    String key = profile.getConfigName();
    if (activeProfilesContext.containsKey(key)) {
      throw new UtamError(
          String
              .format("duplicate profile '%s'", profile.getConfigName()));
    }
    // add to list to maintain order of overrides
    activeProfiles.add(key);
    activeProfilesContext.put(key, new DefaultProfileContext(profile));
  }


  @Override
  public PageObjectContext getPageContext() {
    Map<Class<? extends PageObject>, Class<? extends PageObject>> overrides = new HashMap<>();
    activeProfiles
        .forEach(
            profileKey -> {
              ProfileContext profileContext = activeProfilesContext.get(profileKey);
              setBean(profileContext, overrides);
            });
    overrideProfiles.forEach(profileKey -> {
      ProfileContext profileContext = overrideProfilesContext.get(profileKey);
      setBean(profileContext, overrides);
    });
    return new PageObjectContextImpl(overrides);
  }

  @Override
  public void setBridgeAppTitle(String title) {
    this.bridgeAppTitle = title;
  }

  @Override
  public <T extends PageObject> void setProfileOverride(Profile profile, Class<T> poInterface,
      Class<? extends T> poClass) {
    Profile notNullProfile = profile == null ? DEFAULT_PROFILE : profile;
    String key = notNullProfile.getConfigName();
    ProfileContext context =
        overrideProfilesContext.containsKey(key) ? overrideProfilesContext.get(key)
            : new DefaultProfileContext(notNullProfile);
    context.setBean(poInterface, poClass.getName());
    if (!overrideProfiles.contains(key)) {
      overrideProfiles.add(key);
    }
    overrideProfilesContext.put(key, context);
  }

  @Override
  public DriverContext getDriverContext() {
    return new DriverContext(timeouts, bridgeAppTitle);
  }

  @Override
  public void setFindTimeout(Duration findTimeout) {
    timeouts = new DriverTimeouts(findTimeout, timeouts.getWaitForTimeout(), timeouts.getPollingInterval());
  }

  @Override
  public void setWaitForTimeout(Duration waitForTimeout) {
    timeouts = new DriverTimeouts(timeouts.getFindTimeout(), waitForTimeout, timeouts.getPollingInterval());
  }

  @Override
  public void setPollingInterval(Duration pollingInterval) {
    timeouts = new DriverTimeouts(timeouts.getFindTimeout(), timeouts.getWaitForTimeout(), pollingInterval);
  }
}
