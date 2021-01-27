package utam.core.framework.consumer;

import utam.core.framework.base.PageObject;
import utam.core.framework.context.Profile;
import utam.core.framework.context.ProfileContext;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static utam.core.framework.consumer.PageObjectContextImpl.getClassFromName;

/**
 * configuration to provider is passed from translator
 *
 * @author elizaveta.ivanova
 * @since 228
 */
class DependencyInjectionConfig {

  private final Map<Profile, ProfileContext> profileContextMap = new HashMap<>();
  private final Map<Class<? extends PageObject>, PageObject> beansOverride = new HashMap<>();

  void setProfileContext(ProfileContext profileContext) {
    Profile profile = profileContext.getProfile();
    if (profileContextMap.containsKey(profile)) {
      throw new UtamError(String.format("duplicate profile '%s'", profile.toString()));
    }
    profileContextMap.put(profile, profileContext);
  }

  @SuppressWarnings("unchecked")
  private static Class<? extends PageObject> getClass(String name) {
    try {
      return getClassFromName(name);
    } catch (ClassNotFoundException e) {
      throw new UtamError(String.format("error configuring bean %s", name), e);
    }
  }

  PageObjectContext getDependenciesContext(List<Profile> activeProfiles) {
    Map<Class<? extends PageObject>, Class<? extends PageObject>> overrides = new HashMap<>();
    activeProfiles
        .forEach(
            profile -> {
              ProfileContext profileContext = profileContextMap.get(profile);
              if (profileContext == null) {
                throw new UtamError(
                    String.format(
                        "profile context is not configured for profile '%s'", profile.toString()));
              }
              profileContext
                  .getConfiguredBeans()
                  .forEach(
                      bean -> overrides.put(bean, getClass(profileContext.getBeanName(bean))));
            });
    return new PageObjectContextImpl(overrides, beansOverride);
  }

  @Deprecated
  void setBean(Class<? extends PageObject> type, PageObject instance) {
    beansOverride.put(type, instance);
  }
}
