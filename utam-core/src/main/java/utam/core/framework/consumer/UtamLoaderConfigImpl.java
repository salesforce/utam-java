/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.consumer;

import static utam.core.framework.consumer.JsonLoaderConfig.loadConfig;
import static utam.core.framework.consumer.PageObjectContextImpl.getClassFromName;

import java.io.File;
import java.time.Duration;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.BiFunction;
import java.util.stream.Collectors;
import utam.core.driver.DriverConfig;
import utam.core.framework.UtamCoreError;
import utam.core.framework.base.PageObject;
import utam.core.framework.consumer.JsonLoaderConfig.Module;
import utam.core.framework.context.DefaultProfileContext;
import utam.core.framework.context.Profile;
import utam.core.framework.context.ProfileContext;

/**
 * Loader config is used to set
 *
 * @author elizaveta.ivanova
 * @since 232
 */
public class UtamLoaderConfigImpl implements UtamLoaderConfig {

  static final String ERR_DUPLICATE_PROFILE = "Profile %s = %s is already configured";

  private final Map<ProfileKey, ProfileContext> configuredProfilesContext = new HashMap<>();
  // profiles that were set as active
  private final List<Profile> activeProfiles = new ArrayList<>();
  private final List<String> pageObjectModules = new ArrayList<>();
  // driver timeouts
  private Duration implicitTimeout = Duration.ZERO;
  private Duration explicitTimeout = Duration.ofSeconds(20);
  private Duration pollingInterval = Duration.ofMillis(200);
  private String bridgeAppTitle;
  private final BiFunction<String, Profile, ProfileContext> profileContextProvider;

  public UtamLoaderConfigImpl(JsonLoaderConfig config) {
    this(config, DefaultProfileContext::new);
  }

  public UtamLoaderConfigImpl(String resourceWithConfig) {
    this(loadConfig(resourceWithConfig));
  }

  public UtamLoaderConfigImpl(File fileWithConfig) {
    this(loadConfig(fileWithConfig));
  }

  public UtamLoaderConfigImpl() {
    this(new JsonLoaderConfig());
  }

  protected UtamLoaderConfigImpl(String resourceWithConfig, BiFunction<String, Profile, ProfileContext> profileContextProvider) {
    this(loadConfig(resourceWithConfig), profileContextProvider);
  }

  private UtamLoaderConfigImpl(JsonLoaderConfig config, BiFunction<String, Profile, ProfileContext> profileContextProvider) {
    this.profileContextProvider = profileContextProvider;
    for (Module module : config.getModules()) {
      String moduleName = module.getName();
      pageObjectModules.add(moduleName);
      for(Profile profile : module.getModuleProfiles(config.profiles)) {
        setConfiguredProfile(moduleName, profile);
      }
    }
    DriverConfig driverConfig = config.driverConfig;
    setImplicitTimeout(driverConfig.getImplicitTimeout());
    setExplicitTimeout(driverConfig.getExplicitTimeout());
    setPollingInterval(driverConfig.getPollingInterval());
  }

  private static Map<Class<? extends PageObject>, Class<? extends PageObject>> getConfiguredBeans(
      ProfileContext profileContext) {
    return profileContext
        .getConfiguredBeans()
        .stream()
        .collect(Collectors.toMap(bean -> bean, bean -> {
          String name = profileContext.getBeanName(bean);
          return getClassFromName(name);
        }));
  }

  @Override
  public void setConfiguredProfile(String module, Profile profile) {
    ProfileKey key = new ProfileKey(profile, module);
    if (configuredProfilesContext.containsKey(key)) {
      throw new UtamCoreError(String.format(ERR_DUPLICATE_PROFILE, profile.getName(), profile.getValue()));
    }
    configuredProfilesContext.put(key, profileContextProvider.apply(module, profile));
  }

  @Override
  public void setProfile(Profile profile) {
    if (activeProfiles.contains(profile)) {
      throw new UtamCoreError(String.format(ERR_DUPLICATE_PROFILE, profile.getName(), profile.getValue()));
    }
    activeProfiles.add(profile);
    for (String module : pageObjectModules) {
      ProfileKey profileKey = new ProfileKey(profile, module);
      if (!configuredProfilesContext.containsKey(profileKey)) {
        setConfiguredProfile(module, profile);
      }
    }
  }

  private boolean isInactiveDefaultProfile(ProfileKey profileKey) {
    return !activeProfiles.contains(profileKey.profile) && profileKey.profile.isDefault();
  }

  @Override
  public PageObjectContext getPageContext() {
    Map<Class<? extends PageObject>, Class> beans = new HashMap<>();
    // first load beans for inactive profiles, active profiles are loaded last
    configuredProfilesContext.keySet().forEach(key -> {
      if (isInactiveDefaultProfile(key)) {
        ProfileContext profileContext = configuredProfilesContext.get(key);
        beans.putAll(getConfiguredBeans(profileContext));
      }
    });
    // load beans for active profiles to override
    activeProfiles
        .stream()
        .flatMap(profile -> getModules().stream().map(module -> new ProfileKey(profile, module)))
        .forEach(
            profileKey -> {
              ProfileContext profileContext = configuredProfilesContext.get(profileKey);
              beans.putAll(getConfiguredBeans(profileContext));
            });
    return new PageObjectContextImpl(beans);
  }

  @Override
  public void setBridgeAppTitle(String title) {
    this.bridgeAppTitle = title;
  }

  @Override
  public void setImplicitTimeout(Duration timeout) {
    this.implicitTimeout = timeout;
  }

  @Override
  public void setExplicitTimeout(Duration timeout) {
    this.explicitTimeout = timeout;
  }

  @Override
  public void setPollingInterval(Duration pollingInterval) {
    this.pollingInterval = pollingInterval;
  }

  @Override
  public DriverConfig getDriverConfig() {
    return new DriverConfig(implicitTimeout, explicitTimeout, pollingInterval);
  }

  @Override
  public String getBridgeAppTitle() {
    return bridgeAppTitle;
  }

  // used in tests
  Set<Profile> getConfiguredProfiles() {
    return configuredProfilesContext.keySet().stream().map(key -> key.profile).collect(Collectors.toSet());
  }

  // used in tests
  List<String> getModules() {
    return pageObjectModules;
  }

  static class ProfileKey {

    private final Profile profile;
    private final String moduleName;

    ProfileKey(Profile profile, String moduleName) {
      this.profile = profile;
      this.moduleName = moduleName;
    }

    // used as key in a map
    @Override
    public int hashCode() {
      return profile.getConfigName(moduleName).hashCode();
    }

    // used as key in a map
    @Override
    public boolean equals(Object obj) {
      if(obj instanceof ProfileKey) {
        return profile.getConfigName(moduleName)
            .equals(((ProfileKey)obj).profile.getConfigName(((ProfileKey) obj).moduleName));
      }
      return false;
    }
  }
}
