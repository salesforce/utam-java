/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.consumer;

import static utam.core.framework.consumer.PageObjectContextImpl.getClassFromName;

import java.time.Duration;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import utam.core.driver.DriverContext;
import utam.core.driver.DriverTimeouts;
import utam.core.framework.UtamCoreError;
import utam.core.framework.base.PageObject;
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

  static final String DEFAULT_LOADER_CONFIG = "utam.loader";
  static final String ERR_PROFILE_ALREADY_SET = "duplicate profile '%s', it's already set";

  private final Map<String, ProfileContext> configuredProfilesContext = new HashMap<>();
  // profiles that were set as active
  private final List<String> activeProfiles = new ArrayList<>();
  private final List<String> pageObjectModules;
  // driver
  private DriverTimeouts timeouts;
  private String bridgeAppTitle;

  public UtamLoaderConfigImpl(DriverTimeouts timeouts, String configFile) {
    this.timeouts = timeouts;
    JsonBasedLoader config = new JsonBasedLoader(configFile);
    this.pageObjectModules = config.getModules();
    for (Profile profile : config.getConfiguredProfiles()) {
      for (String module : pageObjectModules) {
        setConfiguredProfile(module, profile);
      }
    }
  }

  public UtamLoaderConfigImpl(String configFile) {
    this(DriverTimeouts.DEFAULT, configFile);
  }

  public UtamLoaderConfigImpl() {
    this(DriverTimeouts.DEFAULT, DEFAULT_LOADER_CONFIG);
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
    String key = profile.getConfigName(module);
    if (configuredProfilesContext.containsKey(key)) {
      throw new UtamCoreError(String.format(ERR_PROFILE_ALREADY_SET, key));
    }
    configuredProfilesContext.put(key, new DefaultProfileContext(module, profile));
  }

  @Override
  public void setProfile(Profile profile) {
    for (String module : pageObjectModules) {
      String key = profile.getConfigName(module);
      if (activeProfiles.contains(key)) {
        throw new UtamCoreError(String.format(ERR_PROFILE_ALREADY_SET, key));
      }
      if (!configuredProfilesContext.containsKey(key)) {
        setConfiguredProfile(module, profile);
      }
      activeProfiles.add(key);
    }
  }

  @Override
  public PageObjectContext getPageContext() {
    Map<Class<? extends PageObject>, Class> beans = new HashMap<>();
    // first load beans for inactive profiles
    configuredProfilesContext.keySet().forEach(key -> {
      if (!activeProfiles.contains(key)) { //active profiles are loaded last
        ProfileContext profileContext = configuredProfilesContext.get(key);
        beans.putAll(getConfiguredBeans(profileContext));
      }
    });
    // then load beans for active profiles to override
    activeProfiles
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
  public DriverContext getDriverContext() {
    return new DriverContext(timeouts, bridgeAppTitle);
  }

  @Override
  public void setFindTimeout(Duration findTimeout) {
    timeouts = new DriverTimeouts(findTimeout, timeouts.getWaitForTimeout(),
        timeouts.getPollingInterval());
  }

  @Override
  public void setWaitForTimeout(Duration waitForTimeout) {
    timeouts = new DriverTimeouts(timeouts.getFindTimeout(), waitForTimeout,
        timeouts.getPollingInterval());
  }

  @Override
  public void setPollingInterval(Duration pollingInterval) {
    timeouts = new DriverTimeouts(timeouts.getFindTimeout(), timeouts.getWaitForTimeout(),
        pollingInterval);
  }

  // used in tests
  Map<String, ProfileContext> getConfiguredProfiles() {
    return configuredProfilesContext;
  }

  // used in tests
  List<String> getModules() {
    return pageObjectModules;
  }
}
