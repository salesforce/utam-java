/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.consumer;

import static utam.core.framework.consumer.JsonLoaderConfig.loadConfig;
import static utam.core.framework.context.DefaultProfileContext.getEmptyProfileContext;
import static utam.core.framework.context.DefaultProfileContext.mergeBeans;
import static utam.core.framework.context.DefaultProfileContext.mergeDependencies;
import static utam.core.framework.context.DefaultProfileContext.mergeWithProperties;

import java.io.File;
import java.time.Duration;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.stream.Stream;
import utam.core.driver.DriverConfig;
import utam.core.framework.UtamLogger;
import utam.core.framework.base.PageObject;
import utam.core.framework.context.Profile;
import utam.core.framework.context.ProfileContext;
import utam.core.framework.context.StringValueProfile;

/**
 * Loader config is used to set
 *
 * @author elizaveta.ivanova
 * @since 232
 */
public class UtamLoaderConfigImpl implements UtamLoaderConfig {

  /**
   * always an active profile in the loader, but has lower precedence than any other profiles
   * that would implement the interface
   */
  public static final Profile DEFAULT_PROFILE = new StringValueProfile("default", "impl");

  // profiles that were set as active. it's a map because same profile should override old value
  private final Map<String, String> activeProfiles = new HashMap<>();
  // profiles assigned to a module: key is module name, value is pairs of profileKey/profile
  private final Map<String, Map<String, Profile>> pageObjectModules;
  // driver timeouts
  private Duration implicitTimeout = Duration.ZERO;
  private Duration explicitTimeout = Duration.ofSeconds(20);
  private Duration pollingInterval = Duration.ofMillis(200);
  // mobile bridge app title
  private String bridgeAppTitle;

  /**
   * Initializes a new instance of the UtamLoaderConfigImpl class
   *
   * @param resourceWithConfig a string resource containing configuration information to configure
   *                           the loader
   */
  public UtamLoaderConfigImpl(String resourceWithConfig) {
    this(loadConfig(resourceWithConfig));
  }

  /**
   * Initializes a new instance of the UtamLoaderConfigImpl class
   *
   * @param fileWithConfig a file containing configuration information to configure the loader
   */
  public UtamLoaderConfigImpl(File fileWithConfig) {
    this(loadConfig(fileWithConfig));
  }

  /**
   * Initializes a new instance of the UtamLoaderConfigImpl class
   */
  public UtamLoaderConfigImpl() {
    this(new JsonLoaderConfig());
  }

  /**
   * Initializes a new instance of the UtamLoaderConfigImpl class
   *
   * @param config loader config from json file
   */
  public UtamLoaderConfigImpl(JsonLoaderConfig config) {
    this.pageObjectModules = config.getModuleToProfilesMapping();
    DriverConfig driverConfig = config.driverConfig;
    setImplicitTimeout(driverConfig.getImplicitTimeout());
    setExplicitTimeout(driverConfig.getExplicitTimeout());
    setPollingInterval(driverConfig.getPollingInterval());
  }

  /**
   * used in tests to initiate config with hardcoded modules
   *
   * @param modules name of the configured modules
   */
  UtamLoaderConfigImpl(String... modules) {
    this();
    Stream.of(modules).forEach(this::setLoaderConfig);
  }

  private void setModuleProfile(String moduleName, Profile profile) {
    if (pageObjectModules.containsKey(moduleName)) {
      pageObjectModules.get(moduleName).put(profile.getKey(), profile);
    } else {
      Map<String, Profile> map = new HashMap<>();
      map.put(profile.getKey(), profile);
      pageObjectModules.put(moduleName, map);
    }
  }

  @Override
  public void setProfile(Profile profile) {
    UtamLogger.info(String.format("Set profile '%s' value to '%s'", profile.getName(), profile.getValue()));
    activeProfiles.put(profile.getName(), profile.getValue());
    Set<String> modules = getModules();
    if (modules.isEmpty()) { //modules might not be set
      setModuleProfile("", profile);
    }
    else {
      for (String moduleName : getModules()) {
        setModuleProfile(moduleName, profile);
      }
    }
  }

  @Override
  public void setLoaderConfig(String moduleName) {
    pageObjectModules.put(moduleName, new HashMap<>());
  }

  /**
   * different modules can have config for same profile. This method reduces it to one map
   *
   * @param defaultContext default context has to be collected separately
   * @return map: key - profile key, value - reduced context
   */
  private Map<String, ProfileContext> reduceConfig(ProfileContext defaultContext) {
    Map<String, ProfileContext> mergedConfigs = new HashMap<>();
    for (String moduleName : getModules()) {
      // from json
      if (!moduleName.isEmpty()) {
        Map<String, ProfileContext> moduleConfig = new JsonInjectionsConfig()
            .readDependenciesConfig(moduleName);
        mergeDependencies(mergedConfigs, defaultContext, moduleConfig);
      }
      // from properties
      for (String profileKey : pageObjectModules.get(moduleName).keySet()) {
        Profile profile = pageObjectModules.get(moduleName).get(profileKey);
        mergeWithProperties(mergedConfigs, moduleName, profile);
      }
    }
    return mergedConfigs;
  }

  /**
   * based on profile name get context
   *
   * @param profileName   profile name
   * @param mergedContext map with all contexts
   * @return profile context to collect beans
   */
  private ProfileContext getProfileContext(String profileName,
      Map<String, ProfileContext> mergedContext) {
    String profileValue = activeProfiles.get(profileName);
    String profileKey = new StringValueProfile(profileName, profileValue).getKey();
    return mergedContext.get(profileKey);
  }

  @Override
  public PageObjectContext getPageContext() {
    UtamLogger.info("Reload injection dependencies configurations");
    ProfileContext defaultProfileContext = getEmptyProfileContext();
    Map<String, ProfileContext> mergedContext = reduceConfig(defaultProfileContext);
    Map<Class<? extends PageObject>, Class> beans = new HashMap<>();
    for (String profileName : activeProfiles.keySet()) {
      ProfileContext profileContext = getProfileContext(profileName, mergedContext);
      mergeBeans(beans, profileContext);
    }
    Map<Class<? extends PageObject>, Class> defaultBeans = mergeBeans(new HashMap<>(), defaultProfileContext);
    return new PageObjectContextImpl(beans, defaultBeans);
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
    return new DriverConfig(implicitTimeout, explicitTimeout, pollingInterval, bridgeAppTitle);
  }

  private Set<String> getModules() {
    return pageObjectModules.keySet();
  }
}
