/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.consumer;

import static utam.core.driver.DriverConfig.DEFAULT_EXPLICIT_TIMEOUT;
import static utam.core.driver.DriverConfig.DEFAULT_IMPLICIT_TIMEOUT;
import static utam.core.driver.DriverConfig.DEFAULT_POLLING_INTERVAL;
import static utam.core.framework.consumer.JsonLoaderConfig.loadConfig;
import static utam.core.framework.context.DefaultProfileContext.getEmptyProfileContext;
import static utam.core.framework.context.DefaultProfileContext.mergeBeans;
import static utam.core.framework.context.DefaultProfileContext.mergeDependencies;

import java.io.File;
import java.time.Duration;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import utam.core.driver.DriverConfig;
import utam.core.framework.UtamLogger;
import utam.core.framework.base.PageObject;
import utam.core.framework.consumer.JsonLoaderConfig.DeserializerFromFile;
import utam.core.framework.consumer.JsonLoaderConfig.DeserializerFromUrl;
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
   * always an active profile in the loader, but has lower precedence than any other profiles that
   * would implement the interface
   */
  public static final Profile DEFAULT_PROFILE = new StringValueProfile("default", "impl");

  // profiles that were set as active. it's a map because same profile should override old value
  private final Map<String, String> activeProfiles = new HashMap<>();
  // module names to read dependency injections
  private final Set<String> dependenciesModules;
  // driver timeouts, assigned by default values until set
  private Duration implicitTimeout;
  private Duration explicitTimeout;
  private Duration pollingInterval;
  // mobile bridge app title, by default empty string
  private String bridgeAppTitle;

  /**
   * Initializes a new instance of the UtamLoaderConfigImpl class
   *
   * @param resourceWithConfig a string resource containing configuration information to configure
   *     the loader
   */
  public UtamLoaderConfigImpl(String resourceWithConfig) {
    this(loadConfig(new DeserializerFromUrl(resourceWithConfig)));
  }

  /**
   * Initializes a new instance of the UtamLoaderConfigImpl class
   *
   * @param fileWithConfig a file containing configuration information to configure the loader
   */
  public UtamLoaderConfigImpl(File fileWithConfig) {
    this(loadConfig(new DeserializerFromFile(fileWithConfig)));
  }

  /** Initializes a new instance of the UtamLoaderConfigImpl class */
  public UtamLoaderConfigImpl() {
    this(new JsonLoaderConfig());
  }

  /**
   * Initializes a new instance of the UtamLoaderConfigImpl class
   *
   * @param config loader config from json file
   */
  public UtamLoaderConfigImpl(JsonLoaderConfig config) {
    this.dependenciesModules = config.getInjectionConfigs();
    this.implicitTimeout = DEFAULT_IMPLICIT_TIMEOUT;
    this.explicitTimeout = DEFAULT_EXPLICIT_TIMEOUT;
    this.pollingInterval = DEFAULT_POLLING_INTERVAL;
    this.bridgeAppTitle = "";
  }

  /**
   * used in tests to initiate config with hardcoded modules
   *
   * @param modules name of the configured modules
   */
  UtamLoaderConfigImpl(String... modules) {
    this();
    dependenciesModules.addAll(Arrays.asList(modules));
  }

  @Override
  public void setProfile(Profile profile) {
    UtamLogger.info(
        String.format("Set profile '%s' value to '%s'", profile.getName(), profile.getValue()));
    activeProfiles.put(profile.getName(), profile.getValue());
  }

  /**
   * based on profile name get context
   *
   * @param profileName profile name
   * @param mergedContext map with all contexts
   * @return profile context to collect beans
   */
  private ProfileContext getProfileContext(
      String profileName, Map<String, ProfileContext> mergedContext) {
    String profileValue = activeProfiles.get(profileName);
    String profileKey = new StringValueProfile(profileName, profileValue).getKey();
    return mergedContext.containsKey(profileKey)
        ? mergedContext.get(profileKey)
        : getEmptyProfileContext();
  }

  @Override
  public PageObjectContext getPageContext() {
    UtamLogger.info("Reload dependency injections configurations");
    ProfileContext defaultProfileContext = getEmptyProfileContext();
    // different modules can have config for same profile
    // map: key - profile key, value - profile context merged from multiple modules
    Map<String, ProfileContext> mergedContext = new HashMap<>();
    for (String moduleName : dependenciesModules) {
      Map<String, ProfileContext> moduleConfig =
          new JsonInjectionsConfig().readDependenciesConfig(moduleName);
      mergeDependencies(mergedContext, defaultProfileContext, moduleConfig);
    }
    Map<Class<? extends PageObject>, Class> beans = new HashMap<>();
    for (String profileName : activeProfiles.keySet()) {
      ProfileContext profileContext = getProfileContext(profileName, mergedContext);
      mergeBeans(beans, profileContext);
    }
    Map<Class<? extends PageObject>, Class> defaultBeans =
        mergeBeans(new HashMap<>(), defaultProfileContext);
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
}
