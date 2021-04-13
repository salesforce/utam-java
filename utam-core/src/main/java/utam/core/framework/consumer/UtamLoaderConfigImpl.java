/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.consumer;

import static utam.core.framework.consumer.PageObjectContextImpl.getClassFromName;
import static utam.core.framework.context.StringValueProfile.DEFAULT_PROFILE;

import io.appium.java_client.AppiumDriver;
import java.time.Duration;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.openqa.selenium.WebDriver;
import utam.core.appium.context.AppiumContextProvider;
import utam.core.framework.base.PageObject;
import utam.core.framework.base.PageObjectsFactory;
import utam.core.framework.base.PageObjectsFactoryImpl;
import utam.core.framework.context.Driver;
import utam.core.framework.context.Profile;
import utam.core.framework.context.ProfileContext;
import utam.core.framework.context.DefaultProfileContext;
import utam.core.selenium.context.SeleniumContext;
import utam.core.selenium.context.SeleniumContextProvider;

/**
 * This config can be used if consumer has a single POs dependency <br/> In this case we do not need
 * multiple class loaders because there is only one set of property files <br/> Config will be using
 * System Class Loader
 *
 * @author elizaveta.ivanova
 * @since 232
 */
public class UtamLoaderConfigImpl implements UtamLoaderConfig {

  // dependencies
  private final List<String> activeProfiles = new ArrayList<>();
  private final Map<String, ProfileContext> activeProfilesContext = new HashMap<>();
  final List<String> overrideProfiles = new ArrayList<>();
  final Map<String, ProfileContext> overrideProfilesContext = new HashMap<>();
  final DriverSettings driverSettings;
  // driver
  private final WebDriver webDriver;
  PageObjectsFactory pageObjectsFactory;
  SeleniumContext seleniumContext;

  public UtamLoaderConfigImpl(WebDriver driver) {
    this.webDriver = driver;
    this.driverSettings = new DriverSettings();
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
    resetFactory();
  }

  final void setDefaultProfile() {
    setProfile(DEFAULT_PROFILE);
  }

  /**
   * create page objects context for dependency injection <br/> for each Jar and each profile,
   * search for override config and remember in context
   *
   * @return page objects context for factory
   */
  PageObjectContext setPageObjectsContext() {
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

  void resetSeleniumContext() {
    if (this.seleniumContext != null) {
      this.seleniumContext = this.driverSettings.getSeleniumContext(this.webDriver);
    }
  }

  SeleniumContext getSeleniumContext() {
    if (this.seleniumContext == null) {
      this.seleniumContext = this.driverSettings.getSeleniumContext(this.webDriver);
    }
    return seleniumContext;
  }

  void resetFactory() {
    if (pageObjectsFactory != null) {
      this.pageObjectsFactory = new PageObjectsFactoryImpl(setPageObjectsContext(),
          this.seleniumContext);
    }
  }

  PageObjectsFactory getFactory() {
    if (pageObjectsFactory == null) {
      PageObjectContext context = setPageObjectsContext();
      this.pageObjectsFactory = new PageObjectsFactoryImpl(context, getSeleniumContext());
    }
    return pageObjectsFactory;
  }

  @Override
  public void setBridgeAppTitle(String title) {
    this.driverSettings.bridgeAppTitle = title;
    resetSeleniumContext();
    resetFactory();
  }

  @Override
  public <T extends PageObject> void setProfileOverride(Profile profile, Class<T> poInterface,
      Class<? extends T> poClass) {
    Profile notNullProfile = profile == null? DEFAULT_PROFILE : profile;
    String key = notNullProfile.getConfigName();
    ProfileContext context =
        overrideProfilesContext.containsKey(key) ? overrideProfilesContext.get(key)
            : new DefaultProfileContext(notNullProfile);
    context.setBean(poInterface, poClass.getName());
    if (!overrideProfiles.contains(key)) {
      overrideProfiles.add(key);
    }
    overrideProfilesContext.put(key, context);
    resetFactory();
  }

  @Override
  public void setLocationPolicy(LocationPolicy policy) {
    this.driverSettings.locationPolicy = policy;
    resetSeleniumContext();
    resetFactory();
  }

  @Override
  public void setTimeout(Duration timeout) {
    this.driverSettings.customTimeout = timeout;
    resetSeleniumContext();
    resetFactory();
  }

  /**
   * settings related to driver
   */
  static class DriverSettings {

    String bridgeAppTitle;
    Duration customTimeout;
    LocationPolicy locationPolicy;

    DriverSettings() {
      this.bridgeAppTitle = null;
      this.customTimeout = null;
      this.locationPolicy = LocationPolicyType.getDefault();
    }

    SeleniumContext getSeleniumContext(WebDriver webDriver) {
      SeleniumContextProvider seleniumContext = Driver.isMobileDriver(webDriver)
          ? new AppiumContextProvider((AppiumDriver) webDriver, bridgeAppTitle)
          : new SeleniumContextProvider(webDriver, locationPolicy);
      if (customTimeout != null) {
        seleniumContext.setPollingTimeout(customTimeout);
      }
      return seleniumContext;
    }
  }
}
