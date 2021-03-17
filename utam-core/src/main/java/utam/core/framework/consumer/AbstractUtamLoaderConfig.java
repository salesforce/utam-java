package utam.core.framework.consumer;

import static utam.core.framework.consumer.PageObjectContextImpl.getClassFromName;
import static utam.core.framework.context.StringValueProfile.DEFAULT_IMPL;

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
import utam.core.framework.context.ProfileContextEmpty;
import utam.core.selenium.context.SeleniumContext;
import utam.core.selenium.context.SeleniumContextProvider;

/**
 * implementation of a loader config
 *
 * @author elizaveta.ivanova
 * @since 230
 */
abstract class AbstractUtamLoaderConfig implements UtamLoaderConfig {

  // dependencies
  final List<String> activeProfiles = new ArrayList<>();
  final Map<String, ProfileContext> activeProfilesContext = new HashMap<>();
  final List<String> overrideProfiles = new ArrayList<>();
  final Map<String, ProfileContext> overrideProfilesContext = new HashMap<>();
  // driver
  private final WebDriver webDriver;
  PageObjectsFactory pageObjectsFactory;
  final DriverSettings driverSettings;
  SeleniumContext seleniumContext;

  AbstractUtamLoaderConfig(WebDriver driver) {
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

  final void setDefaultProfile() {
    setProfile(DEFAULT_IMPL);
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

  void resetNotNullSeleniumContext() {
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

  void resetNotNullFactory() {
    if (pageObjectsFactory != null) {
      this.pageObjectsFactory = new PageObjectsFactoryImpl(setPageObjectsContext(),
          this.seleniumContext);
    }
  }

  final PageObjectsFactory getFactory() {
    if (pageObjectsFactory == null) {
      PageObjectContext context = setPageObjectsContext();
      this.pageObjectsFactory = new PageObjectsFactoryImpl(context, getSeleniumContext());
    }
    return pageObjectsFactory;
  }

  @Override
  public void setBridgeAppTitle(String title) {
    this.driverSettings.bridgeAppTitle = title;
    resetNotNullSeleniumContext();
    resetNotNullFactory();
  }

  @Override
  public abstract void setProfile(Profile profile);

  @Override
  public <T extends PageObject> void setProfileOverride(Profile profile, Class<T> poInterface,
      Class<? extends T> poClass) {
    String key = profile == null ? DEFAULT_IMPL.getConfigName() : profile.getConfigName();
    ProfileContext context =
        overrideProfilesContext.containsKey(key) ? overrideProfilesContext.get(key)
            : new ProfileContextEmpty();
    context.setBean(poInterface, poClass.getName());
    if(!overrideProfiles.contains(key)) {
      overrideProfiles.add(key);
    }
    overrideProfilesContext.put(key, context);
    resetNotNullFactory();
  }

  @Override
  public void setLocationPolicy(LocationPolicy policy) {
    this.driverSettings.locationPolicy = policy;
    resetNotNullSeleniumContext();
    resetNotNullFactory();
  }

  @Override
  public void setTimeout(Duration timeout) {
    this.driverSettings.customTimeout = timeout;
    resetNotNullSeleniumContext();
    resetNotNullFactory();
  }

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
