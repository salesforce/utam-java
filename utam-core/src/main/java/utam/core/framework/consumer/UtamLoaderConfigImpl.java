package utam.core.framework.consumer;

import static utam.core.framework.consumer.PageObjectContextImpl.getClassFromName;
import static utam.core.framework.context.StringValueProfile.DEFAULT_IMPL;

import io.appium.java_client.AppiumDriver;
import java.time.Duration;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Stream;
import org.openqa.selenium.WebDriver;
import utam.core.appium.context.AppiumContextProvider;
import utam.core.framework.base.PageObject;
import utam.core.framework.base.PageObjectsFactory;
import utam.core.framework.base.PageObjectsFactoryImpl;
import utam.core.framework.context.DefaultProfileContext;
import utam.core.framework.context.Driver;
import utam.core.framework.context.PlatformType;
import utam.core.framework.context.Profile;
import utam.core.framework.context.ProfileContext;
import utam.core.selenium.context.SeleniumContext;
import utam.core.selenium.context.SeleniumContextProvider;

/**
 * implementation of a loader config
 *
 * @author elizaveta.ivanova
 * @since 230
 */
public final class UtamLoaderConfigImpl implements UtamLoaderConfig {

  private static final String OVERRIDE_BEAN_KEY = "OVERRIDE_BEAN_KEY";

  // dependencies
  final List<String> activeProfiles = new ArrayList<>();
  private final Class[] configLoaders;
  private final Map<String, ProfileContext> profileContextMap = new HashMap<>();
  private final Map<String, ProfileContext> overridesContextMap = new HashMap<>();
  // driver
  private final WebDriver webDriver;
  PageObjectsFactory pageObjectsFactory;
  DriverSettings driverSettings;
  SeleniumContext seleniumContext;

  /**
   * each module with page objects has config with same names like platform_ios_config <br/> unless
   * classLoader is specified, cannot read config from multiple jars
   *
   * @param pageObjectLoaders ClassLoader from each jar with page objects
   */
  public UtamLoaderConfigImpl(WebDriver driver, Class... pageObjectLoaders) {
    this.webDriver = driver;
    this.driverSettings = new DriverSettings();
    if (pageObjectLoaders == null || pageObjectLoaders.length == 0) {
      this.configLoaders = new Class[0];
    } else {
      this.configLoaders = Stream.of(pageObjectLoaders).toArray(Class[]::new);
    }
    // by default set default profile and platform profile
    setActiveProfile(DEFAULT_IMPL);
    setActiveProfile(PlatformType.getActivePlatformProfile(driver));
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

  /**
   * create page objects context for dependency injection <br/> for each Jar and each profile,
   * search for override config and remember in context
   *
   * @return
   */
  PageObjectContext setPageObjectsContext() {
    Map<Class<? extends PageObject>, Class<? extends PageObject>> overrides = new HashMap<>();
    activeProfiles
        .forEach(
            profileKey -> {
              ProfileContext profileContext = profileContextMap.get(profileKey);
              setBean(profileContext, overrides);
            });
    overridesContextMap.values().forEach(override -> setBean(override, overrides));
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
      this.pageObjectsFactory = new PageObjectsFactoryImpl(context, this.seleniumContext);
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
  public void setActiveProfile(Profile profile) {
    for (Class jarLoader : this.configLoaders) {
      String key = String.format("%s_%s", jarLoader.getName(), profile.asKey());
      if (profileContextMap.containsKey(key)) {
        throw new UtamError(String.format("duplicate profile '%s' for loader '%s'", profile.asKey(),
            jarLoader.getName()));
      }
      // add to list to maintain order of overrides
      activeProfiles.add(key);
      profileContextMap.put(key, new DefaultProfileContext(profile, jarLoader));
    }
    resetNotNullFactory();
  }

  @Override
  public <T extends PageObject> void setProfileOverride(Profile profile, Class<T> poInterface,
      Class<? extends T> poClass) {
    String key = profile == null? DEFAULT_IMPL.asKey() : profile.asKey();
    ProfileContext context = overridesContextMap.containsKey(key) ? overridesContextMap.get(key)
        : new DefaultProfileContext();
    context.setBean(poInterface, poClass.getName());
    overridesContextMap.put(key, context);
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
