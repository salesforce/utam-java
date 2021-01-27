package utam.core.framework.consumer;

import utam.core.appium.context.AppiumContextProvider;
import utam.core.framework.base.PageObjectsFactory;
import utam.core.framework.base.PageObjectsFactoryImpl;
import utam.core.framework.context.Driver;
import utam.core.framework.context.Profile;
import utam.core.framework.context.ProfileContext;
import io.appium.java_client.AppiumDriver;
import org.openqa.selenium.WebDriver;
import utam.core.selenium.context.SeleniumContext;
import utam.core.selenium.context.SeleniumContextProvider;

import java.time.Duration;
import java.util.ArrayList;
import java.util.List;

/**
 * default implementation of loader config
 *
 * @author elizaveta.ivanova
 * @since 230
 */
public final class UtamLoaderConfigImpl implements UtamLoaderConfig {

  final DependencyInjectionConfig dependencyInjectionConfig;
  final List<Profile> activeProfiles = new ArrayList<>();
  PageObjectsFactory pageObjectsFactory;
  SeleniumContext seleniumContext;
  String bridgeAppTitle;
  LocationPolicy locationPolicy = LocationPolicyType.getDefault();
  private WebDriver webDriver;
  private Duration customTimeout;

  public UtamLoaderConfigImpl() {
    this.dependencyInjectionConfig = new DependencyInjectionConfig();
  }

  private PageObjectsFactoryImpl setFactory() {
    PageObjectContext context = dependencyInjectionConfig.getDependenciesContext(activeProfiles);
    return new PageObjectsFactoryImpl(context, getSeleniumContext());
  }

  private SeleniumContext setSeleniumContext(WebDriver webDriver) {
    if(webDriver == null) {
      throw new NullPointerException("Driver is null, can't create WebDriver context");
    }
    SeleniumContextProvider seleniumContext = Driver.isMobileDriver(webDriver)
        ? new AppiumContextProvider((AppiumDriver) webDriver, bridgeAppTitle)
        : new SeleniumContextProvider(webDriver, locationPolicy);
    if(customTimeout != null) {
      seleniumContext.setPollingTimeout(customTimeout);
    }
    return seleniumContext;
  }

  void resetSettings() {
    if (seleniumContext != null) {
      seleniumContext = setSeleniumContext(this.webDriver);
    }
    if (pageObjectsFactory != null) {
      pageObjectsFactory = setFactory();
    }
  }

  final void setDriver(WebDriver driver) {
    this.webDriver = driver;
  }

  final SeleniumContext getSeleniumContext() {
    if (seleniumContext == null) {
      seleniumContext = setSeleniumContext(this.webDriver);
    }
    return seleniumContext;
  }

  final PageObjectsFactory getFactory() {
    if (pageObjectsFactory == null) {
      pageObjectsFactory = setFactory();
    }
    return pageObjectsFactory;
  }

  @Override
  public void setBridgeAppTitle(String title) {
    this.bridgeAppTitle = title;
    resetSettings();
  }

  @Override
  public void setActiveProfile(Profile key) {
    activeProfiles.add(key);
    resetSettings();
  }

  @Override
  public void setProfileContext(ProfileContext profileContext) {
    dependencyInjectionConfig.setProfileContext(profileContext);
  }

  @Override
  public void setLocationPolicy(LocationPolicy policy) {
    this.locationPolicy = policy;
    resetSettings();
  }

  @Override
  public void setTimeout(Duration timeout) {
    this.customTimeout = timeout;
    resetSettings();
  }
}
