/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.consumer;

import static utam.core.driver.DriverConfig.DEFAULT_EXPLICIT_TIMEOUT_MOCK;
import static utam.core.selenium.factory.WebDriverFactory.getAdapter;

import java.time.Duration;
import java.util.function.Supplier;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import utam.core.driver.Document;
import utam.core.driver.Driver;
import utam.core.driver.Navigation;
import utam.core.element.Element;
import utam.core.element.FrameElement;
import utam.core.element.Locator;
import utam.core.framework.base.PageObject;
import utam.core.framework.base.PageObjectsFactory;
import utam.core.framework.base.PageObjectsFactoryImpl;
import utam.core.framework.base.RootPageObject;
import utam.core.framework.element.DocumentObject;
import utam.core.framework.element.NavigationImpl;
import utam.core.selenium.appium.MobileDriverAdapter;
import utam.core.selenium.appium.MobileElementAdapter;
import utam.core.selenium.element.ElementAdapter;
import utam.core.selenium.factory.WebDriverFactory;

/**
 * implementation of UtamLoader
 *
 * @author elizaveta.ivanova
 * @since 230
 */
public class UtamLoaderImpl implements UtamLoader {

  private final Driver driver;
  private final UtamLoaderConfig loaderConfig;
  private PageObjectsFactory factory;
  private Document document;
  private final Navigation navigation;

  /**
   * Initializes a new instance of the UtamLoaderImpl class
   *
   * @param loaderConfig the configuration object to configure the loader
   * @param driver a driver object to drive loaded Page Objects
   */
  public UtamLoaderImpl(UtamLoaderConfig loaderConfig, Driver driver) {
    this.loaderConfig = loaderConfig;
    this.driver = driver;
    this.factory = new PageObjectsFactoryImpl(loaderConfig, driver);
    this.document = new DocumentObject(factory);
    this.navigation = new NavigationImpl(factory);
  }

  /**
   * Initializes a new instance of the UtamLoaderImpl class
   *
   * @param loaderConfig the configuration object to configure the loader
   * @param webDriver a WebDriver object to drive loaded Page Objects
   */
  public UtamLoaderImpl(UtamLoaderConfig loaderConfig, WebDriver webDriver) {
    this.loaderConfig = loaderConfig;
    this.driver = WebDriverFactory.getAdapter(webDriver, loaderConfig.getDriverConfig());
    this.factory = new PageObjectsFactoryImpl(loaderConfig, driver);
    this.document = new DocumentObject(factory);
    this.navigation = new NavigationImpl(factory);
  }

  /**
   * create instance of loader for unit tests with minimum possible timeout
   *
   * @param driver simulator driver
   * @return loader instance
   */
  public static UtamLoader getSimulatorLoader(WebDriver driver) {
    UtamLoaderConfig config = new UtamLoaderConfigImpl(new JsonLoaderConfig());
    config.setExplicitTimeout(DEFAULT_EXPLICIT_TIMEOUT_MOCK);
    return new UtamLoaderImpl(config, getAdapter(driver, config.getDriverConfig()));
  }

  /**
   * Gets the factory for creating Page Object instances
   *
   * @return the factory for creating Page Object instances
   */
  protected final PageObjectsFactory getFactory() {
    return factory;
  }

  /**
   * Gets the driver object for automating Page Object instances
   *
   * @return the driver object for automating Page Object instances
   */
  protected final Driver getDriver() {
    return driver;
  }

  @Override
  public final void resetContext() {
    // reset context
    PageObjectContext context = loaderConfig.getPageContext();
    // driver config might have changed, propagate it to the driver
    driver.resetDriverConfig(loaderConfig.getDriverConfig());
    this.factory = new PageObjectsFactoryImpl(context, driver);
    this.document = new DocumentObject(factory);
  }

  @Override
  public final UtamLoaderConfig getConfig() {
    return loaderConfig;
  }

  @Override
  public <T extends RootPageObject> T create(Class<T> type) {
    return factory.create(type);
  }

  @Override
  public <T extends RootPageObject> T load(Class<T> type) {
    T instance = create(type);
    instance.load();
    return instance;
  }

  @Override
  @Deprecated
  public <T extends PageObject> T create(
      Container externalScopeProvider, Class<T> utamPageObjectType, Locator utamPageObjectRoot) {
    // todo - abstract selenium
    WebElement webElement = (WebElement) externalScopeProvider.getScope().get();
    // 1. create element wrapper for scope
    Element scope =
        driver instanceof MobileDriverAdapter
            ? new MobileElementAdapter(webElement, driver)
            : new ElementAdapter(webElement, driver);
    // 2. scope root inside wrapper
    Element element = scope.findElement(utamPageObjectRoot, false);
    T instance = factory.getPageContext().getBean(utamPageObjectType);
    // 3. inject root
    factory.bootstrap(instance, element, utamPageObjectRoot);
    return instance;
  }

  @Override
  public Document getDocument() {
    return document;
  }

  @Override
  public Navigation getNavigation() {
    return navigation;
  }

  @Override
  public void enterFrame(FrameElement frame) {
    document.enterFrame(frame);
  }

  @Override
  public <T extends RootPageObject> T enterFrameAndLoad(FrameElement frame, Class<T> type) {
    return document.enterFrameAndLoad(frame, type);
  }

  @Override
  public void exitToParentFrame() {
    document.exitToParentFrame();
  }

  @Override
  public void exitFrame() {
    document.exitFrame();
  }

  @Override
  public <T> T waitFor(Supplier<T> isTrue, String message, Duration timeout) {
    return getDriver().waitFor(isTrue, message, timeout);
  }

  @Override
  public <T> T waitFor(Supplier<T> isTrue, String message) {
    return waitFor(isTrue, message, loaderConfig.getDriverConfig().getExplicitTimeout());
  }
}
