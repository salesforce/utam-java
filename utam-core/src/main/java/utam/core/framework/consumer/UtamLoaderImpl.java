/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.consumer;

import static utam.core.element.FindContext.Type.EXISTING;
import static utam.core.selenium.factory.WebDriverFactory.getAdapter;

import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import utam.core.driver.Document;
import utam.core.driver.Driver;
import utam.core.driver.DriverTimeouts;
import utam.core.element.Element;
import utam.core.element.ElementLocation;
import utam.core.element.FrameElement;
import utam.core.element.Locator;
import utam.core.framework.base.PageObject;
import utam.core.framework.base.PageObjectsFactory;
import utam.core.framework.base.PageObjectsFactoryImpl;
import utam.core.framework.base.RootPageObject;
import utam.core.framework.element.DocumentObject;
import utam.core.framework.element.ElementLocationChain;
import utam.core.selenium.appium.MobileElementAdapter;
import utam.core.selenium.element.ElementAdapter;

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

  public UtamLoaderImpl(UtamLoaderConfig loaderConfig, Driver driver) {
    this.loaderConfig = loaderConfig;
    this.driver = driver;
    this.factory = new PageObjectsFactoryImpl(loaderConfig, driver);
    this.document = new DocumentObject(factory);
  }

  /**
   * create instance of loader for unit tests with minimum possible timeout
   *
   * @param driver simulator driver
   * @return loader instance
   */
  public static UtamLoader getSimulatorLoader(WebDriver driver) {
    UtamLoaderConfig config = new UtamLoaderConfigImpl(DriverTimeouts.TEST, new JsonLoaderConfig());
    return new UtamLoaderImpl(config, getAdapter(driver));
  }

  protected final PageObjectsFactory getFactory() {
    return factory;
  }

  protected final Driver getDriver() {
    return driver;
  }

  @Override
  public final void resetContext() {
    // new factory instance resets page context
    this.factory = new PageObjectsFactoryImpl(loaderConfig, driver);
    this.document = new DocumentObject(factory);
  }

  @Override
  public final UtamLoaderConfig getConfig() {
    return loaderConfig;
  }

  @Override
  public <T extends RootPageObject> T create(Class<T> type) {
    T instance = factory.getPageContext().getBean(type);
    ElementLocation finder = instance.setRootLocator(EXISTING);
    factory.bootstrap(instance, finder);
    return instance;
  }

  @Override
  public <T extends RootPageObject> T load(Class<T> type) {
    T instance = create(type);
    instance.load();
    return instance;
  }

  @Override
  public <T extends PageObject> T create(
      Container parent, Class<T> type, Locator locator) {
    T instance = factory.getPageContext().getBean(type);
    // todo - abstract selenium
    WebElement webElement = (WebElement) parent.getScope().get();
    Element element =
        driver.isMobile() ? new MobileElementAdapter(webElement, driver) : new ElementAdapter(webElement, driver);
    ElementLocation finder = new ElementLocationChain(element)
        .scope(locator, EXISTING);
    factory.bootstrap(instance, finder);
    return instance;
  }

  @Override
  public Document getDocument() {
    return document;
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
}
