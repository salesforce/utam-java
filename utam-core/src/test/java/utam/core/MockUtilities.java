/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.withSettings;
import static utam.core.framework.element.BasePageElement.createInstance;
import static utam.core.selenium.element.ShadowRootWebElement.GET_SHADOW_ROOT_QUERY_SELECTOR;
import static utam.core.selenium.element.ShadowRootWebElement.GET_SHADOW_ROOT_QUERY_SELECTOR_ALL;

import io.appium.java_client.AppiumDriver;
import io.appium.java_client.android.AndroidDriver;
import io.appium.java_client.ios.IOSDriver;
import java.util.Collections;
import org.openqa.selenium.Capabilities;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.Platform;
import org.openqa.selenium.SearchContext;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebDriver.TargetLocator;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.WrapsDriver;
import utam.core.driver.Document;
import utam.core.driver.Driver;
import utam.core.driver.Navigation;
import utam.core.element.Element;
import utam.core.element.FrameElement;
import utam.core.framework.base.FrameElementImpl;
import utam.core.framework.base.PageObjectsFactory;
import utam.core.framework.base.PageObjectsFactoryImpl;
import utam.core.framework.consumer.PageObjectContext;
import utam.core.framework.consumer.PageObjectContextImpl;
import utam.core.framework.element.BasePageElement;
import utam.core.framework.element.DocumentObject;
import utam.core.framework.element.NavigationImpl;
import utam.core.selenium.appium.MobileDriverAdapter;
import utam.core.selenium.appium.MobileElementAdapter;
import utam.core.selenium.element.DriverAdapter;
import utam.core.selenium.element.ElementAdapter;
import utam.core.selenium.element.ShadowRootWebElement;
import utam.core.selenium.factory.WebDriverFactory;

/**
 * @author elizaveta.ivanova
 * @since 234
 */
public class MockUtilities {

  private final WebDriver webDriverMock;
  private final Driver driverAdapter;
  private final PageObjectsFactory factory;
  private final WebElement webElementMock;
  private final ElementAdapter elementAdapter;
  private final BasePageElement utamElement;
  private final FrameElement frameElement;
  private final WebDriver.Options optionsMock;
  private final WebDriver.Window windowMock;

  public MockUtilities(Class<? extends WebDriver> driverType) {
    webDriverMock =
        mock(
            driverType,
            withSettings().extraInterfaces(JavascriptExecutor.class, SearchContext.class));
    ;
    webElementMock = mock(WebElement.class, withSettings().extraInterfaces(WrapsDriver.class));
    when(((WrapsDriver) webElementMock).getWrappedDriver()).thenReturn(webDriverMock);
    driverAdapter = setDriverAdapter(driverType);
    PageObjectContext pageObjectContext = new PageObjectContextImpl(Collections.emptyMap());
    factory = new PageObjectsFactoryImpl(pageObjectContext, driverAdapter);
    elementAdapter = setElementAdapter(driverType);
    utamElement = createInstance(BasePageElement.class, elementAdapter, factory.getDriver());
    frameElement = createInstance(FrameElementImpl.class, elementAdapter, factory.getDriver());
    if (isMobileMock(driverType)) {
      setMobilePlatform(Platform.LINUX);
    }
    TargetLocator targetLocator = mock(TargetLocator.class);
    when(webDriverMock.switchTo()).thenReturn(targetLocator);

    optionsMock = mock(WebDriver.Options.class);
    windowMock = mock(WebDriver.Window.class);

    when(webDriverMock.manage()).thenReturn(optionsMock);
    when(webDriverMock.manage().window()).thenReturn(windowMock);
  }

  public MockUtilities() {
    this(WebDriver.class);
  }

  private static boolean isMobileMock(Class<? extends WebDriver> driverType) {
    return AppiumDriver.class.isAssignableFrom(driverType);
  }

  public MobileDriverAdapter getMobileDriverAdapter() {
    return (MobileDriverAdapter) driverAdapter;
  }

  ElementAdapter setElementAdapter(Class<? extends WebDriver> driverType) {
    return isMobileMock(driverType)
        ? new MobileElementAdapter(webElementMock, driverAdapter)
        : new ElementAdapter(webElementMock, driverAdapter);
  }

  DriverAdapter setDriverAdapter(Class<? extends WebDriver> driverType) {
    WebDriver driver = getWebDriverMock();
    if (driverType.equals(AppiumDriver.class)) {
      setMobilePlatform(Platform.LINUX);
    } else if (driverType.equals(IOSDriver.class)) {
      setMobilePlatform(Platform.IOS);
    } else if (driverType.equals(AndroidDriver.class)) {
      setMobilePlatform(Platform.ANDROID);
    }
    return (DriverAdapter) WebDriverFactory.getAdapterMock(driver);
  }

  public void setMobilePlatform(Platform platform) {
    AppiumDriver driver = (AppiumDriver) webDriverMock;
    Capabilities capabilities = mock(Capabilities.class);
    when(capabilities.getPlatform()).thenReturn(platform);
    when(driver.getCapabilities()).thenReturn(capabilities);
    when(driver.getSessionDetail("device")).thenReturn("iphone");
    when(driver.getSessionDetail("deviceScreenSize")).thenReturn("1080x1920");
    when(driver.getSessionDetail("deviceScreenDensity")).thenReturn("480");
  }

  public void setShadowMock(WebElement element, String cssSelector) {
    ShadowRootWebElement shadowRootWebElement = new ShadowRootWebElement(element);
    when(shadowRootWebElement
            .getExecutor()
            .executeScript(String.format(GET_SHADOW_ROOT_QUERY_SELECTOR_ALL, cssSelector), element))
        .thenReturn(Collections.singletonList(element));
    when(shadowRootWebElement
            .getExecutor()
            .executeScript(String.format(GET_SHADOW_ROOT_QUERY_SELECTOR, cssSelector), element))
        .thenReturn(element);
  }

  public WebDriver getWebDriverMock() {
    return webDriverMock;
  }

  public AppiumDriver getAppiumDriverMock() {
    return (AppiumDriver) webDriverMock;
  }

  public JavascriptExecutor getExecutorMock() {
    return (JavascriptExecutor) webDriverMock;
  }

  public Driver getDriverAdapter() {
    return driverAdapter;
  }

  public PageObjectsFactory getFactory() {
    return factory;
  }

  public WebElement getWebElementMock() {
    return webElementMock;
  }

  public Element getElementAdapter() {
    return elementAdapter;
  }

  public FrameElement getFrameElement() {
    return frameElement;
  }

  public Document getDocument() {
    return new DocumentObject(getFactory());
  }

  public Navigation getNavigation() {
    return new NavigationImpl(getFactory());
  }

  public BasePageElement getUtamElement() {
    return utamElement;
  }

  // sometimes Driver Adaptor should be mocked to intercept method calls
  public static class MockDriver extends MockUtilities {

    public MockDriver() {
      super();
    }

    public MockDriver(Class<? extends WebDriver> driverType) {
      super(driverType);
    }

    @Override
    DriverAdapter setDriverAdapter(Class<? extends WebDriver> driverType) {
      DriverAdapter driverAdapterMock =
          isMobileMock(driverType) ? mock(MobileDriverAdapter.class) : mock(DriverAdapter.class);
      when(driverAdapterMock.getSeleniumDriver()).thenReturn(getWebDriverMock());
      return driverAdapterMock;
    }
  }

  // sometimes Element Adaptor should be mocked to intercept method calls
  public static class MockAdapter extends MockUtilities {

    public MockAdapter() {
      super();
    }

    @Override
    ElementAdapter setElementAdapter(Class<? extends WebDriver> driverType) {
      ElementAdapter elementAdapter =
          isMobileMock(driverType) ? mock(MobileElementAdapter.class) : mock(ElementAdapter.class);
      when(elementAdapter.getWebElement()).thenReturn(getWebElementMock());
      return elementAdapter;
    }
  }
}
