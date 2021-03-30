package utam.core;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.withSettings;
import static utam.core.selenium.element.ShadowRootWebElement.GET_SHADOW_ROOT_QUERY_SELECTOR;
import static utam.core.selenium.element.ShadowRootWebElement.GET_SHADOW_ROOT_QUERY_SELECTOR_ALL;

import io.appium.java_client.AppiumDriver;
import java.util.Collections;
import org.openqa.selenium.Capabilities;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.Platform;
import org.openqa.selenium.SearchContext;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.WrapsDriver;
import utam.core.element.Element;
import utam.core.framework.consumer.PageObjectContext;
import utam.core.framework.consumer.PageObjectContextImpl;
import utam.core.driver.Driver;
import utam.core.driver.DriverContext;
import utam.core.framework.base.PageObjectsFactory;
import utam.core.framework.base.PageObjectsFactoryImpl;
import utam.core.framework.element.BasePageElement;
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
  private final DriverAdapter driverAdapterMock;
  private final PageObjectsFactory factory;
  private final WebElement webElementMock;
  private final ElementAdapter elementAdapter;
  private final BasePageElement utamElement;

  public MockUtilities(Class<? extends WebDriver> driverType) {
    webDriverMock = mock(driverType, withSettings().extraInterfaces(
        JavascriptExecutor.class,
        SearchContext.class));
    DriverContext driverContext = DriverContext.TEST;
    driverAdapter = WebDriverFactory.getAdapter(webDriverMock, driverContext);
    driverAdapterMock = mock(DriverAdapter.class);
    when(driverAdapterMock.getSeleniumDriver()).thenReturn(webDriverMock);
    PageObjectContext pageObjectContext = new PageObjectContextImpl(Collections.emptyMap());
    factory = new PageObjectsFactoryImpl(pageObjectContext, driverContext,
        driverAdapter);
    webElementMock = mock(WebElement.class, withSettings().extraInterfaces(WrapsDriver.class));
    when(((WrapsDriver) webElementMock).getWrappedDriver()).thenReturn(webDriverMock);
    elementAdapter = new ElementAdapter(webElementMock);
    utamElement = new BasePageElement(factory, elementAdapter);
    if(driverType.equals(AppiumDriver.class)) {
      setMobilePlatform(Platform.LINUX);
    }
  }

  public MockUtilities() {
    this(WebDriver.class);
  }

  public void setMobilePlatform(Platform platform) {
    AppiumDriver driver = (AppiumDriver) webDriverMock;
    Capabilities capabilities = mock(Capabilities.class);
    when(capabilities.getPlatform()).thenReturn(platform);
    when(driver.getCapabilities()).thenReturn(capabilities);
  }

  public void setShadowMock(WebElement element, String cssSelector) {
    ShadowRootWebElement shadowRootWebElement = new ShadowRootWebElement(element);
    when(shadowRootWebElement.getExecutor().executeScript(
        String.format(GET_SHADOW_ROOT_QUERY_SELECTOR_ALL, cssSelector), element))
        .thenReturn(Collections.singletonList(element));
    when(shadowRootWebElement.getExecutor().executeScript(
        String.format(GET_SHADOW_ROOT_QUERY_SELECTOR, cssSelector), element))
        .thenReturn(element);
  }

  public WebDriver getWebDriverMock() {
    return webDriverMock;
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

  public BasePageElement getUtamElement() {
    return utamElement;
  }

  public DriverAdapter getDriverAdapterMock() {
    return driverAdapterMock;
  }
}
