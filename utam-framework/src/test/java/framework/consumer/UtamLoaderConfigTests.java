package framework.consumer;

import appium.context.AppiumContextProvider;
import io.appium.java_client.AppiumDriver;
import org.openqa.selenium.WebDriver;
import org.testng.annotations.Test;
import selenium.context.SeleniumContext;
import selenium.context.SeleniumContextProvider;

import static org.hamcrest.CoreMatchers.*;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.Mockito.mock;
import static org.testng.Assert.assertThrows;

/**
 * @author elizaveta.ivanova
 * @since 230
 */
public class UtamLoaderConfigTests {

  static UtamLoaderConfigImpl getConfig() {
    return new UtamLoaderConfigImpl();
  }

  @Test
  public void testSetBridgeApp() {
    UtamLoaderConfigImpl config = getConfig();
    config.setBridgeAppTitle("bridge");
    assertThat(config.bridgeAppTitle, is(equalTo("bridge")));
  }

  @Test
  public void testGettingContextWithoutSettingDriverThrows() {
    UtamLoaderConfigImpl config = getConfig();
    assertThrows(config::getSeleniumContext);
    assertThrows(config::getFactory);
  }

  @Test
  public void testSetLocationPolicy() {
    UtamLoaderConfigImpl config = getConfig();
    config.setDriver(mock(WebDriver.class));
    assertThat(
        config.getSeleniumContext().getLocationPolicy(), is(equalTo(LocationPolicyType.CHAIN)));
    config.setLocationPolicy(LocationPolicyType.JAVASCRIPT);
    assertThat(
        config.getSeleniumContext().getLocationPolicy(),
        is(equalTo(LocationPolicyType.JAVASCRIPT)));
  }

  @Test
  public void testResetSettingsAreLazy() {
    UtamLoaderConfigImpl config = getConfig();
    config.resetSettings();
    assertThat(config.seleniumContext, is(nullValue()));
    assertThat(config.pageObjectsFactory, is(nullValue()));
  }

  @Test
  public void testGetFactory() {
    UtamLoaderConfigImpl config = getConfig();
    config.setDriver(mock(WebDriver.class));
    assertThat(config.getFactory(), is(notNullValue()));
  }

  @Test
  public void testSetSeleniumContext() {
    UtamLoaderConfigImpl config = getConfig();
    config.setDriver(mock(WebDriver.class));
    SeleniumContext context = config.getSeleniumContext();
    //can't use instance of here because it would be true for AppiumContextProvider as well
    assertThat(context.getClass(), is(equalTo(SeleniumContextProvider.class)));
  }

  @Test
  public void testSetMobileSeleniumContext() {
    UtamLoaderConfigImpl config = new UtamLoaderConfigImpl();
    config.setDriver(mock(AppiumDriver.class));
    SeleniumContext context = config.getSeleniumContext();
    assertThat(context, is(instanceOf(AppiumContextProvider.class)));
  }
}
