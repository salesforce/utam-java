package utam.core.framework.consumer;

import java.time.Duration;
import java.util.Properties;
import utam.core.appium.context.AppiumContextProvider;
import io.appium.java_client.AppiumDriver;
import org.openqa.selenium.WebDriver;
import org.testng.annotations.Test;
import utam.core.framework.context.DefaultProfileContext;
import utam.core.framework.context.Profile;
import utam.core.framework.context.ProfileContext;
import utam.core.framework.context.StringValueProfile;
import utam.core.selenium.context.SeleniumContext;
import utam.core.selenium.context.SeleniumContextProvider;

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
    return new UtamLoaderConfigImpl(mock(WebDriver.class));
  }

  @Test
  public void testSetBridgeApp() {
    UtamLoaderConfigImpl config = getConfig();
    config.setBridgeAppTitle("bridge");
    assertThat(config.driverSettings.bridgeAppTitle, is(equalTo("bridge")));
  }

  @Test
  public void testSetTimeout() {
    UtamLoaderConfigImpl config = getConfig();
    config.setTimeout(Duration.ofSeconds(100));
    assertThat(config.driverSettings.customTimeout, is(equalTo(Duration.ofSeconds(100))));
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
    config.resetNotNullFactory();
    config.resetNotNullSeleniumContext();
    assertThat(config.seleniumContext, is(nullValue()));
    assertThat(config.pageObjectsFactory, is(nullValue()));
  }

  @Test
  public void testGetFactory() {
    UtamLoaderConfigImpl config = getConfig();
    assertThat(config.getFactory(), is(notNullValue()));
  }

  @Test
  public void testSetSeleniumContext() {
    UtamLoaderConfigImpl config = getConfig();
    SeleniumContext context = config.getSeleniumContext();
    //can't use instance of here because it would be true for AppiumContextProvider as well
    assertThat(context.getClass(), is(equalTo(SeleniumContextProvider.class)));
  }

  @Test
  public void testSetMobileSeleniumContext() {
    UtamLoaderConfigImpl config = new UtamLoaderConfigImpl(mock(AppiumDriver.class));
    SeleniumContext context = config.getSeleniumContext();
    assertThat(context, is(instanceOf(AppiumContextProvider.class)));
  }

  @Test
  public void testMultipleSources() {
    UtamLoaderConfigImpl config = new UtamLoaderConfigImpl(mock(WebDriver.class), this.getClass());
    Profile iosProfile = new StringValueProfile("platform", "ios");
    config.setActiveProfile(iosProfile);
    config.setPageObjectsContext().getBean(LoaderConfigTest.class);
  }


}
