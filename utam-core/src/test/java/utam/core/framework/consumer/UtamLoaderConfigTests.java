package utam.core.framework.consumer;

import io.appium.java_client.android.AndroidDriver;
import java.time.Duration;
import java.util.ResourceBundle;
import utam.core.appium.context.AppiumContextProvider;
import org.openqa.selenium.WebDriver;
import org.testng.annotations.Test;
import utam.core.framework.consumer.UtamLoaderTests.PageObjectMockForProfile;
import utam.core.framework.consumer.impl.LoaderConfigTestImpl;
import utam.core.framework.consumer.impl.LoaderConfigTestImplIOS;
import utam.core.framework.context.Profile;
import utam.core.framework.context.StringValueProfile;
import utam.core.selenium.context.SeleniumContext;
import utam.core.selenium.context.SeleniumContextProvider;

import static org.hamcrest.CoreMatchers.*;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static org.mockito.Mockito.mock;
import static org.testng.Assert.assertThrows;

/**
 * @author elizaveta.ivanova
 * @since 230
 */
public class UtamLoaderConfigTests {

  @Test
  public void testSetBridgeApp() {
    AbstractUtamLoaderConfig config = new UtamLoaderEmptyConfig(mock(WebDriver.class));
    config.setBridgeAppTitle("bridge");
    assertThat(config.driverSettings.bridgeAppTitle, is(equalTo("bridge")));
  }

  @Test
  public void testSetTimeout() {
    AbstractUtamLoaderConfig config = new UtamLoaderEmptyConfig(mock(WebDriver.class));
    config.setTimeout(Duration.ofSeconds(100));
    assertThat(config.driverSettings.customTimeout, is(equalTo(Duration.ofSeconds(100))));
  }

  @Test
  public void testSetLocationPolicy() {
    AbstractUtamLoaderConfig config = new UtamLoaderEmptyConfig(mock(WebDriver.class));
    assertThat(
        config.getSeleniumContext().getLocationPolicy(), is(equalTo(LocationPolicyType.CHAIN)));
    config.setLocationPolicy(LocationPolicyType.JAVASCRIPT);
    assertThat(
        config.getSeleniumContext().getLocationPolicy(),
        is(equalTo(LocationPolicyType.JAVASCRIPT)));
  }

  @Test
  public void testResetSettingsAreLazy() {
    AbstractUtamLoaderConfig config = new UtamLoaderEmptyConfig(mock(WebDriver.class));
    config.resetNotNullFactory();
    config.resetNotNullSeleniumContext();
    assertThat(config.seleniumContext, is(nullValue()));
    assertThat(config.pageObjectsFactory, is(nullValue()));
  }

  @Test
  public void testGetFactory() {
    WebDriver driver = mock(WebDriver.class);
    AbstractUtamLoaderConfig config = new UtamLoaderEmptyConfig(driver);
    assertThat(config.getFactory(), is(notNullValue()));
    config = new UtamLoaderBundleConfig(driver, ResourceBundle::getBundle);
    assertThat(config.getFactory(), is(notNullValue()));
    config = new UtamLoaderModulesConfig(driver, this.getClass());
    assertThat(config.getFactory(), is(notNullValue()));
  }

  @Test
  public void testSetSeleniumContext() {
    AbstractUtamLoaderConfig config = new UtamLoaderEmptyConfig(mock(WebDriver.class));
    SeleniumContext context = config.getSeleniumContext();
    //can't use instance of here because it would be true for AppiumContextProvider as well
    assertThat(context.getClass(), is(equalTo(SeleniumContextProvider.class)));
  }

  @Test
  public void testSetMobileSeleniumContext() {
    AbstractUtamLoaderConfig config = new UtamLoaderEmptyConfig(mock(AndroidDriver.class));
    SeleniumContext context = config.getSeleniumContext();
    assertThat(context, is(instanceOf(AppiumContextProvider.class)));
  }

  @Test
  public void testEmptyConfigOverride() {
    AbstractUtamLoaderConfig config = new UtamLoaderEmptyConfig(mock(WebDriver.class));
    Profile profile = new StringValueProfile("profile", "name");
    config.setProfileOverride(profile, PageObjectMock.class, PageObjectMockForProfile.class);
    config.setProfileOverride(profile, LoaderConfigTest.class, LoaderConfigTestImpl.class);
    assertThat(config.overrideProfiles, is(hasSize(1)));
    assertThat(config.overrideProfilesContext.values(), is(hasSize(1)));
    PageObjectContext context = config.setPageObjectsContext();
    assertThat(context.getBean(PageObjectMock.class), is(instanceOf(PageObjectMockForProfile.class)));
    assertThat(context.getBean(LoaderConfigTest.class), is(instanceOf(LoaderConfigTestImpl.class)));
  }

  @Test
  public void testEmptyConfigOverrideWithNull() {
    AbstractUtamLoaderConfig config = new UtamLoaderEmptyConfig(mock(WebDriver.class));
    config.setProfileOverride(null, PageObjectMock.class, PageObjectMockForProfile.class);
    config.setProfileOverride(null, LoaderConfigTest.class, LoaderConfigTestImpl.class);
    assertThat(config.overrideProfiles, is(hasSize(1)));
    assertThat(config.overrideProfilesContext.values(), is(hasSize(1)));
    PageObjectContext context = config.setPageObjectsContext();
    assertThat(context.getBean(PageObjectMock.class), is(instanceOf(PageObjectMockForProfile.class)));
    assertThat(context.getBean(LoaderConfigTest.class), is(instanceOf(LoaderConfigTestImpl.class)));
  }

  @Test
  public void testEmptyConfigOverrideThrows() {
    AbstractUtamLoaderConfig config = new UtamLoaderEmptyConfig(mock(WebDriver.class));
    assertThrows(() -> config.setProfile(null));
  }

  @Test
  public void testOverride() {
    WebDriver driver = mock(WebDriver.class);
    Profile iosProfile = new StringValueProfile("platform", "ios");
    AbstractUtamLoaderConfig config = new UtamLoaderBundleConfig(driver, ResourceBundle::getBundle);
    config.setProfile(iosProfile);
    config.setProfileOverride(iosProfile, LoaderConfigTest.class, LoaderConfigTestImplIOS.class);
    assertThat(config.setPageObjectsContext().getBean(LoaderConfigTest.class), is(instanceOf(LoaderConfigTestImplIOS.class)));
    config = new UtamLoaderModulesConfig(driver, this.getClass());
    config.setProfile(iosProfile);
    config.setProfileOverride(iosProfile, LoaderConfigTest.class, LoaderConfigTestImplIOS.class);
    assertThat(config.setPageObjectsContext().getBean(LoaderConfigTest.class), is(instanceOf(LoaderConfigTestImplIOS.class)));
  }


}
