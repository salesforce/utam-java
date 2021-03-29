package utam.core.framework.consumer;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static org.mockito.Mockito.mock;
import static org.testng.Assert.assertThrows;

import io.appium.java_client.android.AndroidDriver;
import java.time.Duration;
import org.openqa.selenium.WebDriver;
import org.testng.annotations.Test;
import utam.core.appium.context.AppiumContextProvider;
import utam.core.framework.base.RootPageObject;
import utam.core.framework.context.Profile;
import utam.core.framework.context.StringValueProfile;
import utam.core.selenium.context.SeleniumContext;
import utam.core.selenium.context.SeleniumContextProvider;

public class UtamLoaderConfigTests {

  private static UtamLoaderConfigImpl getDefaultConfig() {
    return new UtamLoaderConfigImpl(mock(WebDriver.class));
  }

  @Test
  public void testSetBridgeApp() {
    UtamLoaderConfigImpl config = getDefaultConfig();
    config.setBridgeAppTitle("bridge");
    assertThat(config.driverSettings.bridgeAppTitle, is(equalTo("bridge")));
  }

  @Test
  public void testSetTimeout() {
    UtamLoaderConfigImpl config = getDefaultConfig();
    config.setTimeout(Duration.ofSeconds(100));
    assertThat(config.driverSettings.timeouts, is(equalTo(Duration.ofSeconds(100))));
  }

  @Test
  public void testSetLocationPolicy() {
    UtamLoaderConfigImpl config = getDefaultConfig();
    assertThat(
        config.getSeleniumContext().getLocationPolicy(), is(equalTo(LocationPolicyType.CHAIN)));
    config.setLocationPolicy(LocationPolicyType.JAVASCRIPT);
    assertThat(
        config.getSeleniumContext().getLocationPolicy(),
        is(equalTo(LocationPolicyType.JAVASCRIPT)));
  }

  @Test
  public void testResetSettingsAreLazy() {
    UtamLoaderConfigImpl config = getDefaultConfig();
    config.resetFactory();
    config.resetSeleniumContext();
    assertThat(config.seleniumContext, is(nullValue()));
    assertThat(config.pageObjectsFactory, is(nullValue()));
  }

  @Test
  public void testGetFactory() {
    UtamLoaderConfigImpl config = getDefaultConfig();
    assertThat(config.getFactory(), is(notNullValue()));
  }

  @Test
  public void testSetSeleniumContext() {
    UtamLoaderConfigImpl config = getDefaultConfig();
    SeleniumContext context = config.getSeleniumContext();
    //can't use instance of here because it would be true for AppiumContextProvider as well
    assertThat(context.getClass(), is(equalTo(SeleniumContextProvider.class)));
  }

  @Test
  public void testSetMobileSeleniumContext() {
    UtamLoaderConfigImpl config = new UtamLoaderConfigImpl(mock(AndroidDriver.class));
    SeleniumContext context = config.getSeleniumContext();
    assertThat(context, is(instanceOf(AppiumContextProvider.class)));
  }

  @Test
  public void testDefaultConfigOverride() {
    UtamLoaderConfigImpl config = getDefaultConfig();
    Profile profile = new StringValueProfile("profile", "override");
    config.setProfileOverride(profile, TestLoaderConfigPageObject.class, TestLoaderConfigPageObject.class);
    config.setProfileOverride(profile, TestLoaderConfigPageObject.class,
        TestLoaderConfigPageObjectOverride.class);
    assertThat(config.overrideProfiles, is(hasSize(1)));
    assertThat(config.overrideProfilesContext.values(), is(hasSize(1)));
    PageObjectContext context = config.setPageObjectsContext();
    assertThat(context.getBean(TestLoaderConfigPageObject.class),
        is(instanceOf(TestLoaderConfigPageObjectOverride.class)));
  }

  @Test
  public void testDefaultConfigOverrideWithNull() {
    UtamLoaderConfigImpl config = getDefaultConfig();
    config.setProfileOverride(null, TestLoaderConfigPageObject.class, TestLoaderConfigPageObject.class);
    config.setProfileOverride(null, TestLoaderConfigPageObject.class,
        TestLoaderConfigPageObjectOverride.class);
    assertThat(config.overrideProfiles, is(hasSize(1)));
    assertThat(config.overrideProfilesContext.values(), is(hasSize(1)));
    PageObjectContext context = config.setPageObjectsContext();
    assertThat(context.getBean(TestLoaderConfigPageObject.class), is(instanceOf(
        TestLoaderConfigPageObjectOverride.class)));
  }

  @Test
  public void testEmptyConfigOverrideThrows() {
    UtamLoaderConfig config = getDefaultConfig();
    assertThrows(() -> config.setProfile(null));
  }

  @Test
  public void testThatHardcodedBeanOverrides() {
    UtamLoaderConfigImpl config = new UtamLoaderConfigImpl(mock(WebDriver.class));
    config.setProfile(new StringValueProfile("test", "profiles"));
    assertThat(config.setPageObjectsContext().getBean(TestLoaderConfigPageObject.class),
        is(instanceOf(
            TestLoaderConfigPageObjectOverride.class)));
    config.setProfileOverride(new StringValueProfile("another", "profiles"), TestLoaderConfigPageObject.class,
        TestLoaderConfigPageObjectIOS.class);
    assertThat(config.setPageObjectsContext().getBean(TestLoaderConfigPageObject.class),
        is(instanceOf(
            TestLoaderConfigPageObjectIOS.class)));
  }

  @Test
  public void testSetDefault() {
    UtamLoaderConfigImpl config = new UtamLoaderConfigImpl(mock(WebDriver.class));
    config.setDefaultProfile();
  }


  @Test
  public void testHardcodedOverride() {
    UtamLoaderConfig config = new UtamLoaderConfigImpl(mock(WebDriver.class));
    Profile profile = new StringValueProfile("profile", "override");
    config.setProfileOverride(profile, TestLoaderConfigPageObject.class, UtamLoaderTests.TestLoaderConfigPageObjectOverride.class);
    UtamLoader loader = new UtamLoaderImpl(config);
    RootPageObject rootPageObject = loader.load(TestLoaderConfigPageObject.class);
    assertThat(rootPageObject, is(instanceOf(UtamLoaderTests.TestLoaderConfigPageObjectOverride.class)));
  }
}
