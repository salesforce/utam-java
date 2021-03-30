package utam.core.framework.consumer;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static org.testng.Assert.assertThrows;

import java.time.Duration;
import org.testng.annotations.Test;
import utam.core.MockUtilities;
import utam.core.framework.context.Profile;
import utam.core.framework.context.StringValueProfile;
import utam.core.driver.DriverTimeouts;
import utam.core.framework.base.RootPageObject;

public class UtamLoaderConfigTests {

  private static UtamLoaderConfigImpl getDefaultConfig() {
    return new UtamLoaderConfigImpl();
  }

  @Test
  public void testSetBridgeApp() {
    UtamLoaderConfigImpl config = getDefaultConfig();
    config.setBridgeAppTitle("bridge");
    assertThat(config.getDriverContext().getBridgeAppTitle(), is(equalTo("bridge")));
  }

  @Test
  public void testSetTimeout() {
    UtamLoaderConfigImpl config = getDefaultConfig();
    Duration test = Duration.ofSeconds(100);
    config.setFindTimeout(test);
    config.setWaitForTimeout(test);
    config.setPollingInterval(test);
    assertThat(config.getDriverContext().getTimeouts(),
        is(equalTo(new DriverTimeouts(test, test, test))));
  }

  @Test
  public void testDefaultConfigOverride() {
    UtamLoaderConfigImpl config = getDefaultConfig();
    Profile profile = new StringValueProfile("profile", "override");
    config.setProfileOverride(profile, TestLoaderConfigPageObject.class,
        TestLoaderConfigPageObject.class);
    config.setProfileOverride(profile, TestLoaderConfigPageObject.class,
        TestLoaderConfigPageObjectOverride.class);
    assertThat(config.overrideProfiles, is(hasSize(1)));
    assertThat(config.overrideProfilesContext.values(), is(hasSize(1)));
    PageObjectContext context = config.getPageContext();
    assertThat(context.getBean(TestLoaderConfigPageObject.class),
        is(instanceOf(TestLoaderConfigPageObjectOverride.class)));
  }

  @Test
  public void testDefaultConfigOverrideWithNull() {
    UtamLoaderConfigImpl config = getDefaultConfig();
    config.setProfileOverride(null, TestLoaderConfigPageObject.class,
        TestLoaderConfigPageObject.class);
    config.setProfileOverride(null, TestLoaderConfigPageObject.class,
        TestLoaderConfigPageObjectOverride.class);
    assertThat(config.overrideProfiles, is(hasSize(1)));
    assertThat(config.overrideProfilesContext.values(), is(hasSize(1)));
    PageObjectContext context = config.getPageContext();
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
    UtamLoaderConfigImpl config = getDefaultConfig();
    config.setProfile(new StringValueProfile("test", "profiles"));
    assertThat(config.getPageContext().getBean(TestLoaderConfigPageObject.class),
        is(instanceOf(
            TestLoaderConfigPageObjectOverride.class)));
    config.setProfileOverride(new StringValueProfile("another", "profiles"),
        TestLoaderConfigPageObject.class,
        TestLoaderConfigPageObjectIOS.class);
    assertThat(config.getPageContext().getBean(TestLoaderConfigPageObject.class),
        is(instanceOf(
            TestLoaderConfigPageObjectIOS.class)));
  }


  @Test
  public void testHardcodedOverride() {
    UtamLoaderConfig config = getDefaultConfig();
    Profile profile = new StringValueProfile("profile", "override");
    config.setProfileOverride(profile, TestLoaderConfigPageObject.class,
        UtamLoaderTests.TestLoaderConfigPageObjectOverride.class);
    UtamLoader loader = new UtamLoaderImpl(config, new MockUtilities().getDriverAdapter());
    RootPageObject rootPageObject = loader.load(TestLoaderConfigPageObject.class);
    assertThat(rootPageObject,
        is(instanceOf(UtamLoaderTests.TestLoaderConfigPageObjectOverride.class)));
  }
}
