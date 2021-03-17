package utam.core.framework.context;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.nullValue;
import static utam.core.framework.context.StringValueProfile.DEFAULT_PROFILE;

import org.testng.annotations.Test;
import utam.core.framework.consumer.TestLoaderConfigPageObject;
import utam.core.framework.consumer.TestLoaderConfigPageObjectOverride;

public class ProfileContextTests {

  @Test
  public void testEmptyProfileContextSetupOverride() {
    DefaultProfileContext profile = new DefaultProfileContext(DEFAULT_PROFILE);
    assertThat(profile.getBeanName(null), is(nullValue()));
    assertThat(profile.getBeanName(TestLoaderConfigPageObject.class), is(nullValue()));
    assertThat(profile.beans.isEmpty(), is(true));
    profile.setBean(TestLoaderConfigPageObject.class, TestLoaderConfigPageObject.class.getName());
    assertThat(profile.getBeanName(TestLoaderConfigPageObject.class),
        is(equalTo(TestLoaderConfigPageObject.class.getName())));
  }

   @Test
  public void testNotExistingConfig() {
    DefaultProfileContext profile = new DefaultProfileContext(new StringValueProfile("foo", "bar"));
    assertThat(profile.getBeanName(null), is(nullValue()));
    assertThat(profile.getBeanName(TestLoaderConfigPageObject.class), is(nullValue()));
    assertThat(profile.beans.isEmpty(), is(true));
  }

  @Test
  public void testExistingConfig() {
    // read beans from config test_profiles_config
    ProfileContext profileContext =
        new DefaultProfileContext(new StringValueProfile("test", "profiles"));
    assertThat(profileContext.getBeanName(TestLoaderConfigPageObject.class),
        is(equalTo(TestLoaderConfigPageObjectOverride.class.getName())));
  }
}
