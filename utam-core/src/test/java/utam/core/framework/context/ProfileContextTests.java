package utam.core.framework.context;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.nullValue;

import java.util.ResourceBundle;
import org.testng.annotations.Test;
import utam.core.framework.consumer.LoaderConfigTest;

public class ProfileContextTests {

  @Test
  public void testEmptyProfileContext() {
    AbstractProfileContext profile = new ProfileContextEmpty();
    assertThat(profile.getBeanName(null), is(nullValue()));
    assertThat(profile.getBeanName(LoaderConfigTest.class), is(nullValue()));
    assertThat(profile.beans.isEmpty(), is(true));
    profile.setBean(LoaderConfigTest.class, LoaderConfigTest.class.getName());
    assertThat(profile.getBeanName(LoaderConfigTest.class),
        is(equalTo(LoaderConfigTest.class.getName())));
  }

  @Test
  public void testWithBundle_NotExistingConfig() {
    final AbstractProfileContext profile =
        new ProfileContextBundle(new StringValueProfile("foo", "bar"), ResourceBundle::getBundle);
    assertThat(profile.getBeanName(null), is(nullValue()));
    assertThat(profile.getBeanName(LoaderConfigTest.class), is(nullValue()));
    assertThat(profile.beans.isEmpty(), is(true));
  }

  @Test
  public void testWithLoader_NotExistingConfig() {
    final AbstractProfileContext profile =
        new ProfileContextModule(new StringValueProfile("foo", "bar"), this.getClass());
    assertThat(profile.getBeanName(null), is(nullValue()));
    assertThat(profile.getBeanName(LoaderConfigTest.class), is(nullValue()));
    assertThat(profile.beans.isEmpty(), is(true));
  }

  @Test
  public void testWithBundle_Existing() {
    // read beans from config test_profiles_config
    AbstractProfileContext profileContext =
        new ProfileContextBundle(new StringValueProfile("test", "profiles"),
            ResourceBundle::getBundle);
    assertThat(profileContext.getBeanName(LoaderConfigTest.class),
        is(equalTo(LoaderConfigTest.class.getName())));
  }

  @Test
  public void testWithLoader_Existing() {
    // read beans from config test_profiles_config
    AbstractProfileContext profileContext =
        new ProfileContextModule(new StringValueProfile("test", "profiles"), this.getClass());
    assertThat(profileContext.getBeanName(LoaderConfigTest.class),
        is(equalTo(LoaderConfigTest.class.getName())));
  }
}
