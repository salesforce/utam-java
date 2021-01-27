package framework.context;

import framework.base.PageObject;
import framework.consumer.UtamError;
import org.hamcrest.Matchers;
import org.testng.annotations.Test;

import java.util.ResourceBundle;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.testng.Assert.expectThrows;

public class DefaultProfileContextTests {

  private static DefaultProfileContext getProfileContext() {
    DefaultProfileContext profile = new MockProfileContext();
    profile.setBean(FakePageObject.class, FakePageObject.class.getName());
    return profile;
  }

  /** The getConfig method should return the proper config value */
  @Test
  public void testGetConfig() {
    DefaultProfileContext profile = getProfileContext();
    assertThat(profile.getBeanName(FakePageObject.class), is(not(nullValue())));
  }

  /** The getConfig method should return null when passed a null profile value */
  @Test
  public void testGetConfigWithNullProfileValueReturnsNull() {
    DefaultProfileContext profile = new DefaultProfileContext(getTestProfile());
    assertThat(profile.getBeanName(null), is(nullValue()));
  }

  /** The getConfig method should return null when passed a nonexistent profile value */
  @Test
  public void testGetConfigWithNonexistentProfileValueReturnsNull() {
    DefaultProfileContext profile = new DefaultProfileContext(getTestProfile());
    assertThat(profile.getBeanName(FakePageObject.class), is(nullValue()));
  }

  /** The equals method should return true for equivalent instances */
  @Test
  public void testEquals() {
    DefaultProfileContext profile = getProfileContext();
    assertThat(profile.equals(new MockProfileContext()), is(equalTo(true)));
  }
  
  @Test
  public void testGetInjectionConfigWriterWithNullPathThrows() {
    DefaultProfileContext profile = new DefaultProfileContext(getTestProfile());
    UtamError e = expectThrows(
        UtamError.class,
        () -> profile.getInjectionConfigWriter(null));
    assertThat(e.getMessage(), containsString("profile config path is null or empty"));
  }
  
  @Test
  public void testGetInjectionConfigWriterWithEmptyPathThrows() {
    DefaultProfileContext profile = new DefaultProfileContext(getTestProfile());
    UtamError e = expectThrows(
        UtamError.class,
        () -> profile.getInjectionConfigWriter(""));
    assertThat(e.getMessage(), containsString("profile config path is null or empty"));
  }

  @Test
  public void testProfileFromProperties_NotExisting() {
    final DefaultProfileContext profileContext =
        new DefaultProfileContext(getTestProfile(), ResourceBundle::getBundle);
    assertThat(profileContext.beans.isEmpty(), is(true));
  }

  @Test
  public void testProfileFromProperties_Existing() {
    // read beans from config test_profiles_config
    final DefaultProfileContext profileContext =
        new DefaultProfileContext(
            getTestProfile("test", "profiles"), ResourceBundle::getBundle);
    assertThat(profileContext.beans.size(), is(1));
    assertThat(profileContext.beans.keySet(), is(Matchers.contains(this.getClass())));
    assertThat(profileContext.beans.values(), is(Matchers.contains(this.getClass().getName())));
  }

  private static Profile getTestProfile() {
    return getTestProfile("foo", "bar");
  }

  private static Profile getTestProfile(String name, String value) {
    Profile testProfile = mock(Profile.class);
    when(testProfile.getName()).thenReturn(name);
    when(testProfile.getValue()).thenReturn(value);
    when(testProfile.isDefault()).thenReturn(false);
    return testProfile;
  }

  private static class MockProfileContext extends DefaultProfileContext {
    MockProfileContext() {
      super(getTestProfile());
    }
  }

  private static class FakePageObject implements PageObject {}
}
