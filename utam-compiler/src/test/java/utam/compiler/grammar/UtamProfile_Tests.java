package utam.compiler.grammar;

import utam.compiler.helpers.TranslationContext;
import utam.core.declarative.translator.ProfileConfiguration;
import utam.compiler.translator.StringValueProfileConfig;
import utam.core.declarative.translator.TranslatorConfig;
import utam.core.framework.context.Profile;
import utam.core.framework.context.StringValueProfile;
import org.hamcrest.Matchers;
import org.testng.annotations.Test;

import static utam.compiler.grammar.TestUtilities.TEST_URI;
import static utam.compiler.translator.TranslatorMockUtilities.getDefaultConfig;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.sameInstance;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * @author elizaveta.ivanova
 * @since 228
 */
public class UtamProfile_Tests {

  static final String PROFILE_KEY = "profileConfigKey";
  static final String PROFILE_VALUE = "profileConfigValue";
  private static final ProfileConfiguration DRIVER_PROFILE =
      new StringValueProfileConfig("driver", new String[] {"chrome", "firefox"});
  private static final Profile MOCK_PROFILE = new StringValueProfile(PROFILE_KEY, PROFILE_VALUE);

  static TranslationContext getContextWithProfile() {
    TranslatorConfig config = getDefaultConfig();
    ProfileConfiguration profileConfiguration = mock(ProfileConfiguration.class);
    when(profileConfiguration.getPropertyKey()).thenReturn(PROFILE_KEY);
    when(profileConfiguration.getFromString(PROFILE_VALUE))
        .thenReturn(UtamProfile_Tests.MOCK_PROFILE);
    config.setConfiguredProfile(profileConfiguration);
    return new TranslationContext(TEST_URI, config);
  }

  @Test
  public void testDriverProfile() {
    TranslatorConfig config = getDefaultConfig();
    config.setConfiguredProfile(DRIVER_PROFILE);
    TranslationContext translationInstantContext = new TranslationContext(TEST_URI, config);
    final String DRIVER_KEY = "driver";
    final String CHROME_VALUE = "chrome";
    Profile profile =
        new UtamProfile(DRIVER_KEY, CHROME_VALUE).getProfile(translationInstantContext);
    assertThat(profile.getName(), is(equalTo(DRIVER_KEY)));
    assertThat(profile.getValue(), is(equalTo(CHROME_VALUE)));
  }

  @Test
  public void testGetDefaultProfile() {
    Profile mockProfile = mock(Profile.class);
    when(mockProfile.isDefault()).thenReturn(true);
    ProfileConfiguration profileConfiguration = mock(ProfileConfiguration.class);
    when(profileConfiguration.getPropertyKey()).thenReturn(PROFILE_KEY);
    when(profileConfiguration.getFromString(PROFILE_VALUE)).thenReturn(mockProfile);
    TranslatorConfig translatorConfig = getDefaultConfig();
    translatorConfig.setConfiguredProfile(profileConfiguration);
    TranslationContext context = new TranslationContext(TEST_URI, translatorConfig);
    assertThat(
        context.getProfile(PROFILE_KEY, PROFILE_VALUE), Matchers.is(sameInstance(mockProfile)));
  }
}
