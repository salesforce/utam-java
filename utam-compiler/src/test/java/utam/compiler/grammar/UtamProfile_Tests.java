/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.sameInstance;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.testng.Assert.expectThrows;
import static utam.compiler.grammar.TestUtilities.TEST_URI;
import static utam.compiler.grammar.TestUtilities.getDeserializedObject;
import static utam.compiler.grammar.TestUtilities.getTestTranslationContext;
import static utam.compiler.grammar.UtamProfile.ERR_DUPLICATE_PROFILE_VALUE;
import static utam.compiler.grammar.UtamProfile.ERR_INVALID_ARRAY_TYPES;
import static utam.compiler.grammar.UtamProfile.ERR_PROFILE_VALUE_WRONG_FORMAT;
import static utam.compiler.grammar.UtamProfile.getPageObjectProfiles;
import static utam.compiler.translator.TranslatorMockUtilities.getDefaultConfig;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.hamcrest.Matchers;
import org.testng.annotations.Test;
import utam.compiler.grammar.DeserializerUtilities.Result;
import utam.compiler.helpers.TranslationContext;
import utam.compiler.translator.DefaultTranslatorConfiguration;
import utam.compiler.translator.StringValueProfileConfig;
import utam.core.declarative.translator.ProfileConfiguration;
import utam.core.declarative.translator.TranslatorConfig;
import utam.core.framework.consumer.UtamError;
import utam.core.framework.context.Profile;
import utam.core.framework.context.StringValueProfile;

/**
 * @author elizaveta.ivanova
 * @since 228
 */
public class UtamProfile_Tests {

  static final String PROFILE_KEY = "profileConfigKey";
  static final String PROFILE_VALUE = "profileConfigValue";

  private static final ProfileConfiguration DRIVER_PROFILE =
      new StringValueProfileConfig("driver", new String[]{"chrome", "firefox"});

  private static void deserialize(String json) {
    getDeserializedObject(json, UtamProfile.class);
  }

  @Test
  public void testGetProfileWhenConfigured() {
    DefaultTranslatorConfiguration config = getDefaultConfig();
    config.setConfiguredProfile(DRIVER_PROFILE);
    TranslationContext translationInstantContext = new TranslationContext(TEST_URI, config);
    final String DRIVER_KEY = "driver";
    final String CHROME_VALUE = "chrome";
    List<Profile> profiles = new UtamProfile(DRIVER_KEY, CHROME_VALUE)
        .getProfiles(translationInstantContext);
    assertThat(profiles.size(), is(1));
    Profile profile = profiles.get(0);
    assertThat(profile.getName(), is(equalTo(DRIVER_KEY)));
    assertThat(profile.getValue(), is(equalTo(CHROME_VALUE)));
  }

  @Test
  public void testGetDefaultProfile() {
    Profile mockProfile = mock(Profile.class);
    ProfileConfiguration profileConfiguration = mock(ProfileConfiguration.class);
    when(profileConfiguration.getPropertyKey()).thenReturn(PROFILE_KEY);
    when(profileConfiguration.getFromString(PROFILE_VALUE)).thenReturn(mockProfile);
    DefaultTranslatorConfiguration translatorConfig = getDefaultConfig();
    translatorConfig.setConfiguredProfile(profileConfiguration);
    TranslationContext context = new TranslationContext(TEST_URI, translatorConfig);
    assertThat(
        context.getProfile(PROFILE_KEY, PROFILE_VALUE), Matchers.is(sameInstance(mockProfile)));
  }

  @Test
  public void testGetPageObjectProfilesWithNullProfile() {
    TranslationContext context = getTestTranslationContext();
    UtamPageObject pageObject = new UtamPageObject();
    assertThat(getPageObjectProfiles(pageObject.profiles, context), hasSize(0));
  }

  @Test
  public void testDeserializerWithCorrectProfiles() {
    DeserializerUtilities utilities = new DeserializerUtilities();
    TranslatorConfig translatorConfig = utilities.getTranslatorConfig();
    ProfileConfiguration platform = new StringValueProfileConfig("platform", "android");
    translatorConfig.getConfiguredProfiles().add(platform);
    ProfileConfiguration devices = new StringValueProfileConfig("device",
        new String[]{"phone", "tablet"});
    translatorConfig.getConfiguredProfiles().add(devices);
    Result res = utilities.getResultFromFile("pageobjects/profiles");
    List<Profile> profiles = res.getPageObject().getImplementation().getProfiles();
    assertThat(profiles, hasSize(3));
    Profile platformProfile = profiles.get(0);
    assertThat(platformProfile, is(equalTo(new StringValueProfile("platform", "android"))));
    Profile devicePhone = profiles.get(1);
    assertThat(devicePhone, is(equalTo(new StringValueProfile("device", "phone"))));
    Profile deviceTablet = profiles.get(2);
    assertThat(deviceTablet, is(equalTo(new StringValueProfile("device", "tablet"))));
  }

  @Test
  public void testDeserializerWrongFormatThrows() {
    UtamError e = expectThrows(UtamError.class, () -> deserialize("error"));
    assertThat(e.getCause().getMessage(), containsString("Unrecognized token"));
  }

  @Test
  public void testDeserializerWrongValueFormatThrows() {
    String json = " { \"name\" : {}}";
    UtamError e = expectThrows(UtamError.class, () -> deserialize(json));
    assertThat(e.getMessage(), is(equalTo(String.format(ERR_PROFILE_VALUE_WRONG_FORMAT, "name"))));
  }

  @Test
  public void testDeserializerWrongValueArrayFormatThrows() {
    String json = " { \"name\" : [1] }";
    UtamError e = expectThrows(UtamError.class, () -> deserialize(json));
    assertThat(e.getMessage(), is(equalTo(String.format(ERR_INVALID_ARRAY_TYPES, "name"))));
  }

  @Test
  public void testDeserializerDuplicateValueThrows() {
    String json = new JsonStringBuilder("same", "same").getJsonString();
    UtamError e = expectThrows(UtamError.class, () -> deserialize(json));
    assertThat(e.getMessage(),
        is(equalTo(String.format(ERR_DUPLICATE_PROFILE_VALUE, "name", "same"))));
  }

  private static class JsonStringBuilder {

    private final String jsonString;

    private JsonStringBuilder(String... values) {
      String valuesStr = Stream.of(values).map(str -> String.format("\"%s\"", str)).collect(
          Collectors.joining(", "));
      String content = String.format("\"name\" : [ %s ] ", valuesStr);
      jsonString = String.format("{ %s }", content);
    }

    String getJsonString() {
      return jsonString;
    }
  }
}
