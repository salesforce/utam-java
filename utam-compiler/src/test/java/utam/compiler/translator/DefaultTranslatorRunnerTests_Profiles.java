/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.translator;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.aMapWithSize;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.sameInstance;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.testng.Assert.assertThrows;
import static org.testng.Assert.expectThrows;
import static utam.compiler.translator.DefaultTranslatorRunner.DUPLICATE_IMPL_WITH_PROFILE_ERR;
import static utam.compiler.translator.DefaultTranslatorRunner.PROFILE_NOT_CONFIGURED_ERR;
import static utam.core.framework.context.StringValueProfile.DEFAULT_PROFILE;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Properties;
import org.testng.annotations.Test;
import utam.compiler.grammar.TestUtilities;
import utam.core.declarative.representation.PageObjectDeclaration;
import utam.core.declarative.translator.ProfileConfiguration;
import utam.core.declarative.translator.TranslatorConfig;
import utam.core.framework.consumer.UtamError;
import utam.core.framework.context.Profile;

/**
 * @author jim.evans
 */
public class DefaultTranslatorRunnerTests_Profiles {

  private static DefaultTranslatorConfiguration getTranslatorConfig(
      ProfileConfiguration profileConfig) {
    DefaultTranslatorConfiguration translatorConfig = TranslatorMockUtilities.getDefaultConfig();
    translatorConfig.setConfiguredProfile(profileConfig);
    return translatorConfig;
  }

  private static ProfileConfiguration getProfileConfigMock(
      String profileKey, Map<String, Profile> profiles) {
    ProfileConfiguration config = mock(ProfileConfiguration.class);
    when(config.getPropertyKey()).thenReturn(profileKey);
    when(config.getSupportedValues()).thenReturn(profiles.keySet());
    for (Entry<String, Profile> entry : profiles.entrySet()) {
      when(config.getFromString(entry.getKey())).thenReturn(entry.getValue());
    }
    return config;
  }

  private static Profile getProfile(String name, String value) {
    Profile profile = mock(Profile.class);
    when(profile.getName()).thenReturn(name);
    when(profile.getValue()).thenReturn(value);
    when(profile.toString()).thenReturn(String.format("%s = %s", name, value));
    return profile;
  }

  private static ProfileConfiguration setupColorProfileConfig() {
    Profile redProfile = getProfile("red", "colorRed");
    Profile blueProfile = getProfile("blue", "colorBlue");
    Map<String, Profile> colorMap = new HashMap<>();
    colorMap.put(redProfile.getName(), redProfile);
    colorMap.put(blueProfile.getName(), blueProfile);
    return getProfileConfigMock("color", colorMap);
  }

  static DefaultTranslatorRunner getRunnerMock() {
    return new DefaultTranslatorRunner(mock(DefaultTranslatorConfiguration.class));
  }

  @Test
  public void testGetProfileMapping() {
    Profile testProfile = getProfile("test", "testValue");
    TranslatorConfig translatorConfig =
        getTranslatorConfig(
            getProfileConfigMock(
                "testConfig", Collections.singletonMap(testProfile.getName(), testProfile)));
    DefaultTranslatorRunner runner =
        new DefaultTranslatorRunner(translatorConfig);
    Properties mappingProperties = runner.getProfileMapping(testProfile);
    assertThat(mappingProperties, is(aMapWithSize(0)));
  }

  @Test
  public void testGetProfileMappingOnUnknownProfileThrows() {
    Profile testProfile = getProfile("test", "testValue");
    DefaultTranslatorRunner runner = getRunnerMock();
    UtamError e = expectThrows(UtamError.class, () -> runner.getProfileMapping(testProfile));
    assertThat(
        e.getMessage(), is(equalTo(String.format(PROFILE_NOT_CONFIGURED_ERR, "test"))));
  }

  @Test
  public void testSetPageObjectWithProfile() {
    String json =
        "{\"implements\": \"utam-test/pageObjects/test/testInterface\","
            + "  \"profile\": [{"
            + "    \"color\": \"red\""
            + "  }]}";
    TranslatorConfig translatorConfig = getTranslatorConfig(setupColorProfileConfig());
    PageObjectDeclaration declaration =
        TestUtilities.getJsonStringDeserializer(json, translatorConfig).getObject();
    DefaultTranslatorRunner runner =
        new DefaultTranslatorRunner(translatorConfig);
    runner.setPageObject("initial", declaration);
    assertThat(runner.getGeneratedObject("initial"), is(sameInstance(declaration)));
  }

  @Test
  public void testSetPageObjectWithProfileReferencingUnconfiguredProfileThrows() {
    String json =
        "{"
            + "  \"implements\": \"utam-test/pageObjects/test/testInterface\","
            + "  \"profile\": [{"
            + "    \"color\": \"red\""
            + "  }]"
            + "}";
    PageObjectDeclaration declaration = TestUtilities.getPageObject(json);
    DefaultTranslatorRunner runner = getRunnerMock();
    UtamError e = expectThrows(UtamError.class, () -> runner.setPageObject("name", declaration));
    assertThat(e.getMessage(), is(equalTo(String.format(PROFILE_NOT_CONFIGURED_ERR, "color"))));
  }

  @Test
  public void testSetPageObjectWithProfileWithDuplicateTypeThrows() {
    String json =
        "{"
            + "  \"implements\": \"utam-test/pageObjects/test/testInterface\","
            + "  \"profile\": [{"
            + "    \"color\": \"red\""
            + "  }]"
            + "}";
    TranslatorConfig translatorConfig = getTranslatorConfig(setupColorProfileConfig());
    PageObjectDeclaration declaration =
        TestUtilities.getJsonStringDeserializer(json, translatorConfig).getObject();

    String interfaceTypeName = declaration.getInterface().getInterfaceType().getFullName();
    DefaultTranslatorRunner runner =
        new DefaultTranslatorRunner(translatorConfig);
    runner.setPageObject("initial", declaration);
    UtamError e =
        expectThrows(UtamError.class, () -> runner.setPageObject(interfaceTypeName, declaration));
    assertThat(
        e.getMessage(),
        is(
            equalTo(
                String.format(
                    DUPLICATE_IMPL_WITH_PROFILE_ERR,
                    "utam.test.pageobjects.test.TestInterface",
                    "utam.test.pageobjects.test.impl.TestImpl",
                    "red"))));
  }

  @Test
  public void testSetImplForProfile() {
    DefaultTranslatorRunner translatorRunner = getRunnerMock();
    translatorRunner.setImplOnlyForProfile(DEFAULT_PROFILE, "typename", "classtypename");
    assertThrows(
        UtamError.class,
        () ->
            translatorRunner.setImplOnlyForProfile(
                mock(Profile.class), "typename", "classtypename"));
  }
}
