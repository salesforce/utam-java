package utam.compiler.translator;

import utam.core.declarative.translator.ProfileConfiguration;
import utam.core.declarative.translator.TranslatorConfig;
import utam.compiler.grammar.TestUtilities;
import utam.core.declarative.representation.PageObjectDeclaration;
import utam.core.framework.consumer.UtamError;
import utam.core.framework.context.Profile;
import org.testng.annotations.Test;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Properties;

import static utam.compiler.translator.DefaultTranslatorRunner.*;
import static utam.compiler.translator.DefaultTranslatorRunnerTests.Mock.getConfig;
import static utam.core.framework.context.StringValueProfile.DEFAULT_PROFILE;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.testng.Assert.assertThrows;
import static org.testng.Assert.expectThrows;

/**
 * created by Jim for no longer existing class PageObjectsInventory <br>
 * to be revised
 *
 * @author jim.evans
 */
public class DefaultTranslatorRunnerTests_Profiles {

  private static TranslatorConfig getTranslatorConfig(ProfileConfiguration profileConfig) {
    TranslatorConfig translatorConfig = TranslatorMockUtilities.getDefaultConfig();
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

  @Test
  public void testGetProfileMapping() {
    Profile testProfile = getProfile("test", "testValue");
    TranslatorConfig translatorConfig =
        getTranslatorConfig(
            getProfileConfigMock(
                "testConfig", Collections.singletonMap(testProfile.getName(), testProfile)));
    DefaultTranslatorRunnerTests.Mock runner =
        new DefaultTranslatorRunnerTests.Mock(translatorConfig);
    Properties mappingProperties = runner.getProfileMapping(testProfile);
    assertThat(mappingProperties, is(aMapWithSize(0)));
  }

  @Test
  public void testGetProfileMappingOnUnknownProfileThrows() {
    Profile testProfile = getProfile("test", "testValue");
    DefaultTranslatorRunnerTests.Mock runner = new DefaultTranslatorRunnerTests.Mock();
    UtamError e = expectThrows(UtamError.class, () -> runner.getProfileMapping(testProfile));
    assertThat(
        e.getMessage(), is(equalTo(String.format(PROFILE_NOT_CONFIGURED_ERR, "test = testValue"))));
  }

  @Test
  public void testSetPageObject() {
    PageObjectDeclaration declaration = TestUtilities.getPageObject("{}");
    DefaultTranslatorRunnerTests.Mock runner = new DefaultTranslatorRunnerTests.Mock();
    runner.setPageObject("initial", declaration);
    assertThat(runner.getGeneratedObject("initial"), is(sameInstance(declaration)));
  }

  @Test
  public void testSetPageObjectWithDuplicateNameThrows() {
    PageObjectDeclaration declaration = TestUtilities.getPageObject("{}");
    DefaultTranslatorRunnerTests.Mock runner = new DefaultTranslatorRunnerTests.Mock();
    runner.setPageObject("initial", declaration);
    UtamError e = expectThrows(UtamError.class, () -> runner.setPageObject("initial", declaration));
    assertThat(
        e.getMessage(), containsString(String.format(DUPLICATE_PAGE_OBJECT_NAME, "initial")));
  }

  @Test
  public void testSetPageObjectWithInterface() {
    String json = "{  \"interface\": true  }";
    PageObjectDeclaration declaration = TestUtilities.getPageObject(json);
    DefaultTranslatorRunnerTests.Mock runner = new DefaultTranslatorRunnerTests.Mock();
    runner.setPageObject("initial", declaration);
    assertThat(runner.getGeneratedObject("initial"), is(sameInstance(declaration)));
  }

  @Test
  public void testSetPageObjectWithInterfaceWithDuplicateTypeThrows() {
    String json = "{  \"interface\": true  }";
    PageObjectDeclaration declaration = TestUtilities.getPageObject(json);
    DefaultTranslatorRunnerTests.Mock runner = new DefaultTranslatorRunnerTests.Mock();
    runner.setPageObject("initial", declaration);
    String type = declaration.getInterface().getInterfaceType().getFullName();
    UtamError e = expectThrows(UtamError.class, () -> runner.setPageObject(type, declaration));
    assertThat(e.getMessage(), containsString(String.format(DUPLICATE_PAGE_OBJECT_NAME, type)));
  }

  @Test
  public void testSetPageObjectWithImplementation() {
    String json = "{ \"implements\": \"utam-test/pageObjects/test/testInterface\"}";
    PageObjectDeclaration declaration = TestUtilities.getPageObject(json);
    DefaultTranslatorRunnerTests.Mock runner = new DefaultTranslatorRunnerTests.Mock();
    runner.setPageObject("initial", declaration);
    assertThat(runner.getGeneratedObject("initial"), is(sameInstance(declaration)));
  }

  @Test
  public void testSetPageObjectWithImplementationOfDeclaredInterface() {
    String interfaceJson = "{ \"interface\": true }";
    String implementationJson = "{\"implements\": \"utam-test/pageObjects/test/test\"}";

    PageObjectDeclaration interfaceDeclaration = TestUtilities.getPageObject(interfaceJson);
    String interfaceTypeName = interfaceDeclaration.getInterface().getInterfaceType().getFullName();
    PageObjectDeclaration implDeclaration = TestUtilities.getPageObject(implementationJson);
    String implementationTypeName =
        implDeclaration.getImplementation().getClassType().getFullName();
    DefaultTranslatorRunnerTests.Mock runner = new DefaultTranslatorRunnerTests.Mock();
    runner.setPageObject(interfaceTypeName, interfaceDeclaration);
    runner.setPageObject(implementationTypeName, implDeclaration);
    assertThat(
        runner.getGeneratedObject(interfaceTypeName), is(sameInstance(interfaceDeclaration)));
    assertThat(
        runner.getGeneratedObject(implementationTypeName), is(sameInstance(implDeclaration)));
  }

  @Test
  public void testSetPageObjectWithImplementationWithDuplicateTypeThrows() {
    PageObjectDeclaration declaration =
        TestUtilities.getPageObject("{\"implements\": \"utam-test/pageObjects/test/testInterface\"}");
    String interfaceTypeName = declaration.getInterface().getInterfaceType().getFullName();
    DefaultTranslatorRunnerTests.Mock runner = new DefaultTranslatorRunnerTests.Mock();
    runner.setPageObject("initial", declaration);
    UtamError e =
        expectThrows(UtamError.class, () -> runner.setPageObject(interfaceTypeName, declaration));
    assertThat(
        e.getMessage(),
        is(
            equalTo(
                String.format(
                    DUPLICATE_IMPL_ERR,
                    "utam.test.pageobjects.test.TestInterface",
                    "utam.test.pageobjects.test.impl.TestImpl"))));
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
    DefaultTranslatorRunnerTests.Mock runner =
        new DefaultTranslatorRunnerTests.Mock(translatorConfig);
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
    DefaultTranslatorRunnerTests.Mock runner = new DefaultTranslatorRunnerTests.Mock();
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
    DefaultTranslatorRunnerTests.Mock runner =
        new DefaultTranslatorRunnerTests.Mock(translatorConfig);
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
                    "red = colorRed"))));
  }

  @Test
  public void testSetImplForProfile() {
    DefaultTranslatorRunner translatorRunner = new DefaultTranslatorRunner(getConfig()) {};
    translatorRunner.setImplOnlyForProfile(DEFAULT_PROFILE, "typename", "classtypename");
    assertThrows(
        UtamError.class,
        () ->
            translatorRunner.setImplOnlyForProfile(
                mock(Profile.class), "typename", "classtypename"));
  }
}
