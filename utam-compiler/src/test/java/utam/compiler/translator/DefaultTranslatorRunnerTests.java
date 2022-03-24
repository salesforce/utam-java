/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.translator;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.aMapWithSize;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.sameInstance;
import static org.mockito.Mockito.mock;
import static org.testng.Assert.expectThrows;
import static utam.compiler.grammar.TestUtilities.getJsonStringDeserializer;
import static utam.compiler.translator.DefaultSourceConfigurationTests.TranslatorConfigWithProfile.TEST_PROFILE;
import static utam.compiler.translator.DefaultTranslatorRunner.DUPLICATE_IMPL_WITH_PROFILE_ERR;
import static utam.compiler.translator.DefaultTranslatorRunner.DUPLICATE_PAGE_OBJECT_NAME;
import static utam.compiler.translator.DefaultTranslatorRunner.ERR_PROFILE_PATH_DOES_NOT_EXIST;
import static utam.compiler.translator.DefaultTranslatorRunner.ERR_PROFILE_PATH_NOT_CONFIGURED;
import static utam.compiler.translator.DefaultTranslatorRunner.PROFILE_NOT_CONFIGURED_ERR;
import static utam.compiler.translator.TranslatorMockUtilities.IMPL_ONLY_CLASS_NAME;
import static utam.compiler.translator.TranslatorMockUtilities.INTERFACE_ONLY_CLASS_NAME;
import static utam.compiler.translator.TranslatorMockUtilities.PAGE_OBJECT_IMPL_CLASS_NAME;
import static utam.compiler.translator.TranslatorMockUtilities.PAGE_OBJECT_INTERFACE_CLASS_NAME;
import static utam.compiler.translator.TranslatorMockUtilities.TEST_URI;
import static utam.compiler.translator.TranslatorMockUtilities.TEST_URI_CLASS_NAME;
import static utam.compiler.translator.TranslatorMockUtilities.TEST_URI_INTERFACE_NAME;

import java.io.IOException;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.hamcrest.Matchers;
import org.testng.annotations.Test;
import utam.compiler.grammar.JsonDeserializer;
import utam.compiler.grammar.TestUtilities;
import utam.compiler.translator.DefaultSourceConfigurationTests.TranslatorConfigWithProfile;
import utam.core.declarative.representation.PageObjectDeclaration;
import utam.core.declarative.translator.ProfileConfiguration;
import utam.core.declarative.translator.TranslatorConfig;
import utam.core.declarative.translator.TranslatorRunner;
import utam.core.declarative.translator.UnitTestRunner;
import utam.core.framework.consumer.UtamError;
import utam.core.framework.context.Profile;
import utam.core.framework.context.StringValueProfile;

/**
 * @author jim.evans
 */
public class DefaultTranslatorRunnerTests {

  private static DefaultTranslatorRunner getRunner() {
    DefaultSourceConfigurationTests.Mock sourceConfig =
        new DefaultSourceConfigurationTests.Mock();
    sourceConfig.setSources();
    DefaultTargetConfigurationTests.Mock targetConfig =
        new DefaultTargetConfigurationTests.Mock();
    TranslatorConfig translatorConfig = new TranslatorConfigWithProfile(sourceConfig,
        targetConfig);
    return new DefaultTranslatorRunner(translatorConfig);
  }

  private static DefaultTranslatorConfiguration getTranslatorConfig(
      ProfileConfiguration profileConfig) {
    DefaultTranslatorConfiguration translatorConfig = TranslatorMockUtilities.getDefaultConfig();
    translatorConfig.setConfiguredProfile(profileConfig);
    return translatorConfig;
  }

  private static ProfileConfiguration setupColorProfileConfig() {
    Profile redProfile = new StringValueProfile("color", "red");
    return new StringValueProfileConfig(redProfile);
  }

  private static DefaultTranslatorRunner getRunnerMock() {
    return new DefaultTranslatorRunner(mock(DefaultTranslatorConfiguration.class));
  }

  @Test
  public void testRun() {
    DefaultTranslatorRunner translator = getRunner();
    translator.run();
  }

  @Test
  public void testWrite() throws IOException {
    DefaultSourceConfigurationTests.Mock sourceConfig =
        new DefaultSourceConfigurationTests.Mock();
    sourceConfig.setSources();
    DefaultTargetConfigurationTests.Mock targetConfig =
        new DefaultTargetConfigurationTests.Mock();
    TranslatorConfig translatorConfig = new TranslatorConfigWithProfile(sourceConfig, targetConfig);
    TranslatorRunner translator = new DefaultTranslatorRunner(translatorConfig);
    translator.run();
    translator.write();
    assertThat(targetConfig.writers.keySet(), hasSize(4));
    assertThat(
        targetConfig.writers.keySet(),
        containsInAnyOrder(
            PAGE_OBJECT_INTERFACE_CLASS_NAME,
            PAGE_OBJECT_IMPL_CLASS_NAME,
            INTERFACE_ONLY_CLASS_NAME,
            IMPL_ONLY_CLASS_NAME));
    assertThat(
        targetConfig.writers.get(PAGE_OBJECT_INTERFACE_CLASS_NAME).toString(),
        containsString("public interface TestPageObject extends PageObject"));
    assertThat(
        targetConfig.writers.get(PAGE_OBJECT_IMPL_CLASS_NAME).toString(),
        containsString(
            "public final class TestPageObjectImpl extends BasePageObject implements TestPageObject"));
    assertThat(
        targetConfig.writers.get(INTERFACE_ONLY_CLASS_NAME).toString(),
        containsString("public interface TestAbstractObject extends PageObject"));
    assertThat(
        targetConfig.writers.get(IMPL_ONLY_CLASS_NAME).toString(),
        containsString(
            "public final class TestImplObjectImpl extends BasePageObject implements TestAbstractObject"));
  }

  @Test
  public void testWriteWithJunitRunner() throws IOException {
    DefaultSourceConfigurationTests.Mock sourceConfig =
        new DefaultSourceConfigurationTests.Mock();
    sourceConfig.setSources();
    DefaultTargetConfigurationTests.Mock targetConfig =
        new DefaultTargetConfigurationTests.Mock(UnitTestRunner.JUNIT);
    TranslatorConfig translatorConfig = new TranslatorConfigWithProfile(sourceConfig, targetConfig);
    TranslatorRunner translator = new DefaultTranslatorRunner(translatorConfig);
    translator.run();
    translator.write();
    assertThat(targetConfig.writers.keySet(), hasSize(6));
    assertThat(
        targetConfig.writers.keySet(),
        containsInAnyOrder(
            PAGE_OBJECT_INTERFACE_CLASS_NAME,
            PAGE_OBJECT_IMPL_CLASS_NAME,
            PAGE_OBJECT_IMPL_CLASS_NAME + "Tests",
            INTERFACE_ONLY_CLASS_NAME,
            IMPL_ONLY_CLASS_NAME,
            IMPL_ONLY_CLASS_NAME + "Tests"));
    assertThat(
        targetConfig.writers.get(PAGE_OBJECT_INTERFACE_CLASS_NAME).toString(),
        containsString("public interface TestPageObject extends PageObject"));
    assertThat(
        targetConfig.writers.get(PAGE_OBJECT_IMPL_CLASS_NAME).toString(),
        containsString(
            "public final class TestPageObjectImpl extends BasePageObject implements TestPageObject"));
    assertThat(
        targetConfig.writers.get(INTERFACE_ONLY_CLASS_NAME).toString(),
        containsString("public interface TestAbstractObject extends PageObject"));
    assertThat(
        targetConfig.writers.get(IMPL_ONLY_CLASS_NAME).toString(),
        containsString(
            "public final class TestImplObjectImpl extends BasePageObject implements TestAbstractObject"));
    assertThat(
        targetConfig.writers.get(PAGE_OBJECT_IMPL_CLASS_NAME + "Tests").toString(),
        containsString("org.junit.Test"));
    assertThat(
        targetConfig.writers.get(IMPL_ONLY_CLASS_NAME + "Tests").toString(),
        containsString("org.junit.Test"));
  }

  @Test
  public void testWriteWithTestNGRunner() throws IOException {
    DefaultSourceConfigurationTests.Mock sourceConfig =
        new DefaultSourceConfigurationTests.Mock();
    sourceConfig.setSources();
    DefaultTargetConfigurationTests.Mock targetConfig =
        new DefaultTargetConfigurationTests.Mock(UnitTestRunner.TESTNG);
    TranslatorConfig translatorConfig = new TranslatorConfigWithProfile(sourceConfig, targetConfig);
    TranslatorRunner translator = new DefaultTranslatorRunner(translatorConfig);
    translator.run();
    translator.write();
    assertThat(targetConfig.writers.keySet(), hasSize(6));
    assertThat(
        targetConfig.writers.keySet(),
        containsInAnyOrder(
            PAGE_OBJECT_INTERFACE_CLASS_NAME,
            PAGE_OBJECT_IMPL_CLASS_NAME,
            PAGE_OBJECT_IMPL_CLASS_NAME + "Tests",
            INTERFACE_ONLY_CLASS_NAME,
            IMPL_ONLY_CLASS_NAME,
            IMPL_ONLY_CLASS_NAME + "Tests"));
    assertThat(
        targetConfig.writers.get(PAGE_OBJECT_INTERFACE_CLASS_NAME).toString(),
        containsString("public interface TestPageObject extends PageObject"));
    assertThat(
        targetConfig.writers.get(PAGE_OBJECT_IMPL_CLASS_NAME).toString(),
        containsString(
            "public final class TestPageObjectImpl extends BasePageObject implements TestPageObject"));
    assertThat(
        targetConfig.writers.get(INTERFACE_ONLY_CLASS_NAME).toString(),
        containsString("public interface TestAbstractObject extends PageObject"));
    assertThat(
        targetConfig.writers.get(IMPL_ONLY_CLASS_NAME).toString(),
        containsString(
            "public final class TestImplObjectImpl extends BasePageObject implements TestAbstractObject"));
    assertThat(
        targetConfig.writers.get(PAGE_OBJECT_IMPL_CLASS_NAME + "Tests").toString(),
        containsString("org.testng.annotations.Test"));
    assertThat(
        targetConfig.writers.get(IMPL_ONLY_CLASS_NAME + "Tests").toString(),
        containsString("org.testng.annotations.Test"));
  }

  @Test
  public void testProfileConfigNotSetJson() {
    final String json =
        String.format(
            "{ \"implements\" : \"%s\", \"profile\" : [{\"driver\" : \"chrome\"}] }", TEST_URI);

    JsonDeserializer deserializer = getJsonStringDeserializer(json);
    UtamError e =
        expectThrows(
            UtamError.class, () -> deserializer.getObject().getImplementation().getProfiles());
    assertThat(
        e.getMessage(),
        is(equalTo(String.format(PROFILE_NOT_CONFIGURED_ERR, "driver"))));
  }

  @Test
  public void testSetImplProfileConfigNotSet() {
    DefaultTranslatorRunner runner = getRunner();
    Profile profile = new StringValueProfile("my", "test");
    UtamError e = expectThrows(
        UtamError.class, () -> runner.setImplementation(profile, "type", "type"));
    assertThat(
        e.getMessage(),
        is(equalTo(String.format(PROFILE_NOT_CONFIGURED_ERR, "my"))));
  }

  @Test
  public void testSetImplDuplicateProfile() {
    DefaultTranslatorRunner runner = getRunner();
    Profile profile = TEST_PROFILE;
    runner.setImplementation(profile, "type", "typeImpl1");
    UtamError e = expectThrows(
        UtamError.class, () -> runner.setImplementation(profile, "type", "typeImpl2"));
    assertThat(
        e.getMessage(),
        is(equalTo(String.format(DUPLICATE_IMPL_WITH_PROFILE_ERR, "typeImpl2", "type", "typeImpl1", "{ profile : test }"))));
  }

  @Test
  public void testProfilesOutputConfig() {
    final String json =
        String.format(
            "{ \"implements\" : \"%s\", \"profile\" : [{\"driver\" : \"chrome\"}] }", TEST_URI);
    DefaultSourceConfigurationTests.Mock sourceConfig =
        new DefaultSourceConfigurationTests.Mock();
    sourceConfig.setJSONSource(TEST_URI, json);
    DefaultTargetConfigurationTests.Mock targetConfig =
        new DefaultTargetConfigurationTests.Mock();
    DefaultTranslatorConfiguration translatorConfig = new DefaultTranslatorConfiguration(
        sourceConfig, targetConfig);
    translatorConfig.setConfiguredProfile(new StringValueProfileConfig("driver", "chrome"));
    DefaultTranslatorRunner translator = new DefaultTranslatorRunner(translatorConfig);
    translator.run();
    Map<String,String> profiles = translator.testProfileMapping(new StringValueProfile("driver", "chrome"));
    assertThat(profiles.size(), is(equalTo(1)));
    assertThat(profiles.keySet().iterator().next(), is(equalTo(TEST_URI_INTERFACE_NAME)));
    assertThat(profiles.values().iterator().next(), is(equalTo(TEST_URI_CLASS_NAME)));
  }

  @Test
  public void testGetResourcesRootThrows() {
    DefaultTargetConfigurationTests.Mock targetConfig = new DefaultTargetConfigurationTests.Mock();
    DefaultTranslatorRunner runner = targetConfig.getRunner();
    UtamError e = expectThrows(UtamError.class, runner::getResourcesRoot);
    assertThat(e.getMessage(), is(equalTo(ERR_PROFILE_PATH_NOT_CONFIGURED)));
    targetConfig.setConfigPath("");
    e = expectThrows(UtamError.class, runner::getResourcesRoot);
    assertThat(e.getMessage(), is(equalTo(ERR_PROFILE_PATH_NOT_CONFIGURED)));
    targetConfig.setConfigPath("err.err");
    e = expectThrows(UtamError.class, runner::getResourcesRoot);
    assertThat(
        e.getMessage(), is(equalTo(String.format(ERR_PROFILE_PATH_DOES_NOT_EXIST, "err.err"))));
  }

  @Test
  public void testSetPageObject() {
    PageObjectDeclaration declaration = TestUtilities.getPageObject("{}");
    DefaultTranslatorRunner runner = getRunnerMock();
    runner.setPageObject("initial", declaration);
    assertThat(runner.getGeneratedObject("initial"), is(sameInstance(declaration)));
    assertThat(runner.getGeneratedPageObjectsNames(),
        is(Matchers.equalTo(Stream.of("initial").collect(Collectors.toSet()))));
  }

  @Test
  public void testSetPageObjectWithDuplicateNameThrows() {
    PageObjectDeclaration declaration = TestUtilities.getPageObject("{}");
    DefaultTranslatorRunner runner = getRunnerMock();
    runner.setPageObject("initial", declaration);
    UtamError e = expectThrows(UtamError.class, () -> runner.setPageObject("initial", declaration));
    assertThat(
        e.getMessage(), containsString(String.format(DUPLICATE_PAGE_OBJECT_NAME, "initial")));
  }

  @Test
  public void testSetPageObjectWithInterface() {
    String json = "{  \"interface\": true  }";
    PageObjectDeclaration declaration = TestUtilities.getPageObject(json);
    DefaultTranslatorRunner runner = getRunnerMock();
    runner.setPageObject("initial", declaration);
    assertThat(runner.getGeneratedObject("initial"), is(sameInstance(declaration)));
  }

  @Test
  public void testSetPageObjectWithInterfaceWithDuplicateTypeThrows() {
    String json = "{  \"interface\": true  }";
    PageObjectDeclaration declaration = TestUtilities.getPageObject(json);
    DefaultTranslatorRunner runner = getRunnerMock();
    String type = "type";
    runner.setPageObject(type, declaration);
    UtamError e = expectThrows(UtamError.class, () -> runner.setPageObject(type, declaration));
    assertThat(e.getMessage(), containsString(String.format(DUPLICATE_PAGE_OBJECT_NAME, type)));
  }

  @Test
  public void testGetProfileMapping() {
    Profile testProfile = new StringValueProfile("test", "testValue");
    TranslatorConfig translatorConfig = getTranslatorConfig(new StringValueProfileConfig(testProfile));
    DefaultTranslatorRunner runner = new DefaultTranslatorRunner(translatorConfig);
    Map<String,String> mappingProperties = runner.testProfileMapping(testProfile);
    assertThat(mappingProperties, is(aMapWithSize(0)));
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
        getJsonStringDeserializer(json, translatorConfig).getObject();
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
    assertThat(e.getMessage(), is(Matchers.equalTo(String.format(PROFILE_NOT_CONFIGURED_ERR, "color"))));
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
    PageObjectDeclaration declaration = getJsonStringDeserializer(json, translatorConfig)
        .getObject();
    DefaultTranslatorRunner runner = new DefaultTranslatorRunner(translatorConfig);
    runner.setPageObject("one", declaration);
    UtamError e = expectThrows(UtamError.class, () -> runner.setPageObject("two", declaration));
    String implName = "utam.test.pageobjects.test.impl.TestImpl";
    assertThat(
        e.getMessage(),
        is(Matchers.equalTo(String.format(
            DUPLICATE_IMPL_WITH_PROFILE_ERR,
            implName,
            "utam.test.pageobjects.test.TestInterface",
            implName,
            "{ color : red }"))));
  }
}
