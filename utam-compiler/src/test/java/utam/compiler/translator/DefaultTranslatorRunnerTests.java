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
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.emptyArray;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static org.testng.Assert.expectThrows;
import static utam.compiler.grammar.TestUtilities.getJsonStringDeserializer;
import static utam.compiler.translator.DefaultTranslatorRunner.DUPLICATE_IMPL_ERR;
import static utam.compiler.translator.DefaultTranslatorRunner.DUPLICATE_IMPL_WITH_PROFILE_ERR;
import static utam.compiler.translator.DefaultTranslatorRunner.DUPLICATE_PAGE_OBJECT_NAME;
import static utam.compiler.translator.DefaultTranslatorRunner.ERR_PROFILE_PATH_DOES_NOT_EXIST;
import static utam.compiler.translator.DefaultTranslatorRunner.ERR_PROFILE_PATH_NOT_CONFIGURED;
import static utam.compiler.translator.DefaultTranslatorRunner.PROFILE_NOT_CONFIGURED_ERR;
import static utam.compiler.translator.TranslatorMockUtilities.IMPL_ONLY_CLASS_NAME;
import static utam.compiler.translator.TranslatorMockUtilities.IMPL_ONLY_URI;
import static utam.compiler.translator.TranslatorMockUtilities.INTERFACE_ONLY_CLASS_NAME;
import static utam.compiler.translator.TranslatorMockUtilities.INTERFACE_ONLY_URI;
import static utam.compiler.translator.TranslatorMockUtilities.PAGE_OBJECT_IMPL_CLASS_NAME;
import static utam.compiler.translator.TranslatorMockUtilities.PAGE_OBJECT_INTERFACE_CLASS_NAME;
import static utam.compiler.translator.TranslatorMockUtilities.PAGE_OBJECT_URI;
import static utam.compiler.translator.TranslatorMockUtilities.TEST_URI;
import static utam.compiler.translator.TranslatorMockUtilities.TEST_URI_CLASS_NAME;
import static utam.compiler.translator.TranslatorMockUtilities.TEST_URI_INTERFACE_NAME;
import static utam.core.framework.context.StringValueProfile.DEFAULT_PROFILE;

import java.io.IOException;
import java.util.Properties;
import org.testng.annotations.Test;
import utam.compiler.grammar.JsonDeserializer;
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
    TranslatorConfig translatorConfig = new DefaultTranslatorConfiguration(sourceConfig,
        targetConfig);
    return new DefaultTranslatorRunner(translatorConfig);
  }

  @Test
  public void testRun() {
    DefaultTranslatorRunner translator = getRunner();
    assertThat(translator.getAllProfiles().size(), is(equalTo(1)));
    assertThat(translator.getAllProfiles().iterator().next(), is(equalTo(DEFAULT_PROFILE)));
    translator.run();
  }

  @Test
  public void testWrite() throws IOException {
    DefaultSourceConfigurationTests.Mock sourceConfig =
        new DefaultSourceConfigurationTests.Mock();
    sourceConfig.setSources();
    DefaultTargetConfigurationTests.Mock targetConfig =
        new DefaultTargetConfigurationTests.Mock();
    TranslatorConfig translatorConfig = new DefaultTranslatorConfiguration(sourceConfig,
        targetConfig);
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
    TranslatorConfig translatorConfig = new DefaultTranslatorConfiguration(sourceConfig,
        targetConfig);
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
    TranslatorConfig translatorConfig = new DefaultTranslatorConfiguration(sourceConfig,
        targetConfig);
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
  public void testEmptyProfiles() {
    DefaultTranslatorRunner runner = getRunner();
    runner.run();
    assertThat(
        runner.getGeneratedObject(PAGE_OBJECT_URI).getImplementation().getProfiles(),
        is(emptyArray()));
    assertThat(
        runner.getGeneratedObject(INTERFACE_ONLY_URI).getImplementation().getProfiles(),
        is(emptyArray()));
    assertThat(
        runner.getGeneratedObject(IMPL_ONLY_URI).getImplementation().getProfiles(),
        is(emptyArray()));
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
  public void testProfileConfigNotSet() {
    DefaultTranslatorRunner runner = getRunner();
    Profile profile = new StringValueProfile("my", "test");
    UtamError e = expectThrows(
            UtamError.class, () -> runner.getProfileMapping(profile));
    assertThat(
        e.getMessage(),
        is(equalTo(String.format(PROFILE_NOT_CONFIGURED_ERR, "my"))));
  }

  @Test
  public void testSetImplProfileConfigNotSet() {
    DefaultTranslatorRunner runner = getRunner();
    Profile profile = new StringValueProfile("my", "test");
    UtamError e = expectThrows(
        UtamError.class, () -> runner.setImplOnlyForProfile(profile, "type", "type"));
    assertThat(
        e.getMessage(),
        is(equalTo(String.format(PROFILE_NOT_CONFIGURED_ERR, "my"))));
  }

  @Test
  public void testSetImplDuplicateProfile() {
    DefaultTranslatorRunner runner = getRunner();
    Profile profile = DEFAULT_PROFILE;
    runner.setImplOnlyForProfile(profile, "type", "typeImpl1");
    UtamError e = expectThrows(
        UtamError.class, () -> runner.setImplOnlyForProfile(profile, "type", "typeImpl2"));
    assertThat(
        e.getMessage(),
        is(equalTo(String.format(DUPLICATE_IMPL_WITH_PROFILE_ERR, "type", "typeImpl1", "default"))));
  }

  @Test
  public void testSetImplOnly() {
    DefaultTranslatorRunner runner = getRunner();
    runner.setImplOnly("type", "typeImpl1");
    Properties properties = runner.getProfileMapping(DEFAULT_PROFILE);
    assertThat(properties.getProperty("type"), is(equalTo("typeImpl1")));
    UtamError e = expectThrows(
        UtamError.class, () -> runner.setImplOnly( "type", "typeImpl2"));
    assertThat(
        e.getMessage(),
        is(equalTo(String.format(DUPLICATE_IMPL_ERR, "type", "typeImpl1"))));
  }

  @Test
  public void testSetInterfaceOnly() {
    DefaultTranslatorRunner runner = getRunner();
    runner.setInterfaceOnly("type");
    Properties properties = runner.getProfileMapping(DEFAULT_PROFILE);
    assertThat(properties.getProperty("type"), is(equalTo("")));
    UtamError e = expectThrows(
        UtamError.class, () -> runner.setInterfaceOnly("type"));
    assertThat(
        e.getMessage(),
        is(equalTo(String.format(DUPLICATE_PAGE_OBJECT_NAME, "type"))));
  }

  @Test
  public void testDefaultImplementation() {
    DefaultTranslatorRunner runner = getRunner();
    runner.run();
    Properties properties = runner.getProfileMapping(DEFAULT_PROFILE);
    assertThat(
        properties.containsKey(
            TranslationTypesConfigJava.getJavaType(
                INTERFACE_ONLY_URI, TranslationTypesConfigJava.Mask.pageObjects)
                .getFullName()),
        is(true));
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
    Properties profiles = translator.getProfileMapping(new StringValueProfile("driver", "chrome"));
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
}
