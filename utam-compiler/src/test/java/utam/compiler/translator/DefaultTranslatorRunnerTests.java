package utam.compiler.translator;

import utam.compiler.grammar.TestUtilities;
import utam.compiler.grammar.JsonDeserializer;
import utam.core.framework.consumer.UtamError;
import utam.core.framework.context.StringValueProfile;
import org.hamcrest.CoreMatchers;
import org.testng.annotations.Test;
import utam.core.declarative.translator.ProfileConfiguration;
import utam.core.declarative.translator.TranslatorConfig;
import utam.core.declarative.translator.TranslatorRunner;
import utam.core.declarative.translator.UnitTestRunner;

import java.io.IOException;
import java.util.Properties;

import static utam.compiler.translator.AbstractTranslatorConfiguration.ERR_PROFILE_NOT_CONFIGURED;
import static utam.compiler.translator.TranslatorMockUtilities.*;
import static utam.core.framework.context.StringValueProfile.DEFAULT_IMPL;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;
import static org.testng.Assert.expectThrows;

/**
 * @author jim.evans
 */
public class DefaultTranslatorRunnerTests {

  @Test
  public void testRun() throws IOException {
    DefaultSourceConfigurationTests.Mock sourceConfig =
            new DefaultSourceConfigurationTests.Mock();
    sourceConfig.setSources();
    DefaultTargetConfigurationTests.Mock targetConfig =
            new DefaultTargetConfigurationTests.Mock();
    TranslatorConfig translatorConfig =
            new AbstractTranslatorConfigurationTests.Mock(targetConfig, sourceConfig);
    TranslatorRunner translator = new DefaultTranslatorRunnerTests.Mock(translatorConfig);
    translator.run();
  }

  @Test
  public void testWrite() throws IOException {
    DefaultSourceConfigurationTests.Mock sourceConfig =
        new DefaultSourceConfigurationTests.Mock();
    sourceConfig.setSources();
    DefaultTargetConfigurationTests.Mock targetConfig =
        new DefaultTargetConfigurationTests.Mock();
    TranslatorConfig translatorConfig =
        new AbstractTranslatorConfigurationTests.Mock(targetConfig, sourceConfig);
    TranslatorRunner translator = new DefaultTranslatorRunnerTests.Mock(translatorConfig);
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
        new DefaultTargetConfigurationTests.Mock();
    AbstractTranslatorConfiguration translatorConfig =
        new AbstractTranslatorConfigurationTests.Mock(targetConfig, sourceConfig);
    TranslatorRunner translator = new DefaultTranslatorRunnerTests.Mock(translatorConfig);
    translatorConfig.setUnitTestRunner(UnitTestRunner.JUNIT);
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
        new DefaultTargetConfigurationTests.Mock();
    AbstractTranslatorConfiguration translatorConfig = new AbstractTranslatorConfigurationTests.Mock(targetConfig, sourceConfig);
    TranslatorRunner translator = new DefaultTranslatorRunnerTests.Mock(translatorConfig);
    translatorConfig.setUnitTestRunner(UnitTestRunner.TESTNG);
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
    DefaultSourceConfigurationTests.Mock sourceConfig =
        new DefaultSourceConfigurationTests.Mock();
    sourceConfig.setSources();
    DefaultTranslatorRunner runner = sourceConfig.getRunner();
    runner.run();
    assertThat(
        runner.getGeneratedObject(PAGE_OBJECT_URI).getImplementation().getProfiles().length,
        is(equalTo(0)));
    assertThat(
        runner.getGeneratedObject(INTERFACE_ONLY_URI).getImplementation().getProfiles().length,
        is(equalTo(0)));
    assertThat(
        runner.getGeneratedObject(IMPL_ONLY_URI).getImplementation().getProfiles().length,
        is(equalTo(0)));
  }

  @Test
  public void profileConfigNotSet() {
    final String JSON_WITH_DRIVER_PROFILE =
        String.format(
            "{ \"implements\" : \"%s\", \"profile\" : [{\"driver\" : \"chrome\"}] }", TEST_URI);

    JsonDeserializer deserializer =
        TestUtilities.getJsonStringDeserializer(JSON_WITH_DRIVER_PROFILE);
    UtamError e =
        expectThrows(
            UtamError.class, () -> deserializer.getObject().getImplementation().getProfiles());
    assertThat(
        e.getMessage(),
        CoreMatchers.is(CoreMatchers.equalTo(String.format(ERR_PROFILE_NOT_CONFIGURED, "driver"))));
  }

  @Test
  public void testDefaultImplementation() {
    DefaultSourceConfigurationTests.Mock sourceConfig =
        new DefaultSourceConfigurationTests.Mock();
    sourceConfig.setSources();
    DefaultTranslatorRunner runner = sourceConfig.getRunner();
    runner.run();
    Properties properties = runner.getProfileMapping(DEFAULT_IMPL);
    assertThat(
        properties.containsKey(
            TranslationTypesConfigJava.getJavaType(
                    INTERFACE_ONLY_URI, TranslationTypesConfigJava.Mask.pageObjects)
                .getFullName()),
        is(true));
  }

  @Test
  public void testProfilesOutputConfig() {
    final String JSON_WITH_DRIVER_PROFILE =
        String.format(
            "{ \"implements\" : \"%s\", \"profile\" : [{\"driver\" : \"chrome\"}] }", TEST_URI);
    DefaultSourceConfigurationTests.Mock sourceConfig =
        new DefaultSourceConfigurationTests.Mock();
    sourceConfig.setJSONSource(TEST_URI, JSON_WITH_DRIVER_PROFILE);
    AbstractTranslatorConfiguration translatorConfig = sourceConfig.getConfig();
    final ProfileConfiguration
        DRIVER_PROFILE_CONFIG = new StringValueProfileConfig("driver", "chrome");
    translatorConfig.setConfiguredProfile(DRIVER_PROFILE_CONFIG);
    DefaultTranslatorRunner translator = new DefaultTranslatorRunnerTests.Mock(translatorConfig);
    translator.run();
    Properties profiles = translator.getProfileMapping(new StringValueProfile("driver", "chrome"));
    assertThat(profiles.size(), is(equalTo(1)));
    assertThat(profiles.keySet().iterator().next(), is(equalTo(TEST_URI_INTERFACE_NAME)));
    assertThat(profiles.values().iterator().next(), is(equalTo(TEST_URI_CLASS_NAME)));
  }

  @Test
  public void testDefaultProfiles() {
    DefaultTranslatorRunner runner = new Mock();
    assertThat(runner.getAllProfiles().size(), is(equalTo(1)));
    assertThat(runner.getAllProfiles().iterator().next(), is(equalTo(DEFAULT_IMPL)));
  }



  static class Mock extends DefaultTranslatorRunner {

    Mock(TranslatorConfig translatorConfig) {
      super(translatorConfig);
    }

    Mock() {
      super(getConfig());
    }

    static TranslatorConfig getConfig() {
      DefaultSourceConfigurationTests.Mock sourceConfig =
              new DefaultSourceConfigurationTests.Mock();
      DefaultTargetConfigurationTests.Mock targetConfig =
              new DefaultTargetConfigurationTests.Mock();
      return
              new AbstractTranslatorConfigurationTests.Mock(targetConfig, sourceConfig);
    }
  }
}
