/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.translator;

import static org.hamcrest.CoreMatchers.hasItems;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.anEmptyMap;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.hamcrest.Matchers.empty;
import static org.hamcrest.Matchers.emptyIterable;
import static org.hamcrest.Matchers.emptyString;
import static org.hamcrest.collection.IsCollectionWithSize.hasSize;
import static org.hamcrest.core.IsEqual.equalTo;
import static org.testng.Assert.expectThrows;
import static utam.compiler.translator.JsonCompilerConfig.ERR_READING_COMPILER_CONFIG;
import static utam.compiler.translator.JsonCompilerConfig.Module.DEFAULT_JSON_FILE_MASK_REGEX;
import static utam.compiler.translator.JsonCompilerConfig.Module.ERR_DUPLICATE_PROFILE;
import static utam.compiler.translator.JsonCompilerConfig.Module.ERR_FILES_WITHOUT_NAMESPACE;
import static utam.compiler.translator.JsonCompilerConfig.Namespace.ERR_DUPLICATE_MAPPING;
import static utam.compiler.translator.JsonCompilerConfig.Profile.ERR_DUPLICATE_PROFILE_DIFF_VALUES;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import org.hamcrest.CoreMatchers;
import org.testng.annotations.Test;
import utam.compiler.translator.JsonCompilerConfig.Module;
import utam.compiler.translator.JsonCompilerConfig.Namespace;
import utam.compiler.translator.JsonCompilerConfig.Profile;
import utam.core.declarative.translator.ProfileConfiguration;
import utam.core.declarative.translator.TranslatorSourceConfig;
import utam.core.declarative.translator.TranslatorTargetConfig;
import utam.core.declarative.translator.UnitTestRunner;
import utam.core.framework.consumer.UtamError;

/**
 * tests for json config
 *
 * @author elizaveta.ivanova
 * @since 234
 */
public class JsonCompilerConfigTests {

  private static JsonCompilerConfig getTestConfig() throws IOException {
    return new JsonCompilerConfig(
        new File(
            JsonCompilerConfig.class
                .getClassLoader()
                .getResource("config/utam.config.json")
                .getFile()),
        new File(System.getProperty("user.dir")),
        null);
  }

  private static Module getTestModule() {
    return new Module(
        "name",
        null,
        null,
        "pageObjectsDirectory",
        null,
        null,
        null,
        null,
        new ArrayList<>(),
        new ArrayList<>(),
        new ArrayList<>(),
        null,
        null);
  }

  @Test
  public void testValuesFromJsonFile() throws IOException {
    JsonCompilerConfig config = getTestConfig();
    assertThat(config.getModuleName(), is(equalTo("myModule")));
    assertThat(config.getVersion(), is(equalTo("myVersion")));
    Module module = config.getModule();
    assertThat(module.getPageObjectsRootDirectory(), is(equalTo("/src/test/resources/spec")));
    assertThat(module.getPageObjectsOutputDir(), is(equalTo("/src/test/java/pageObjects")));
    assertThat(module.getResourcesOutputDir(), is(equalTo("/src/test/resources")));
    assertThat(module.getUnitTestsOutputDir(), is(equalTo("/src/test/java")));
    assertThat(module.getUnitTestRunnerType(), is(equalTo(UnitTestRunner.JUNIT)));
    assertThat(module.getPageObjectFileMaskRegex(), is(equalTo("(.*)\\.utam\\.json$")));
    assertThat(module.getName(), is(equalTo("myModule")));

    Map<String, String> namespaces = module.getPackagesMapping();
    assertThat(namespaces.keySet(), hasSize(2));
    Namespace namespace = module.namespaces.get(0);
    assertThat(namespace.getTypeMatch(), is(equalTo("utam-one")));
    assertThat(namespaces.get("utam-one"), is(equalTo(".*/one")));
    assertThat(namespace.getPackageMappingKey(), is(equalTo(namespace.getTypeMatch())));
    assertThat(namespace.getPathMatch(), is(equalTo(".*/one")));
    assertThat(namespace.getPackageMappingValue(), is(equalTo(namespace.getPathMatch())));

    List<Profile> profiles = module.getRawProfiles();
    assertThat(profiles, hasSize(1));
    ProfileConfiguration profile = profiles.get(0).getProfileConfiguration();
    assertThat(profile.getPropertyKey(), is(equalTo("platform")));
    assertThat(profile.getSupportedValues(), containsInAnyOrder("ios", "android"));
  }

  @Test
  public void testTargetConfigFromJsonFile() throws IOException {
    JsonCompilerConfig config = getTestConfig();
    Module module = config.getModule();

    TranslatorTargetConfig targetConfig = module.getTargetConfig("path");
    assertThat(targetConfig.getUnitTestRunnerType(), is(equalTo(UnitTestRunner.JUNIT)));
    assertThat(
        targetConfig.getInjectionConfigRootFilePath(), is(equalTo("path/src/test/resources")));
  }

  @Test
  public void testSourceConfigFromJsonFileScanner() throws IOException {
    JsonCompilerConfig config = getTestConfig();
    TranslatorSourceConfig sourceConfig = config.getSourceConfig();
    sourceConfig.recursiveScan();
    Collection<String> scannerResults = sourceConfig.getPageObjects();
    assertThat(scannerResults, hasSize(2));
    assertThat(
        scannerResults, hasItems("utam-one/pageObjects/first", "utam-two/pageObjects/second"));
  }

  @Test
  public void testDefaultModuleProperties() {
    Module module = getTestModule();
    assertThat(module.getPageObjectFileMaskRegex(), is(equalTo(DEFAULT_JSON_FILE_MASK_REGEX)));
    assertThat(module.getName(), is(equalTo("name")));
    assertThat(module.getPageObjectsRootDirectory(), is(equalTo("pageObjectsDirectory")));
    assertThat(module.getConfiguredProfiles(), is(emptyIterable()));
    assertThat(module.getPackagesMapping(), is(anEmptyMap()));
    module.getSourceConfig("", null);
    module.getTargetConfig("");
  }

  @Test
  public void testRootDirectoryWithFileList() {
    Module module = getTestModule();
    List<File> inputFiles = new ArrayList<>();
    inputFiles.add(new File("foo.utam.json"));
    UtamError e = expectThrows(UtamError.class, () -> module.getSourceConfig("", inputFiles));
    assertThat(e.getMessage(), is(equalTo(ERR_FILES_WITHOUT_NAMESPACE)));
  }

  @Test
  public void testNonDefaultModuleProperties() {
    Module module = getTestModule();
    module.namespaces.add(new Namespace("utam-package", "*/package"));
    module.getRawProfiles().add(new Profile("name", new String[] {"values"}));
    assertThat(module.getPageObjectFileMaskRegex(), is(equalTo("(.*)\\.utam\\.json$")));
    assertThat(
        module.getConfiguredProfiles(), hasItems(new StringValueProfileConfig("name", "values")));
    assertThat(module.getPackagesMapping().keySet(), hasItems("utam-package"));
    assertThat(module.getPackagesMapping().values(), hasItems("*/package"));
    module.getTargetConfig("");
  }

  @Test
  public void testNonExistingFile() {
    File wrongFile = new File("error");
    IOException e =
        expectThrows(IOException.class, () -> new JsonCompilerConfig(wrongFile, wrongFile, null));
    assertThat(
        e.getMessage(),
        is(CoreMatchers.equalTo(String.format(ERR_READING_COMPILER_CONFIG, "error"))));
  }

  @Test
  public void testDuplicateNamespace() {
    Module module = getTestModule();
    Namespace namespace = new Namespace("utam-package", "*/package1");
    module.namespaces.add(namespace);
    module.namespaces.add(namespace);
    assertThat(module.getPackagesMapping().keySet(), hasItems("utam-package"));
    module.namespaces.add(new Namespace("utam-package", "*/package2"));
    UtamError e = expectThrows(UtamError.class, module::getPackagesMapping);
    assertThat(
        e.getMessage(),
        is(equalTo(String.format(ERR_DUPLICATE_MAPPING, "utam-package", "*/package1"))));
  }

  @Test
  public void testDuplicateProfiles() {
    Module module = getTestModule();
    Profile profile = new Profile("name", new String[] {"value"});
    module.getRawProfiles().add(profile);
    module.getRawProfiles().add(profile);
    assertThat("duplicate should be ignored", module.getConfiguredProfiles(), hasSize(1));
    module.getRawProfiles().add(new Profile("name", new String[] {"differentValue"}));
    UtamError e = expectThrows(UtamError.class, module::getConfiguredProfiles);
    assertThat(
        e.getMessage(), is(equalTo(String.format(ERR_DUPLICATE_PROFILE_DIFF_VALUES, "name"))));
  }

  @Test
  public void testProfileFromJsonString() throws JsonProcessingException {
    final String PROFILE_NAME = "testProfile";
    final String PROFILE_VALUE_1 = "profileValue1";
    final String PROFILE_VALUE_2 = "profileValue2";

    final String PROFILE_JSON =
        String.format(
            "{\n"
                + "  \"name\": \"%s\",\n"
                + "  \"values\": [\n"
                + "    \"%s\",\n"
                + "    \"%s\"\n"
                + "  ]\n"
                + "}",
            PROFILE_NAME, PROFILE_VALUE_1, PROFILE_VALUE_2);
    ProfileConfiguration profileConfiguration =
        new ObjectMapper().readValue(PROFILE_JSON, Profile.class).getProfileConfiguration();
    assertThat(
        profileConfiguration,
        is(
            equalTo(
                new StringValueProfileConfig(
                    PROFILE_NAME, new String[] {PROFILE_VALUE_1, PROFILE_VALUE_2}))));
  }

  @Test
  public void testProfilesPerModule() {
    Profile profile = new Profile("platform", new String[] {"ios"});
    Module module = getTestModule();
    module.setUniqueProfiles(List.of(profile));
    UtamError e =
        expectThrows(UtamError.class, () -> module.setUniqueProfiles(List.of(profile, profile)));
    assertThat(e.getMessage(), is(equalTo(String.format(ERR_DUPLICATE_PROFILE, "platform"))));
  }

  @Test
  public void testMissingFields() throws IOException {
    JsonCompilerConfig config =
        new JsonCompilerConfig(
            new File(
                JsonCompilerConfig.class
                    .getClassLoader()
                    .getResource("config/nofields.json")
                    .getFile()),
            new File(System.getProperty("user.dir")),
            null);
    assertThat(config.getModuleName(), is(emptyString()));
    assertThat(config.getVersion(), is(emptyString()));
    assertThat(config.getCopyright(), is(empty()));

    Module module = config.getModule();
    assertThat(module.getPageObjectsRootDirectory(), is(equalTo("/src/test/resources/spec")));
    assertThat(module.getPageObjectsOutputDir(), is(equalTo("/src/test/java/pageObjects")));
    assertThat(module.getResourcesOutputDir(), is(equalTo("/src/test/resources")));
    assertThat(module.getUnitTestsOutputDir(), is(equalTo("/src/test/java")));
    assertThat(module.getUnitTestRunnerType(), is(equalTo(UnitTestRunner.JUNIT)));
    assertThat(module.getPageObjectFileMaskRegex(), is(equalTo("(.*)\\.utam\\.json$")));
    assertThat(module.getName(), is(emptyString()));

    Map<String, String> namespaces = module.getPackagesMapping();
    assertThat(namespaces.keySet(), is(empty()));

    List<Profile> profiles = module.getRawProfiles();
    assertThat(profiles, is(empty()));
  }

  @Test
  public void testCopyrightInJson() throws IOException {
    JsonCompilerConfig config = getTestConfig();
    List<String> copyright = config.getCopyright();
    assertThat(copyright, hasSize(2));
    assertThat(copyright.get(0), is(equalTo("copyright")));
    assertThat(copyright.get(1), is(equalTo("test")));
  }
}
