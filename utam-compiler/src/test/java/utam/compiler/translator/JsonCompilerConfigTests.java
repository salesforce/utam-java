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
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.anEmptyMap;
import static org.hamcrest.Matchers.emptyIterable;
import static org.hamcrest.Matchers.emptyString;
import static org.hamcrest.collection.IsCollectionWithSize.hasSize;
import static org.hamcrest.core.IsEqual.equalTo;
import static org.testng.Assert.expectThrows;
import static utam.compiler.translator.JsonCompilerConfig.ERR_READING_COMPILER_CONFIG;
import static utam.compiler.translator.JsonCompilerConfig.ERR_TARGET_NOT_SET;
import static utam.compiler.translator.JsonCompilerConfig.Module.DEFAULT_JSON_FILE_MASK_REGEX;
import static utam.compiler.translator.JsonCompilerConfig.Namespace.ERR_DUPLICATE_MAPPING;
import static utam.compiler.translator.JsonCompilerConfig.Profile.ERR_DUPLICATE_PROFILE_DIFF_VALUES;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.File;
import java.io.IOException;
import java.util.Collection;
import org.hamcrest.CoreMatchers;
import org.testng.annotations.Test;
import utam.compiler.UtamCompilationError;
import utam.compiler.translator.JsonCompilerConfig.Module;
import utam.compiler.translator.JsonCompilerConfig.Namespace;
import utam.compiler.translator.JsonCompilerConfig.Profile;
import utam.compiler.translator.JsonCompilerConfig.Target;
import utam.core.declarative.translator.ProfileConfiguration;
import utam.core.declarative.translator.TranslatorSourceConfig;
import utam.core.declarative.translator.UnitTestRunner;
import utam.core.framework.consumer.UtamError;
import utam.core.framework.context.StringValueProfile;

/**
 * tests for json config
 *
 * @author elizaveta.ivanova
 * @since 234
 */
public class JsonCompilerConfigTests {

  private static JsonCompilerConfig getTestConfig() throws IOException {
    return new JsonCompilerConfig(
        new File(JsonCompilerConfig.class.getClassLoader().getResource("utam.config.json").getFile()),
        new File(System.getProperty("user.dir")));
  }

  @Test
  public void testValues() throws IOException {
    JsonCompilerConfig config = getTestConfig();
    ProfileConfiguration profileConfiguration = config.getConfiguredProfiles().iterator().next();
    assertThat(profileConfiguration.getFromString("web"),
        is(equalTo(new StringValueProfile("platform", "web"))));
    assertThat(config.getModuleName(), is(equalTo("myModule")));
  }

  @Test
  public void testScanner() throws IOException {
    JsonCompilerConfig config = getTestConfig();
    TranslatorSourceConfig sourceConfig = config.getSourceConfig();
    sourceConfig.recursiveScan();
    Collection<String> scannerResults = sourceConfig.getPageObjects();
    assertThat(scannerResults, hasSize(2));
    assertThat(scannerResults,
        hasItems("utam-one/pageObjects/first", "utam-two/pageObjects/second"));
  }

  @Test
  public void testDefaultTargetValues() {
    Target target = new Target("src", "res");
    assertThat(target.pageObjectsPath, is(equalTo("src")));
    assertThat(target.resourcesHomePath, is(equalTo("res")));
    assertThat(target.unitTestRunnerType, is(equalTo(UnitTestRunner.NONE)));
    assertThat(target.unitTestDirectory, is(emptyString()));
  }

  @Test
  public void testDefaultModuleProperties() {
    Module module = new Module("name", "pageObjectsDirectory");
    assertThat(module.getPageObjectFileMaskRegex(), is(equalTo(DEFAULT_JSON_FILE_MASK_REGEX)));
    assertThat(module.getName(), is(equalTo("name")));
    assertThat(module.getPageObjectsDirectory(), is(equalTo("pageObjectsDirectory")));
    assertThat(module.target, is(nullValue()));
    assertThat(module.getConfiguredProfiles(), is(emptyIterable()));
    assertThat(module.getPackagesMapping(), is(anEmptyMap()));
    module.getSourceConfig("");
    UtamCompilationError e = expectThrows(UtamCompilationError.class,
        () -> module.getTargetConfig(""));
    assertThat(e.getMessage(), is(equalTo(ERR_TARGET_NOT_SET)));
  }

  @Test
  public void testNonDefaultModuleProperties() {
    Module module = new Module("name",
        "fileMask",
        "pageObjectsDirectory",
        new Target("pageObjects", "resources"));
    module.namespaces.add(new Namespace("utam-package", "*/package"));
    module.getRawProfiles().add(new Profile("name", new String[]{"values"}));
    assertThat(module.getPageObjectFileMaskRegex(), is(equalTo("fileMask")));
    assertThat(module.target, is(not(nullValue())));
    assertThat(module.getConfiguredProfiles(),
        hasItems(new StringValueProfileConfig("name", "values")));
    assertThat(module.getPackagesMapping().keySet(), hasItems("utam-package"));
    assertThat(module.getPackagesMapping().values(), hasItems("*/package"));
    module.getTargetConfig("");
  }

  @Test
  public void testNonExistingFile() {
    File wrongFile = new File("error");
    IOException e = expectThrows(IOException.class, () -> new JsonCompilerConfig(wrongFile, wrongFile));
    assertThat(e.getMessage(),
        is(CoreMatchers.equalTo(String.format(ERR_READING_COMPILER_CONFIG, "error"))));
  }

  @Test
  public void testDuplicateNamespace() {
    Module module = new Module("name", "pageObjectsDirectory");
    Namespace namespace = new Namespace("utam-package", "*/package1");
    module.namespaces.add(namespace);
    module.namespaces.add(namespace);
    assertThat(module.getPackagesMapping().keySet(), hasItems("utam-package"));
    module.namespaces.add(new Namespace("utam-package", "*/package2"));
    UtamError e = expectThrows(UtamError.class, module::getPackagesMapping);
    assertThat(e.getMessage(),
        is(equalTo(String.format(ERR_DUPLICATE_MAPPING, "utam-package", "*/package1"))));
  }

  @Test
  public void testDuplicateProfiles() {
    Module module = new Module("name", "pageObjectsDirectory");
    Profile profile = new Profile("name", new String[]{"value"});
    module.getRawProfiles().add(profile);
    module.getRawProfiles().add(profile);
    assertThat("duplicate should be ignored", module.getConfiguredProfiles(), hasSize(1));
    module.getRawProfiles().add(new Profile("name", new String[]{"differentValue"}));
    UtamError e = expectThrows(UtamError.class, module::getConfiguredProfiles);
    assertThat(e.getMessage(),
        is(equalTo(String.format(ERR_DUPLICATE_PROFILE_DIFF_VALUES, "name"))));
  }

  @Test
  public void testProfileFromJsonString() throws JsonProcessingException {
    final String PROFILE_NAME = "testProfile";
    final String PROFILE_VALUE_1 = "profileValue1";
    final String PROFILE_VALUE_2 = "profileValue2";

    final String PROFILE_JSON = String.format("{\n"
            + "  \"name\": \"%s\",\n"
            + "  \"values\": [\n"
            + "    \"%s\",\n"
            + "    \"%s\"\n"
            + "  ]\n"
            + "}",
        PROFILE_NAME,
        PROFILE_VALUE_1,
        PROFILE_VALUE_2);
    ProfileConfiguration profileConfiguration = new ObjectMapper()
        .readValue(PROFILE_JSON, Profile.class).getProfileConfiguration();
    assertThat(profileConfiguration,
        is(equalTo(new StringValueProfileConfig(PROFILE_NAME, new String[]{PROFILE_VALUE_1, PROFILE_VALUE_2}))));
  }

  @Test
  public void testModuleFromJson() throws IOException {
    final String MODULE_NAME = "testModule";
    final String MODULE_PAGE_OBJECTS_DIRECTORY = "page/objects/directory";
    final String TYPE_PREFIX = "utam-one";
    final String TYPE_FOLDER_REGEX = "*/one";

    final String MODULE_JSON = String.format("{\n"
            + "  \"name\": \"%s\",\n"
            + "  \"pageObjectsDirectory\": \"%s\",\n"
            + "  \"namespaces\": [\n"
            + "    {"
            + "      \"typePrefix\": \"%s\",\n"
            + "      \"pathMatchRegex\": \"%s\"\n"
            + "    }\n"
            + "  ]\n"
            + "}",
        MODULE_NAME,
        MODULE_PAGE_OBJECTS_DIRECTORY,
        TYPE_PREFIX,
        TYPE_FOLDER_REGEX);

    Module module = new ObjectMapper().readValue(MODULE_JSON, Module.class);
    assertThat(module.getName(), is(equalTo(MODULE_NAME)));
    assertThat(module.getPageObjectsDirectory(), is(equalTo(MODULE_PAGE_OBJECTS_DIRECTORY)));
    assertThat(module.namespaces, hasSize(1));
    Namespace namespace = module.namespaces.get(0);
    assertThat(namespace.getTypePrefix(), is(equalTo(TYPE_PREFIX)));
    assertThat(namespace.getPackageMappingKey(), is(equalTo(TYPE_PREFIX)));
    assertThat(namespace.getPathMatchRegex(), is(equalTo(TYPE_FOLDER_REGEX)));
    assertThat(namespace.getPackageMappingValue(), is(equalTo(TYPE_FOLDER_REGEX)));
  }
}
