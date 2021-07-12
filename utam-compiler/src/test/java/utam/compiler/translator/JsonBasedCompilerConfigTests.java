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
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.emptyString;
import static org.hamcrest.collection.IsCollectionWithSize.hasSize;
import static org.hamcrest.core.IsEqual.equalTo;
import static org.testng.Assert.expectThrows;
import static utam.compiler.translator.DefaultSourceConfiguration.DEFAULT_JSON_FILE_MASK_REGEX;
import static utam.compiler.translator.JsonCompilerConfig.ERR_READING_COMPILER_CONFIG;
import static utam.compiler.translator.JsonCompilerConfig.ERR_TARGET_NOT_SET;

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
import utam.core.framework.context.StringValueProfile;

/**
 * tests for json config
 *
 * @author elizaveta.ivanova
 * @since 234
 */
public class JsonBasedCompilerConfigTests {

  @Test
  public void testValues() throws IOException {
    JsonCompilerConfig config = new JsonCompilerConfig();
    ProfileConfiguration profileConfiguration = config.getConfiguredProfiles().iterator().next();
    assertThat(profileConfiguration.getFromString("web"),
        is(equalTo(new StringValueProfile("platform", "web"))));
    assertThat(config.getModuleName(), is(equalTo("myModule")));
  }

  @Test
  public void testScanner() throws IOException {
    JsonCompilerConfig config = new JsonCompilerConfig();
    TranslatorSourceConfig sourceConfig = config.getSourceConfig();
    sourceConfig.recursiveScan();
    Collection<String> scannerResults = sourceConfig.getPageObjects();
    assertThat(scannerResults, hasSize(2));
    assertThat(scannerResults,
        hasItems("utam-one/pageObjects/first", "utam-two/pageObjects/second"));
  }

  @Test
  public void testDefaultTargetValues() {
    Target target = new Target("src", "res" );
    assertThat(target.pageObjectsPath, is(equalTo("src")));
    assertThat(target.resourcesHomePath, is(equalTo("res")));
    assertThat(target.unitTestRunnerType, is(equalTo(UnitTestRunner.NONE)));
    assertThat(target.unitTestDirectory, is(emptyString()));
  }

  @Test
  public void testDefaultModuleProperties() {
    Module module = new Module("name",
        null,
        "pageObjectsDirectory",
        null,
        new Namespace[0], new Profile[0]);

    assertThat(module.pageObjectFileMask, is(equalTo(DEFAULT_JSON_FILE_MASK_REGEX)));
    assertThat(module.moduleName, is(equalTo("name")));
    assertThat(module.target, is(nullValue()));
    assertThat(module.profiles.length, is(0));
    assertThat(module.namespaces.length, is(0));
    module.getSourceConfig("");
    UtamCompilationError e = expectThrows(UtamCompilationError.class, () -> module.getTargetConfig(""));
    assertThat(e.getMessage(), is(equalTo(ERR_TARGET_NOT_SET)));
  }

  @Test
  public void testNonExistingFile() {
    IOException e = expectThrows(IOException.class, () -> new JsonCompilerConfig("error"));
    assertThat(e.getMessage(),
        is(CoreMatchers.equalTo(String.format(ERR_READING_COMPILER_CONFIG, "error"))));
  }
}
