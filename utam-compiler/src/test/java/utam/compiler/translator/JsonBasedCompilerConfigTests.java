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
import static org.hamcrest.collection.IsCollectionWithSize.hasSize;
import static org.hamcrest.core.IsEqual.equalTo;
import static org.testng.Assert.expectThrows;
import static utam.compiler.translator.DefaultSourceConfiguration.DEFAULT_JSON_FILE_MASK_REGEX;
import static utam.compiler.translator.JsonBasedCompilerConfig.ERR_READING_COMPILER_CONFIG;

import java.io.IOException;
import java.util.Collection;
import java.util.HashMap;
import org.hamcrest.CoreMatchers;
import org.testng.annotations.Test;
import utam.compiler.translator.JsonBasedCompilerConfig.ModuleConfig;
import utam.compiler.translator.JsonBasedCompilerConfig.Source;
import utam.compiler.translator.JsonBasedCompilerConfig.Target;
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
    JsonBasedCompilerConfig config = new JsonBasedCompilerConfig();
    ProfileConfiguration profileConfiguration = config.getConfiguredProfiles().iterator().next();
    assertThat(profileConfiguration.getFromString("web"),
        is(equalTo(new StringValueProfile("platform", "web"))));
    assertThat(config.getModuleName(), is(equalTo("myModule")));
  }

  @Test
  public void testScanner() throws IOException {
    JsonBasedCompilerConfig config = new JsonBasedCompilerConfig();
    TranslatorSourceConfig sourceConfig = config.getSourceConfig();
    sourceConfig.recursiveScan();
    Collection<String> scannerResults = sourceConfig.getPageObjects();
    assertThat(scannerResults, hasSize(2));
    assertThat(scannerResults,
        hasItems("utam-one/pageObjects/first", "utam-two/pageObjects/second"));
  }

  @Test
  public void testDefaultModuleProperties() {
    Source source = new Source(null, "root", new HashMap<>());
    assertThat(source.filesMaskRegex, is(equalTo(DEFAULT_JSON_FILE_MASK_REGEX)));
    Target target = new Target("path", "path", "path", null);
    assertThat(target.unitTestRunnerType, is(equalTo(UnitTestRunner.NONE)));
    ModuleConfig config = new ModuleConfig("moduleName", source, target, null);
    assertThat(config.profiles.length, is(0));
  }

  @Test
  public void testNonExistingFile() {
    IOException e = expectThrows(IOException.class, () -> new JsonBasedCompilerConfig("error"));
    assertThat(e.getMessage(),
        is(CoreMatchers.equalTo(String.format(ERR_READING_COMPILER_CONFIG, "error"))));
  }
}
