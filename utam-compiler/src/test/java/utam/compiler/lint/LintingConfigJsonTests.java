/*
 * Copyright (c) 2022, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.lint;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.hasSize;
import static org.testng.Assert.assertThrows;
import static org.testng.Assert.expectThrows;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import org.testng.annotations.Test;
import utam.compiler.translator.DefaultTranslatorRunner;
import utam.compiler.translator.JsonCompilerConfig;
import utam.core.declarative.lint.LintingError;
import utam.core.declarative.translator.TranslatorRunner;

/**
 * Test JSON config for linting
 *
 * @author elizaveta.ivanova
 * @since 242
 */
public class LintingConfigJsonTests {

  private static final String ROOT_DIR = String
      .join(File.separator, System.getProperty("user.dir"), "src", "test", "resources", "lint");

  private static TranslatorRunner getRunner(String folder) {
    String relativeRoot = String.join(File.separator, ROOT_DIR, folder);
    String configFile = String.join(File.separator, "lint", folder, "config.json");
    File config = new File(
        LintingConfigJsonTests.class.getClassLoader().getResource(configFile).getFile());
    try {
      JsonCompilerConfig jsonCompilerConfig = new JsonCompilerConfig(config, new File(relativeRoot),
          new ArrayList<>());
      return new DefaultTranslatorRunner(jsonCompilerConfig.getTranslatorConfig());
    } catch (IOException e) {
      throw new AssertionError(e);
    }
  }

  @Test
  public void testChangingDefaultRulesWithOneFile() {
    TranslatorRunner runner = getRunner("changeDefaultConfig");
    List<LintingError> errors = runner.run().getLintingErrors();
    assertThat(errors, hasSize(1));
    assertThat(errors.get(0).getMessage(),
        containsString("linting warning 2002: page object utam/pageObjects/test: root description is missing"));
  }

  @Test
  public void testChangingDefaultRulesWithMultipleFiles() {
    TranslatorRunner runner = getRunner("changeGlobalRules");
    List<LintingError> errors = runner.run().getLintingErrors();
    assertThat(errors, hasSize(0));
  }

  @Test
  public void testLintingDoesThrowIfConfigured() {
    TranslatorRunner runner = getRunner("defaultConfig");
    List<LintingError> errors = runner.run().getLintingErrors();
    assertThat(errors, hasSize(3));
  }
}
