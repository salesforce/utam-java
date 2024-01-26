/*
 * Copyright (c) 2022, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.lint;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
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

  private static final String ROOT_DIR =
      String.join(
          File.separator, System.getProperty("user.dir"), "src", "test", "resources", "lint");

  static TranslatorRunner getRunner(String folder) {
    String relativeRoot = String.join(File.separator, ROOT_DIR, folder);
    String configFile = String.join(File.separator, "lint", folder, "config.json");
    File config =
        new File(LintingConfigJsonTests.class.getClassLoader().getResource(configFile).getFile());
    try {
      JsonCompilerConfig jsonCompilerConfig =
          new JsonCompilerConfig(config, new File(relativeRoot), new ArrayList<>());
      return new DefaultTranslatorRunner(jsonCompilerConfig.getTranslatorConfig());
    } catch (IOException e) {
      throw new AssertionError(e);
    }
  }

  private static File getOutputSarifFile(String relativePath, String fileName) {
    String filePath = String.join(File.separator, ROOT_DIR, relativePath, fileName);
    return new File(filePath);
  }

  @Test
  public void testChangingDefaultRulesWithOneFile() {
    TranslatorRunner runner = getRunner("changeDefaultConfig");
    List<LintingError> errors = runner.run().getLintingErrors();
    assertThat(errors, hasSize(1));
    assertThat(
        errors.get(0).getFullMessage(),
        equalTo(
            "lint rule ULR02 failure in page object utam/pageObjects/test: "
                + "error 2002: root description is missing; "
                + "add \"description\" property at the root"));
    File sarif = getOutputSarifFile("changeDefaultConfig", "test.sarif.json");
    assertThat(sarif.exists(), is(true));
  }

  @Test
  public void testChangingDefaultRulesWithMultipleFiles() {
    TranslatorRunner runner = getRunner("changeGlobalRules");
    List<LintingError> errors = runner.run().getLintingErrors();
    assertThat(errors, hasSize(0));
    File sarif = getOutputSarifFile("changeGlobalRules", "utam-lint.sarif");
    assertThat(sarif.exists(), is(true));
  }

  @Test
  public void testLintingDisabled() {
    TranslatorRunner runner = getRunner("ignore");
    runner.run();
    File sarif = getOutputSarifFile("ignore", "utam-lint.sarif");
    assertThat(sarif.exists(), is(false));
  }

  @Test
  public void testLintingThrowsIfConfigured() {
    TranslatorRunner runner = getRunner("throwConfig");
    Exception e = expectThrows(UtamLintingError.class, runner::run);
    assertThat(
        e.getMessage(), equalTo("UTAM linting failed, please check SARIF report utam-lint.sarif"));
    File sarif = getOutputSarifFile("throwConfig", "utam-lint.sarif");
    assertThat(sarif.exists(), is(true));
  }

  @Test
  public void testLintingDoesNotThrowIfConfiguredWithoutErrors() {
    TranslatorRunner runner = getRunner("throwConfigPass");
    runner.run();
    File sarif = getOutputSarifFile("throwConfigPass", "utam-lint.sarif");
    assertThat(sarif.exists(), is(true));
  }
}
