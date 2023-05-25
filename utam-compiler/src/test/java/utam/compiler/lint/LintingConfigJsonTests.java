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

  private static final String ROOT_DIR = String
      .join(File.separator, System.getProperty("user.dir"), "src", "test", "resources", "lint");

  static TranslatorRunner getRunner(String folder) {
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
    assertThat(errors.get(0).getFullMessage(),
        equalTo("lint rule ULR02 failure in page object utam/pageObjects/test: "
            + "error 2002: root description is missing; "
            + "add \"description\" property at the root"));
    String outputFile = System.getProperty("user.dir")
        + "/src/test/resources/lint/changeDefaultConfig/test.sarif.json";
    assertThat(new File(outputFile).exists(), is(true));
  }

  @Test
  public void testChangingDefaultRulesWithMultipleFiles() {
    TranslatorRunner runner = getRunner("changeGlobalRules");
    List<LintingError> errors = runner.run().getLintingErrors();
    assertThat(errors, hasSize(0));
    String outputFile = System.getProperty("user.dir")
        + "/src/test/resources/lint/changeGlobalRules/utam-lint.sarif";
    assertThat(new File(outputFile).exists(), is(true));
  }

  @Test
  public void testDoNotProduceSarifReport() {
    TranslatorRunner runner = getRunner("report");
    runner.run();
    String outputFile =
        System.getProperty("user.dir") + "/src/test/resources/lint/ignore/utam-lint.sarif";
    assertThat(new File(outputFile).exists(), is(false));
  }

  @Test
  public void testLintingDisabled() {
    TranslatorRunner runner = getRunner("ignore");
    runner.run();
    String outputFile =
        System.getProperty("user.dir") + "/src/test/resources/lint/ignore/utam-lint.sarif";
    assertThat(new File(outputFile).exists(), is(false));
  }

  @Test
  public void testLintingThrowsIfConfigured() {
    TranslatorRunner runner = getRunner("throwConfig");
    Exception e = expectThrows(UtamLintingError.class, runner::run);
    assertThat(e.getMessage(),
        equalTo("UTAM linting failures:\n"
            + "lint rule ULR01 failure in page object utam/pageObjects/test: "
            + "error 2001: duplicate selector \".one\" for the elements \"two\" and \"one\"; "
            + "remove duplicate elements: \"one\" or \"two\""));
  }
}
