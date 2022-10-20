/*
 * Copyright (c) 2022, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.lint;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static org.testng.Assert.expectThrows;
import static utam.compiler.lint.LintingConfigJson.LINTING_EXCEPTION_PREFIX;
import static utam.compiler.lint.LintingErrorImpl.buildFullErrorMessage;

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
    LintingError error = errors.get(0);
    String errMsg = buildFullErrorMessage("utam/pageObjects/test", error,
        "root description is missing");
    assertThat(errors.get(0).getFullMessage(), equalTo(errMsg));
    String outputFile = System.getProperty("user.dir") + "/src/test/resources/lint/changeDefaultConfig/test.sarif.json";
    assertThat(new File(outputFile).exists(), is(true));
  }

  @Test
  public void testChangingDefaultRulesWithMultipleFiles() {
    TranslatorRunner runner = getRunner("changeGlobalRules");
    List<LintingError> errors = runner.run().getLintingErrors();
    assertThat(errors, hasSize(0));
    String outputFile = System.getProperty("user.dir") + "/src/test/resources/lint/changeGlobalRules/utam-lint.sarif";
    assertThat(new File(outputFile).exists(), is(true));
  }

  @Test
  public void testLintingThrowsIfConfigured() {
    TranslatorRunner runner = getRunner("throwConfig");
    Exception e = expectThrows(UtamLintingError.class, runner::run);
    assertThat(e.getMessage(),
        containsString("lint ULR01 error in page object utam/pageObjects/test: "
            + "duplicate selector \"By.cssSelector: .one\" for the elements \"two\" and \"one\""));
    assertThat(e.getMessage(), containsString(LINTING_EXCEPTION_PREFIX));
  }
}
