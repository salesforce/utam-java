/*
 * Copyright (c) 2022, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.translator;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.not;
import static org.testng.Assert.expectThrows;

import java.io.File;
import java.io.IOException;
import org.testng.annotations.Test;
import utam.compiler.UtamCompilationError;

/**
 * Tests for compiler errors config
 *
 * @author elizaveta.ivanova
 * @since 244
 */
@Test
public class ErrorConfigTests {

  private static void runCompiler(String config) throws IOException {
    TranslatorGenerationCommand command = new TranslatorGenerationCommand();
    String baseDir = System.getProperty("user.dir");
    command.compilerRoot = new File(baseDir);
    command.jsonConfig = new File(baseDir + config);
    command.test();
  }

  @Test
  public void testCombiningErrorsIntoOneReport() {
    // report.json config has "interruptCompilerOnError" set to false
    Exception e =
        expectThrows(
            UtamCompilationError.class,
            () -> runCompiler("/src/test/resources/errors/report.json"));
    assertThat(e.getMessage(), containsString("page object 'errors-spec/pageObjects/error'"));
    assertThat(
        e.getMessage(),
        containsString(
            "error 900: incorrect format of the page object \n" + "Unrecognized field \"error\""));
    assertThat(e.getMessage(), containsString("page object 'errors-spec/pageObjects/root'"));
    assertThat(
        e.getMessage(),
        containsString("error 902: root page object requires \"selector\" property"));
  }

  @Test
  public void testDefaultCompilerInterruption() {
    // interrupt.json config has no "interruptCompilerOnError", default is true
    Exception e =
        expectThrows(
            UtamCompilationError.class,
            () -> runCompiler("/src/test/resources/errors/interrupt.json"));
    assertThat(e.getMessage(), containsString("page object 'errors-spec/pageObjects/error'"));
    assertThat(e.getMessage(), not(containsString("page object 'errors-spec/pageObjects/root'")));
  }
}
