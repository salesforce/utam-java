/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.translator;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static utam.compiler.translator.TranslatorGenerationCommand.CONFIG_ERR;
import static utam.compiler.translator.TranslatorGenerationCommand.INVALID_UNIT_TEST_CONFIG;
import static utam.compiler.translator.TranslatorGenerationCommand.MISSING_INPUT;
import static utam.compiler.translator.TranslatorGenerationCommand.OUTPUT_DIRECTORY_MISSING;
import static utam.compiler.translator.TranslatorGenerationCommand.PACKAGE_CONFIG_MISSING;
import static utam.compiler.translator.TranslatorGenerationCommand.REDUNDANT_CLI_ARGS;
import static utam.compiler.translator.TranslatorGenerationCommand.RUNTIME_ERR;
import static utam.compiler.translator.TranslatorGenerationCommand.TOO_MANY_INPUTS;

import java.io.File;
import java.io.IOException;
import java.util.Collections;
import org.testng.annotations.Test;
import utam.core.declarative.translator.TranslatorConfig;
import utam.core.declarative.translator.UnitTestRunner;

/**
 * @author elizaveta.ivanova
 * @since 234
 */
public class TranslatorGenerationCommandTests {

  @Test
  public void testJsonConfig() {
    TranslatorGenerationCommand command = new TranslatorGenerationCommand();
    command.jsonConfig = new File(System.getProperty("user.dir")
        + "/src/test/resources/utam.config.json");
    TranslatorConfig config = command.getTranslationConfig();
    assertThat(config, is(not(nullValue())));
  }

  @Test
  public void testJsonConfigWithOtherArgsThrows() {
    TranslatorGenerationCommand command = new TranslatorGenerationCommand();
    command.jsonConfig = new File("utam.config");
    command.inputDirectory = new File("input");
    TranslatorConfig config = command.getTranslationConfig();
    assertThat(config, is(nullValue()));
    assertThat(command.returnCode, is(equalTo(CONFIG_ERR)));
    assertThat(command.getThrownError().getMessage(), containsString(REDUNDANT_CLI_ARGS));
  }

  @Test
  public void testJsonConfigNotExistingThrows() {
    TranslatorGenerationCommand command = new TranslatorGenerationCommand();
    command.jsonConfig = new File("error.config");
    TranslatorConfig config = command.getTranslationConfig();
    assertThat(command.returnCode, is(equalTo(RUNTIME_ERR)));
    assertThat(command.getThrownError(), is(instanceOf(IOException.class)));
    assertThat(config, is(nullValue()));
    command.call();
    assertThat(command.returnCode, is(equalTo(RUNTIME_ERR)));
  }

  @Test
  public void testCLIConfigErrors() {
    TranslatorGenerationCommand command = new TranslatorGenerationCommand();
    assertThat(command.getTranslationConfig(), is(nullValue()));
    assertThat(command.returnCode, is(equalTo(CONFIG_ERR)));
    assertThat(command.getThrownError().getMessage(), containsString(MISSING_INPUT));
    command.inputDirectory = new File("test");
    command.inputFiles = Collections.singletonList(new File("test"));
    assertThat(command.getTranslationConfig(), is(nullValue()));
    assertThat(command.returnCode, is(equalTo(CONFIG_ERR)));
    assertThat(command.getThrownError().getMessage(), containsString(TOO_MANY_INPUTS));
    command.inputFiles = null;
    command.testRunner = UnitTestRunner.JUNIT;
    assertThat(command.getTranslationConfig(), is(nullValue()));
    assertThat(command.returnCode, is(equalTo(CONFIG_ERR)));
    assertThat(command.getThrownError().getMessage(), containsString(INVALID_UNIT_TEST_CONFIG));
    command.testRunner = UnitTestRunner.NONE;
    assertThat(command.getTranslationConfig(), is(nullValue()));
    assertThat(command.returnCode, is(equalTo(CONFIG_ERR)));
    assertThat(command.getThrownError().getMessage(), containsString(OUTPUT_DIRECTORY_MISSING));
    command.outputDirectory = new File("output");
    assertThat(command.getTranslationConfig(), is(nullValue()));
    assertThat(command.returnCode, is(equalTo(CONFIG_ERR)));
    assertThat(command.getThrownError().getMessage(), containsString(PACKAGE_CONFIG_MISSING));
    command.packageMappingFile = new File("packages");
    assertThat(command.getTranslationConfig(), is(nullValue()));
    assertThat(command.returnCode, is(equalTo(RUNTIME_ERR)));
    assertThat(command.getThrownError(), instanceOf(IOException.class));
  }
}
