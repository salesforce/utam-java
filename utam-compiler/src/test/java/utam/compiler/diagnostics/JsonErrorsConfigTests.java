/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.diagnostics;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.testng.Assert.expectThrows;
import static utam.compiler.diagnostics.JsonErrorsConfig.ERR_CODE_NOT_CONFIGURED;
import static utam.compiler.diagnostics.JsonErrorsConfig.ERR_FINDING_ERROR_CONFIG;
import static utam.compiler.diagnostics.JsonErrorsConfig.ERR_READING_ERROR_CONFIG;
import static utam.compiler.diagnostics.JsonErrorsConfig.getErrorsConfig;

import org.testng.annotations.Test;

/**
 * test configuration of error codes
 *
 * @author elizaveta.ivanova
 * @since 238
 */
public class JsonErrorsConfigTests {

  @Test
  public void testCorrectConfig() {
    JsonErrorsConfig config = getErrorsConfig("config/test_error_config.json");
    String errorMsg = config.getErrorMessage(1);
    assertThat(errorMsg, equalTo("warning 1: message; \nsee documentation docs; \ntip: tip"));
  }

  @Test
  public void testNotExistingCode() {
    JsonErrorsConfig config = getErrorsConfig();
    Exception e = expectThrows(IllegalArgumentException.class, () -> config.getErrorMessage(-100));
    assertThat(e.getMessage(), is(equalTo(String.format(ERR_CODE_NOT_CONFIGURED, -100))));
  }

  @Test
  public void testNotExistingConfig() {
    Exception e = expectThrows(IllegalArgumentException.class, () -> getErrorsConfig("error"));
    assertThat(e.getMessage(), is(equalTo(String.format(ERR_FINDING_ERROR_CONFIG, "error"))));
  }

  @Test
  public void testIncorrectConfig() {
    String fileName = "config/test_wrong_error_config.json";
    Exception e = expectThrows(IllegalStateException.class, () -> getErrorsConfig(fileName));
    assertThat(e.getMessage(), containsString(String.format(ERR_READING_ERROR_CONFIG, fileName)));
  }
}
