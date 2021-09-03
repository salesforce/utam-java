/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.core.StringContains.containsString;
import static org.testng.Assert.expectThrows;
import static utam.compiler.grammar.UtamMethod.ERR_METHOD_EMPTY_STATEMENTS;
import static utam.compiler.grammar.UtamMethod.ERR_RETURN_TYPE_ABSTRACT_ONLY;
import static utam.compiler.helpers.MethodContext.ERR_ARG_DUPLICATE_NAME;
import static utam.compiler.helpers.MethodContext.ERR_PARAMETER_NEVER_USED;

import org.testng.annotations.Test;
import utam.core.framework.consumer.UtamError;

public class UtamMethod_ComposeTests {


  private static final String METHOD_NAME = "test";

  private static void test(String jsonFile, String expectedError) {
    UtamError e = expectThrows(UtamError.class,
        () -> new DeserializerUtilities().getContext("validate/compose/" + jsonFile));
    assertThat(e.getMessage(), containsString(expectedError));
  }

  @Test
  public void testEmptyStatementsThrows() {
    String expectedErr = String.format(ERR_METHOD_EMPTY_STATEMENTS, METHOD_NAME);
    test("emptyCompose", expectedErr);
  }

  @Test
  public void testNullStatementsThrows() {
    String expectedErr = String.format(ERR_METHOD_EMPTY_STATEMENTS, METHOD_NAME);
    test("nullCompose", expectedErr);
  }

  @Test
  public void testReturnNotMatch() {
    String expectedErr = String.format(ERR_RETURN_TYPE_ABSTRACT_ONLY, METHOD_NAME);
    test("returnNotSupported", expectedErr);
  }

  @Test
  public void testSameParameterReusedThrows() {
    String expectedErr = String.format(ERR_ARG_DUPLICATE_NAME, "method 'test'", "arg1");
    test("sameArgsNames", expectedErr);
  }

  @Test
  public void testRedundantMethodLevelParameterThrows() {
    String expectedErr = String.format(ERR_PARAMETER_NEVER_USED, "method 'test'", "arg");
    test("redundantParameters", expectedErr);
  }
}
