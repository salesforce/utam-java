/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.testng.Assert.expectThrows;
import static utam.compiler.grammar.UtamMethod.ERR_METHOD_EMPTY_STATEMENTS;
import static utam.compiler.grammar.UtamMethod.ERR_METHOD_SHOULD_BE_ABSTRACT;
import static utam.compiler.grammar.UtamMethod.ERR_RETURN_TYPE_ABSTRACT_ONLY;

import org.hamcrest.core.StringContains;
import org.testng.annotations.Test;
import utam.core.framework.consumer.UtamError;

/**
 * @author james.evans
 * @since 228
 */
public class UtamMethodTests {

  private static final String METHOD_NAME = "test";

  private static void test(String jsonFile, String expectedError) {
    UtamError e = expectThrows(UtamError.class,
        () -> new DeserializerUtilities().getContext("validate/compose/" + jsonFile));
    assertThat(e.getMessage(), StringContains.containsString(expectedError));
  }

  /**
   * The getAbstractMethod method should throw the proper exception if a compose element is
   * specified
   */
  @Test
  public void testGetAbstractMethodWithComposeThrows() {
    UtamError e = expectThrows(UtamError.class,
        () -> new DeserializerUtilities().getResultFromFile("interface/nonEmptyMethod"));
    assertThat(e.getMessage(),
        containsString(String.format(ERR_METHOD_SHOULD_BE_ABSTRACT, METHOD_NAME)));
  }

  @Test
  public void testComposeEmptyStatementsThrows() {
    String expectedErr = String.format(ERR_METHOD_EMPTY_STATEMENTS, METHOD_NAME);
    test("emptyCompose", expectedErr);
  }

  @Test
  public void testComposeNullStatementsThrows() {
    String expectedErr = String.format(ERR_METHOD_EMPTY_STATEMENTS, METHOD_NAME);
    test("nullCompose", expectedErr);
  }

  @Test
  public void testComposeReturnNotMatch() {
    String expectedErr = String.format(ERR_RETURN_TYPE_ABSTRACT_ONLY, METHOD_NAME);
    test("returnNotSupported", expectedErr);
  }
}
