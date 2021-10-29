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
import static utam.compiler.grammar.UtamMethodActionDeserializer.ERR_REDUNDANT_ELEMENT;
import static utam.compiler.grammar.UtamMethodActionDeserializer.ERR_REDUNDANT_KEYS;
import static utam.compiler.grammar.UtamMethodActionDeserializer.ERR_REQUIRED_KEYS;

import org.testng.annotations.Test;
import utam.core.framework.consumer.UtamError;

/**
 * tests for statement deserializer
 *
 * @author elizaveta.ivanova
 * @since 236
 */
public class UtamMethodActionDeserializerTests {

  private static void test(String jsonFile, String expectedError) {
    UtamError e = expectThrows(UtamError.class,
        () -> new DeserializerUtilities().getContext(jsonFile));
    assertThat(e.getCause().getMessage(), containsString(expectedError));
  }

  @Test
  public void testRedundantApplyThrows() {
    test("validate/compose/redundantApply", ERR_REDUNDANT_KEYS);
  }

  @Test
  public void testEmptyStatementThrows() {
    test("validate/compose/emptyComposeStatement", ERR_REQUIRED_KEYS);
  }

  @Test
  public void testIncorrectPredicateThrows() {
    test("validate/compose/incorrectPredicate", ERR_REQUIRED_KEYS);
  }

  @Test
  public void testRedundantElementThrows() {
    test("validate/compose/redundantElement", ERR_REDUNDANT_ELEMENT);
  }

  @Test
  public void matcherNotSupportedForUtilities() {
    String expectedError = "Unrecognized field \"matcher\"";
    test("validate/compose/utilWithMatcher", expectedError);
  }

  @Test
  public void chainNotSupportedForUtilities() {
    String expectedError = "Unrecognized field \"chain\"";
    test("validate/compose/utilWithChain", expectedError);
  }

  @Test
  public void elementNotSupportedForUtilities() {
    String expectedError = "should not have 'element' property";
    test("validate/compose/utilWithElement", expectedError);
  }
}
