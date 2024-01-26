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

import org.testng.annotations.Test;
import utam.compiler.UtamCompilationError;

/**
 * tests for statement deserializer
 *
 * @author elizaveta.ivanova
 * @since 236
 */
public class UtamMethodActionDeserializerTests {

  private static Exception getError(String jsonFile) {
    return expectThrows(
        UtamCompilationError.class, () -> new DeserializerUtilities().getContext(jsonFile));
  }

  @Test
  public void testRedundantApplyThrows() {
    Exception e = getError("validate/compose/redundantApply");
    assertThat(
        e.getMessage(),
        containsString(
            "error 610: method \"test\" statement: either \"apply\" or \"applyExternal\" should be"
                + " defined"));
  }

  @Test
  public void testEmptyStatementThrows() {
    Exception e = getError("validate/compose/emptyComposeStatement");
    assertThat(
        e.getMessage(),
        containsString(
            "error 609: method \"test\" statement: either \"element\" or \"apply\" or"
                + " \"applyExternal\" should be defined"));
  }

  @Test
  public void testIncorrectPredicateThrows() {
    Exception e = getError("validate/compose/incorrectPredicate");
    assertThat(
        e.getMessage(),
        containsString(
            "error 609: method \"predicate\" statement: either \"element\" or \"apply\" or"
                + " \"applyExternal\" should be defined"));
  }

  @Test
  public void testRedundantElementThrows() {
    Exception e = getError("validate/compose/redundantElement");
    assertThat(
        e.getMessage(),
        containsString(
            "error 611: method \"test\" utility statement: either \"element\" is redundant"));
  }
}
