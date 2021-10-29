/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.endsWith;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.testng.Assert.expectThrows;
import static utam.compiler.grammar.TestUtilities.getTestTranslationContext;
import static utam.compiler.grammar.UtamArgumentTests.getLiteralArg;

import org.testng.annotations.Test;
import utam.compiler.helpers.MatcherType;
import utam.compiler.helpers.PrimitiveType;
import utam.core.framework.consumer.UtamError;

/**
 * tests for matcher
 *
 * @author elizaveta.ivanova
 * @since 236
 */
public class UtamMatcherTests {

  @Test
  public void testMethodStatementAction() {
    UtamMatcher matcher = new UtamMatcher(MatcherType.isFalse, null);
    matcher.getParameters(getTestTranslationContext(), "element");

    matcher = new UtamMatcher(MatcherType.isTrue, new UtamArgument[0]);
    matcher.getParameters(getTestTranslationContext(), "element");
  }

  @Test
  public void testMethodStatementActionWrongArgsNumberThrows() {
    UtamMatcher matcher = new UtamMatcher(MatcherType.isTrue,
        new UtamArgument[]{getLiteralArg()});
    UtamError e = expectThrows(UtamError.class,
        () -> matcher.getParameters(getTestTranslationContext(), "element"));
    assertThat(e.getMessage(), is(endsWith("expected 0 parameters, provided 1")));
  }

  @Test
  public void testElementFilterWrongArgsTypeThrows() {
    UtamMatcher matcher = new UtamMatcher(MatcherType.stringContains,
        new UtamArgument[]{getLiteralArg()});
    UtamError e = expectThrows(UtamError.class,
        () -> matcher.getParameters(getTestTranslationContext(), "element"));
    assertThat(e.getMessage(), is(containsString("expected type is")));
  }

  @Test
  public void testIncorrectMatcherTypeThrowsInCompose() {
    UtamError e = expectThrows(UtamError.class,
        () -> new DeserializerUtilities().getContext("matcher/incorrectMatcherInCompose"));
    assertThat(
        e.getMessage(),
        containsString(MatcherType.isTrue.getIncorrectTypeError(PrimitiveType.STRING)));
  }

  @Test
  public void testIncorrectMatcherTypeThrowsInFilter() {
    UtamError e = expectThrows(UtamError.class,
        () -> new DeserializerUtilities().getContext("matcher/incorrectMatcherInFilter"));
    assertThat(
        e.getMessage(),
        containsString(MatcherType.isTrue.getIncorrectTypeError(PrimitiveType.STRING)));
  }
}
