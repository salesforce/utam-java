/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static utam.compiler.grammar.DeserializerUtilities.expectCompilerErrorFromFile;

import org.testng.annotations.Test;

/**
 * tests for matcher
 *
 * @author elizaveta.ivanova
 * @since 236
 */
public class UtamMatcherTests {

  @Test
  public void testWrongArgsNumberThrows() {
    Exception e = expectCompilerErrorFromFile("matcher/wrongArgsNumber");
    assertThat(
        e.getMessage(),
        is(
            containsString(
                "error 108: method \"matcherThrows\" statement matcher arguments: expected number"
                    + " of arguments is 1, found 2")));
  }

  @Test
  public void testElementFilterWrongArgsTypeThrows() {
    Exception e = expectCompilerErrorFromFile("matcher/wrongArgsType");
    assertThat(
        e.getMessage(),
        is(
            containsString(
                "error 109: method \"matcherThrows\" statement matcher arguments: argument \"true\""
                    + " has incorrect type, expected \"String\", found \"Boolean\"")));
  }

  @Test
  public void testIncorrectMatcherTypeThrowsInCompose() {
    Exception e = expectCompilerErrorFromFile("matcher/incorrectMatcherInCompose");
    assertThat(
        e.getMessage(),
        containsString(
            "error 1202: method \"matcherThrows\" statement matcher: applied method returns type"
                + " \"String\", which is only compatible with the following matchers -"
                + " stringContains, stringEquals, notNull"));
  }

  @Test
  public void testIncorrectMatcherTypeThrowsInFilter() {
    Exception e = expectCompilerErrorFromFile("matcher/incorrectMatcherInFilter");
    assertThat(
        e.getMessage(),
        containsString(
            "error 1202: element \"test\" filter matcher: applied method returns type \"Boolean\","
                + " which is only compatible with the following matchers - isTrue, isFalse,"
                + " notNull"));
  }

  @Test
  public void testIncorrectMatcherTypeThrowsInFilterVoid() {
    Exception e = expectCompilerErrorFromFile("matcher/incorrectMatcherForVoid");
    assertThat(
        e.getMessage(),
        containsString(
            "error 1202: method \"test\" statement matcher: "
                + "applied method returns type \"void\", "
                + "which is only compatible with the following matchers - none"));
  }
}
