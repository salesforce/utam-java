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
    assertThat(e.getMessage(), is(containsString(
        "error 108: method \"matcherThrows\" statement matcher: expected number of parameters is 1, found 2")));
  }

  @Test
  public void testElementFilterWrongArgsTypeThrows() {
    Exception e = expectCompilerErrorFromFile("matcher/wrongArgsType");
    assertThat(e.getMessage(), is(containsString(
        "error 109: method \"matcherThrows\" statement matcher: "
            + "parameter \"true\" has incorrect type: expected \"String\", found \"Boolean\"")));
  }

  @Test
  public void testIncorrectMatcherTypeThrowsInCompose() {
    Exception e = expectCompilerErrorFromFile("matcher/incorrectMatcherInCompose");
    assertThat(e.getMessage(), containsString("error 614: method \"matcherThrows\" statement: "
        + "expected type to match is \"Boolean\", found \"String\""));
  }

  @Test
  public void testIncorrectMatcherTypeThrowsInFilter() {
    Exception e = expectCompilerErrorFromFile("matcher/incorrectMatcherInFilter");
    assertThat(e.getMessage(), containsString("error 203: element \"test\" matcher: "
        + "expected type to match is \"Boolean\", found \"String\""));
  }
}
