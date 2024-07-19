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
import static utam.compiler.grammar.DeserializerUtilities.expectCompilerErrorFromFile;

import org.testng.annotations.Test;
import utam.compiler.JsonBuilderTestUtility;

/**
 * Provides tests for the UtamShadowElement class
 *
 * @author james.evans
 */
public class UtamShadowElementTests {

  @Test
  public void testShadowElementsMissingThrows() {
    JsonBuilderTestUtility test = new JsonBuilderTestUtility();
    test.addRawString("shadow", "{} ");
    Exception e = test.expectCompilerError();
    assertThat(
        e.getMessage(),
        containsString("error 12: root shadow: property \"elements\" should be a non-empty array"));
  }

  @Test
  public void testNestedShadowMissingElements() {
    Exception e = expectCompilerErrorFromFile("validate/shadow/nestedElementsMissing");
    assertThat(
        e.getMessage(),
        containsString(
            "error 12: element \"parent\" shadow: property \"elements\" should be a non-empty"
                + " array"));
  }

  @Test
  public void testIncorrectElementInsideShadowThrows() {
    Exception e = expectCompilerErrorFromFile("validate/shadow/incorrectElement");
    assertThat(
        e.getMessage(),
        containsString("error 1100: root shadow: incorrect format of elements inside shadow"));
  }

  @Test
  public void testNestedShadowElementsEmptyArray() {
    Exception e = expectCompilerErrorFromFile("validate/shadow/nestedElementsNotArray");
    assertThat(
        e.getMessage(),
        containsString(
            "error 12: element \"parent\" shadow: property \"elements\" should be a non-empty"
                + " array"));
  }
}
