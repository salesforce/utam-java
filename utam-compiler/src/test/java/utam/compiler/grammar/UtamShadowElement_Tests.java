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
public class UtamShadowElement_Tests {

  @Test
  public void testShadowElementsMissingInRoot() {
    JsonBuilderTestUtility test = new JsonBuilderTestUtility();
    test.addRawString("shadow", "{} ");
    Exception e = test.expectCompilerError();
    assertThat(e.getMessage(), containsString(
        "error USE000: root shadow: incorrect format of elements inside shadow: \n"
            + "Missing required creator property 'elements'"));
  }

  @Test
  public void testNestedShadowMissingElements() {
    Exception e = expectCompilerErrorFromFile("validate/shadow/nestedElementsMissing");
    assertThat(e.getMessage(), containsString(
        "error USE000: element \"parent\" shadow: incorrect format of elements inside shadow: \n"
            + "Missing required creator property 'elements'"));
  }

  @Test
  public void testNestedShadowElementsEmptyArray() {
    Exception e = expectCompilerErrorFromFile("validate/shadow/nestedElementsNotArray");
    assertThat(e.getMessage(), containsString(
        "error U0004: element \"parent\" shadow: property \"elements\" should not be an empty array"));
  }
}
