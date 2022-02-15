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
import static utam.compiler.grammar.DeserializerUtilities.expectCompilerErrorFromFile;

import org.testng.annotations.Test;
import utam.compiler.JsonBuilderTestUtility;
import utam.core.framework.consumer.UtamError;

/**
 * @author james.evans
 * @since 228
 */
public class UtamMethodTests {

  private static void test(String jsonFile, String expectedError) {
    UtamError e = expectThrows(UtamError.class,
        () -> new DeserializerUtilities().getContext("validate/compose/" + jsonFile));
    assertThat(e.getMessage(), containsString(expectedError));
  }

  /**
   * The getAbstractMethod method should throw the proper exception if a compose element is
   * specified
   */
  @Test
  public void testGetAbstractMethodWithComposeThrows() {
    Exception e = expectCompilerErrorFromFile("interface/nonEmptyMethod");
    assertThat(e.getMessage(),
        containsString("error UIM000: incorrect format of abstract methods: \n"
            + "Unrecognized field \"compose\""));
  }

  @Test
  public void testComposeEmptyStatementsThrows() {
    test("emptyCompose", "error UM005: method \"test\": compose statements should be a non empty array;");
  }

  @Test
  public void testComposeNullStatementsThrows() {
    test("nullCompose", "error UM005: method \"test\": compose statements should be a non empty array");
  }

  @Test
  public void testIncorrectReturnTypeForInterface() {
    JsonBuilderTestUtility test = new JsonBuilderTestUtility();
    test.addRawString("interface", "true");
    test.addRawString("methods", "[{\"name\":\"test\", \"returnType\":[\"wrong\"]}]");
    Exception e = test.expectCompilerError();
    assertThat(e.getMessage(),
        containsString("error UIM001: abstract method \"test\": return basic type \"[ \"wrong\" ]\" is incorrect"));
  }
}
