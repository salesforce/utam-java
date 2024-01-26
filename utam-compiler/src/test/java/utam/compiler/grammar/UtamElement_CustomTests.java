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

import org.testng.annotations.Test;
import utam.compiler.JsonBuilderTestUtility;
import utam.compiler.UtamCompilationError;
import utam.core.framework.consumer.UtamError;

public class UtamElement_CustomTests {

  private static final String METHOD_NAME = "getTest";

  /**
   * The validateComponentElement method with a component and a null selector should throw the
   * appropriate exception
   */
  @Test
  public void testValidateComponentElementWithNullSelectorThrows() {
    JsonBuilderTestUtility test = new JsonBuilderTestUtility();
    test.addRawString("elements", "[ {\"name\": \"test\", \"type\" : \"my/page/type\" }]");
    UtamError e = expectThrows(UtamCompilationError.class, test::getDeserializedJson);
    assertThat(
        e.getMessage(),
        containsString("error 9: element \"test\": property \"selector\" is required"));
  }

  @Test
  public void testDuplicateArgsNamesThrows() {
    UtamCompilationError e =
        expectThrows(
            UtamCompilationError.class,
            () ->
                new DeserializerUtilities()
                    .getContext("element/customDuplicateArgs")
                    .getMethod(METHOD_NAME));
    assertThat(
        e.getMessage(),
        containsString(
            "error 107: method 'getTestElement' arguments: argument with name \"arg\" is already"
                + " declared"));
  }

  @Test
  public void testContainerCantHaveNestedElements() {
    UtamCompilationError e =
        expectThrows(
            UtamCompilationError.class,
            () -> new DeserializerUtilities().getContext("validate/custom_element/nestedElements"));
    assertThat(
        e.getMessage(),
        containsString(
            "error 205: element \"test\": only basic element can have nested elements or shadow"
                + " root"));
  }
}
