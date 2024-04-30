/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.testng.Assert.expectThrows;

import org.testng.annotations.Test;
import utam.compiler.UtamCompilationError;

/**
 * Test reads JSON file with declared frames
 *
 * @since 236
 */
public class UtamElementFrameTests {

  private static final String VALIDATION_DATA_ROOT = "validate/frame/";

  private static void testThrows(String fileName, String errorMsg) {
    UtamCompilationError e =
        expectThrows(
            UtamCompilationError.class,
            () -> new DeserializerUtilities().getContext(VALIDATION_DATA_ROOT + fileName));
    assertThat(e.getMessage(), containsString(errorMsg));
  }

  @Test
  public void testFrameCantHaveNestedElements() {
    testThrows(
        "nestedElements",
        "error 205: element \"test\": only basic, custom or container element can have nested"
            + " elements or shadow root");
  }

  @Test
  public void testFrameCantHaveNestedShadowElements() {
    testThrows(
        "nestedShadowElements",
        "error 205: element \"test\": only basic, custom or container element can have nested"
            + " elements or shadow root");
  }

  @Test
  public void testFrameElementWithReturnAllSelectorThrows() {
    testThrows(
        "returnAll",
        "error 204: element \"test\": frame selector cannot have \"returnAll\" set to true");
  }

  @Test
  public void testFrameMissingSelectorThrows() {
    testThrows("missingSelector", "error 9: element \"test\": property \"selector\" is required");
  }

  @Test
  public void testFrameElementWithNullableThrows() {
    testThrows(
        "nullable",
        "error 8: element \"test\": property \"nullable\" is not supported, "
            + "supported are: name, public, selector, type");
  }
}
