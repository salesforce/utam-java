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

/**
 * validation tests, generated code is tested under presentation package
 *
 * @since 238
 */
public class UtamElement_BasicTests {

  private static void getContext(String fileName) {
    new DeserializerUtilities().getContext("validate/basic_element/" + fileName);
  }

  @Test
  public void testWithNullSelectorThrows() {
    JsonBuilderTestUtility test = new JsonBuilderTestUtility();
    test.addRawString("elements", "[ {\"name\": \"test\" }]");
    UtamError e = expectThrows(UtamCompilationError.class, test::getDeserializedJson);
    assertThat(e.getMessage(), containsString("error 9: element \"test\": property \"selector\" is required"));
  }

  /**
   * The validateSimpleElement method with a filter and a non-list selector should throw the
   * appropriate exception
   */
  @Test
  public void testBasicElementWithFilterAndNonListSelectorThrows() {
    UtamError e = expectThrows(UtamCompilationError.class, () -> getContext("filterForNonList"));
    assertThat(
        e.getMessage(), containsString("error 302: element \"test\" filter: filter can only be set for list"));
  }

  @Test
  public void testEmptyNestedElementsThrows() {
    UtamError e = expectThrows(UtamError.class, () -> getContext("emptyNestedElementsArray"));
    assertThat(e.getMessage(),
        containsString("error 12: element \"test\" elements: property \"elements\" should be a non-empty array"));
  }

  @Test
  public void testElementWithListCantHaveNestedElements() {
    UtamError e = expectThrows(UtamError.class, () -> getContext("listWithNestedElements"));
    assertThat(e.getMessage(), containsString("error 203: element \"test\": element marked as a list cannot have nested elements or shadow root"));
  }

  @Test
  public void testElementNodeWithInvalidArrayElementTypeThrows() {
    UtamError e = expectThrows(UtamError.class, () -> getContext("wrongBasicTypeArray"));
    assertThat(e.getMessage(), containsString("error 115: element \"test\" type: basic type \"true\" is not supported, " +
            "valid values are: actionable, clickable, draggable, editable, touchable"));
  }

  @Test
  public void testElementTypeAsStringWithInvalidValueThrows() {
    UtamError e = expectThrows(UtamCompilationError.class, () -> getContext("wrongBasicType"));
    assertThat(e.getMessage(), containsString("error 201: element \"test\": type \"wrong\" is not supported, valid values are: " +
            "custom, container, frame, actionable, clickable, draggable, editable, touchable"));
  }

  @Test
  public void testElementNodeWithInvalidArrayElementThrows() {
    UtamError e = expectThrows(UtamError.class, () -> getContext("wrongBasicTypeArrayElement"));
    assertThat(e.getMessage(), containsString("error 115: element \"test\" type: basic type \"wrong\" is not supported, " +
            "valid values are: actionable, clickable, draggable, editable, touchable"));
  }

  @Test
  public void testElementNodeWithDuplicateTypeThrows() {
    UtamError e = expectThrows(UtamError.class, () -> getContext("duplicateBasicType"));
    assertThat(e.getMessage(), containsString("error 116: element \"test\" type: duplicate basic type \"clickable\", basic type values must be unique"));
  }

  @Test
  public void testNavigationElementNameNotAllowed() {
    UtamError e = expectThrows(UtamCompilationError.class, () -> getContext("navigationName"));
    assertThat(e.getMessage(),
        containsString("error 202: element \"navigation\": element with same name was already declared"));
  }
}
