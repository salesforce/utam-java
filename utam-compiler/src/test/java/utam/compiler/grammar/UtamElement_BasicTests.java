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
import static utam.compiler.grammar.TestUtilities.JACKSON_MISSING_REQUIRED_PROPERTY_ERROR;
import static utam.compiler.grammar.TestUtilities.UtamEntityCreator;
import static utam.compiler.grammar.TestUtilities.getDeserializedObject;
import static utam.compiler.grammar.UtamElement.ERR_ELEMENT_FILTER_NEEDS_LIST;
import static utam.compiler.grammar.UtamElement.ERR_ELEMENT_MISSING_SELECTOR_PROPERTY;
import static utam.compiler.grammar.UtamElement.ERR_ELEMENT_NESTED_ELEMENTS;
import static utam.compiler.grammar.UtamElementFilter_Tests.getInnerTextFilter;
import static utam.compiler.grammar.UtamSelectorTests.getListCssSelector;
import static utam.compiler.grammar.UtamSelectorTests.getUtamCssSelector;
import static utam.compiler.types.BasicElementInterface.ERR_UNSUPPORTED_ELEMENT_TYPE;

import org.testng.annotations.Test;
import utam.core.framework.consumer.UtamError;

/**
 * validation tests, generated code is tested under presentation package
 *
 * @since 238
 */
public class UtamElement_BasicTests {

  private static final String ELEMENT_NAME = "test";

  private static void testElement(UtamSelector selector, UtamElementFilter filter) {
    UtamElement utamElement = UtamEntityCreator.createUtamElement(ELEMENT_NAME);
    utamElement.selector = selector;
    utamElement.filter = filter;
    utamElement.isPublic = true;
    utamElement.getAbstraction();
  }

  private static void getContext(String fileName) {
    new DeserializerUtilities().getContext("element/" + fileName);
  }

  /**
   * The validateSimpleElement method with a filter and a null selector should throw the appropriate
   * exception
   */
  @Test
  public void testValidateSimpleElementWithFilterAndNullSelectorThrows() {
    UtamError e = expectThrows(UtamError.class, () -> testElement(null, null));
    assertThat(
        e.getMessage(),
        containsString(String.format(ERR_ELEMENT_MISSING_SELECTOR_PROPERTY, ELEMENT_NAME)));
  }

  /**
   * The validateSimpleElement method with a filter and a non-list selector should throw the
   * appropriate exception
   */
  @Test
  public void testValidateSimpleElementWithFilterAndNonListSelectorThrows() {
    UtamError e = expectThrows(UtamError.class, () -> testElement(getUtamCssSelector(), getInnerTextFilter()));
    assertThat(
        e.getMessage(), containsString(String.format(ERR_ELEMENT_FILTER_NEEDS_LIST, ELEMENT_NAME)));
  }

  @Test
  public void testElementWithListCantHaveNestedElements() {
    UtamElement element = UtamEntityCreator.createUtamElement(ELEMENT_NAME);
    element.selector = getListCssSelector();
    element.elements = new UtamElement[0];
    UtamError e = expectThrows(UtamError.class, element::getAbstraction);
    assertThat(
        e.getMessage(), containsString(String.format(ERR_ELEMENT_NESTED_ELEMENTS, ELEMENT_NAME)));
  }

  @Test
  public void testDeserializationWithoutNameThrows() {
    String json = "{}";
    UtamError e =
        expectThrows(UtamError.class, () -> getDeserializedObject(json, UtamElement.class));
    assertThat(e.getCause().getMessage(), containsString(JACKSON_MISSING_REQUIRED_PROPERTY_ERROR));
  }

  @Test
  public void testElementNodeWithInvalidArrayElementTypeThrows() {
    String expectedError = String.format(ERR_UNSUPPORTED_ELEMENT_TYPE, ELEMENT_NAME, "[ true ]");
    UtamError e = expectThrows(UtamError.class, () -> getContext("wrongBasicTypeArray"));
    assertThat(e.getCause().getMessage(), containsString(expectedError));
  }

  @Test
  public void testElementTypeAsStringWithInvalidValueThrows() {
    String expectedError = String.format(ERR_UNSUPPORTED_ELEMENT_TYPE, ELEMENT_NAME, "\"wrong\"");
    UtamError e = expectThrows(UtamError.class, () -> getContext("wrongBasicType"));
    assertThat(e.getCause().getMessage(), containsString(expectedError));
  }

  @Test
  public void testElementNodeWithInvalidArrayElementThrows() {
    String expectedError = String.format(ERR_UNSUPPORTED_ELEMENT_TYPE, ELEMENT_NAME, "\"wrong\"");
    UtamError e = expectThrows(UtamError.class, () -> getContext("wrongBasicTypeArrayElement"));
    assertThat(e.getCause().getMessage(), containsString(expectedError));
  }
}
