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
import static utam.compiler.helpers.PrimitiveType.NUMBER;
import static utam.compiler.helpers.PrimitiveType.STRING;
import static utam.compiler.helpers.TypeUtilities.BASIC_ELEMENT;

import org.testng.annotations.Test;
import utam.compiler.JsonBuilderTestUtility;
import utam.compiler.UtamCompilationError;
import utam.compiler.helpers.TranslationContext;
import utam.compiler.representation.PageObjectValidationTestHelper;
import utam.compiler.representation.PageObjectValidationTestHelper.MethodInfo;
import utam.compiler.representation.PageObjectValidationTestHelper.MethodParameterInfo;
import utam.core.declarative.representation.PageObjectMethod;
import utam.core.framework.consumer.UtamError;

/**
 * validation tests, generated code is tested under presentation package
 *
 * @since 238
 */
public class UtamElementBasicTests {

  private static void getContext(String fileName) {
    new DeserializerUtilities().getContext("validate/basic_element/" + fileName);
  }

  @Test
  public void testWithNullSelectorThrows() {
    JsonBuilderTestUtility test = new JsonBuilderTestUtility();
    test.addRawString("elements", "[ {\"name\": \"test\" }]");
    UtamError e = expectThrows(UtamCompilationError.class, test::getDeserializedJson);
    assertThat(
        e.getMessage(),
        containsString("error 9: element \"test\": property \"selector\" is required"));
  }

  /**
   * The validateSimpleElement method with a filter and a non-list selector should throw the
   * appropriate exception
   */
  @Test
  public void testBasicElementWithFilterAndNonListSelectorThrows() {
    UtamError e = expectThrows(UtamCompilationError.class, () -> getContext("filterForNonList"));
    assertThat(
        e.getMessage(),
        containsString("error 302: element \"test\" filter: filter can only be set for list"));
  }

  @Test
  public void testEmptyNestedElementsThrows() {
    UtamError e = expectThrows(UtamError.class, () -> getContext("emptyNestedElementsArray"));
    assertThat(
        e.getMessage(),
        containsString(
            "error 12: element \"test\" elements: property \"elements\" should be a non-empty"
                + " array"));
  }

  @Test
  public void testElementNodeWithInvalidArrayElementTypeThrows() {
    UtamError e = expectThrows(UtamError.class, () -> getContext("wrongBasicTypeArray"));
    assertThat(
        e.getMessage(),
        containsString(
            "error 115: element \"test\" type: basic type \"true\" is not supported, "
                + "valid values are: actionable, clickable, draggable, editable, touchable"));
  }

  @Test
  public void testElementTypeAsStringWithInvalidValueThrows() {
    UtamError e = expectThrows(UtamCompilationError.class, () -> getContext("wrongBasicType"));
    assertThat(
        e.getMessage(),
        containsString(
            "error 201: element \"test\": type \"wrong\" is not supported, valid values are:"
                + " custom, container, frame, actionable, clickable, draggable, editable,"
                + " touchable"));
  }

  @Test
  public void testElementNodeWithInvalidArrayElementThrows() {
    UtamError e = expectThrows(UtamError.class, () -> getContext("wrongBasicTypeArrayElement"));
    assertThat(
        e.getMessage(),
        containsString(
            "error 115: element \"test\" type: basic type \"wrong\" is not supported, "
                + "valid values are: actionable, clickable, draggable, editable, touchable"));
  }

  @Test
  public void testElementNodeWithDuplicateTypeThrows() {
    UtamError e = expectThrows(UtamError.class, () -> getContext("duplicateBasicType"));
    assertThat(
        e.getMessage(),
        containsString(
            "error 116: element \"test\" type: duplicate basic type \"clickable\", basic type"
                + " values must be unique"));
  }

  @Test
  public void testNavigationElementNameNotAllowed() {
    UtamError e = expectThrows(UtamCompilationError.class, () -> getContext("navigationName"));
    assertThat(
        e.getMessage(),
        containsString(
            "error 202: element \"navigation\": element with same name was already declared"));
  }

  @Test
  public void testGeneratedCodeOfIndexedGetter() {
    TranslationContext context =
        new DeserializerUtilities().getContext("nestedlist/nestedInsideList");
    final String ELEMENT_METHOD_NAME = "_index_getBasicListElement";
    PageObjectMethod nestedGetter = context.getMethod(ELEMENT_METHOD_NAME);
    MethodInfo expected = new MethodInfo(ELEMENT_METHOD_NAME, BASIC_ELEMENT.getSimpleName());
    expected.setNotPublic();
    expected.addParameter(new MethodParameterInfo("_basicListIndex", NUMBER.getSimpleName()));
    expected.addCodeLine("List<BasicElement> basicList = this.getBasicListElement()");
    expected.addCodeLine("if (basicList.size() < _basicListIndex-1) {");
    expected.addCodeLine(
        "throw new RuntimeException(\"Can't find scope element 'basicList' with given index!\")");
    expected.addCodeLine("}");
    expected.addCodeLine("return basicList.get(_basicListIndex)");
    PageObjectValidationTestHelper.validateMethod(nestedGetter, expected);
  }

  @Test
  public void testElementNestedInsideList() {
    TranslationContext context =
        new DeserializerUtilities().getContext("nestedlist/nestedInsideList");
    final String ELEMENT_METHOD_NAME = "getNestedBasic";
    PageObjectMethod nestedGetter = context.getMethod(ELEMENT_METHOD_NAME);
    MethodInfo expected = new MethodInfo(ELEMENT_METHOD_NAME, BASIC_ELEMENT.getSimpleName());
    expected.addParameter(new MethodParameterInfo("_basicListIndex", NUMBER.getSimpleName()));
    expected.addCodeLine(
        "BasicElement basicList = this._index_getBasicListElement(_basicListIndex)");
    expected.addCodeLine(
        "return basic(basicList, this.nestedBasic).build(BasicElement.class,"
            + " BasePageElement.class)");
    PageObjectValidationTestHelper.validateMethod(nestedGetter, expected);
  }

  @Test
  public void testElementNestedInsideListUsedInMethod() {
    TranslationContext context =
        new DeserializerUtilities().getContext("nestedlist/nestedInsideList");
    final String ELEMENT_METHOD_NAME = "testNestedBasic";
    PageObjectMethod nestedGetter = context.getMethod(ELEMENT_METHOD_NAME);
    MethodInfo expected = new MethodInfo(ELEMENT_METHOD_NAME, STRING.getSimpleName());
    expected.addParameter(new MethodParameterInfo("_basicListIndex", NUMBER.getSimpleName()));
    expected.addCodeLine("BasicElement nestedBasic0 = this.getNestedBasic(_basicListIndex)");
    expected.addCodeLine("String statement0 = nestedBasic0.getText()");
    expected.addCodeLine("return statement0");
    PageObjectValidationTestHelper.validateMethod(nestedGetter, expected);
  }

  @Test
  public void testElementDoubleNestedInsideLists() {
    TranslationContext context =
        new DeserializerUtilities().getContext("nestedlist/nestedInsideList");
    final String ELEMENT_METHOD_NAME = "getDoubleNestedBasic";
    PageObjectMethod nestedGetter = context.getMethod(ELEMENT_METHOD_NAME);
    MethodInfo expected = new MethodInfo(ELEMENT_METHOD_NAME, BASIC_ELEMENT.getSimpleName());
    expected.addParameter(new MethodParameterInfo("_basicListIndex", NUMBER.getSimpleName()));
    expected.addParameter(new MethodParameterInfo("_nestedBasicListIndex", NUMBER.getSimpleName()));
    expected.addCodeLine(
        "BasicElement nestedBasicList = this._index_getNestedBasicListElement(_basicListIndex,"
            + " _nestedBasicListIndex)");
    expected.addCodeLine(
        "return basic(nestedBasicList, this.doubleNestedBasic).build(BasicElement.class,"
            + " BasePageElement.class)");
    PageObjectValidationTestHelper.validateMethod(nestedGetter, expected);
  }
}
