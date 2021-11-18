/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.nullValue;
import static org.testng.Assert.expectThrows;
import static utam.compiler.grammar.TestUtilities.JACKSON_MISSING_REQUIRED_PROPERTY_ERROR;
import static utam.compiler.grammar.TestUtilities.UtamEntityCreator;
import static utam.compiler.grammar.TestUtilities.getDeserializedObject;
import static utam.compiler.grammar.TestUtilities.getTestTranslationContext;
import static utam.compiler.grammar.UtamElement.ERR_ELEMENT_FILTER_NEEDS_LIST;
import static utam.compiler.grammar.UtamElement.ERR_ELEMENT_MISSING_SELECTOR_PROPERTY;
import static utam.compiler.grammar.UtamElement.ERR_ELEMENT_NESTED_ELEMENTS;
import static utam.compiler.grammar.UtamElement.Type;
import static utam.compiler.grammar.UtamElementFilter_Tests.getInnerTextFilter;
import static utam.compiler.grammar.UtamSelectorTests.getListCssSelector;
import static utam.compiler.grammar.UtamSelectorTests.getUtamCssSelector;
import static utam.compiler.helpers.TypeUtilities.BASIC_ELEMENT;
import static utam.compiler.types.BasicElementInterface.ERR_UNSUPPORTED_ELEMENT_TYPE;

import java.util.List;
import org.testng.annotations.Test;
import utam.compiler.helpers.ElementContext;
import utam.compiler.helpers.TranslationContext;
import utam.compiler.representation.PageObjectValidationTestHelper.FieldInfo;
import utam.core.declarative.representation.PageClassField;
import utam.core.declarative.representation.PageObjectDeclaration;
import utam.core.declarative.representation.PageObjectMethod;
import utam.core.framework.consumer.UtamError;

public class UtamElement_BasicTests {

  private static final String ELEMENT_NAME = "test";
  private static final String BASIC_TYPE_SIMPLE_NAME = BASIC_ELEMENT.getSimpleName();

  private static UtamElement.Traversal getAbstraction(UtamElement element) {
    UtamElement.Traversal res = element.getAbstraction();
    assertThat(res.getClass(), is(equalTo(UtamElement.Basic.class)));
    return res;
  }

  private static PageObjectMethod getElementMethod(UtamElement element) {
    TranslationContext context = getTestTranslationContext();
    UtamElement.Traversal abstraction = getAbstraction(element);
    return abstraction.testRootTraverse(context).getElementMethod();
  }

  private static UtamElement getPublicHtmlElement(UtamSelector selector, UtamElementFilter filter) {
    UtamElement utamElement = UtamEntityCreator.createUtamElement(ELEMENT_NAME);
    utamElement.selector = selector;
    utamElement.filter = filter;
    utamElement.isPublic = true;
    return utamElement;
  }

  private static String getBasicSupportedProperties() {
    return Type.BASIC.getSupportedPropertiesErr(ELEMENT_NAME);
  }

  private static void getContext(String fileName) {
    new DeserializerUtilities().getContext("element/" + fileName);
  }

  /**
   * The validateSimpleElement method with a filter should succeed
   */
  @Test
  public void testValidateSimpleElementWithFilter() {
    getAbstraction(getPublicHtmlElement(getListCssSelector(), getInnerTextFilter()));
  }

  /**
   * The validateSimpleElement method with a filter and a null selector should throw the appropriate
   * exception
   */
  @Test
  public void testValidateSimpleElementWithFilterAndNullSelectorThrows() {
    UtamError e =
        expectThrows(UtamError.class, () -> getAbstraction(getPublicHtmlElement(null, null)));
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
    UtamError e =
        expectThrows(
            UtamError.class,
            () -> getAbstraction(getPublicHtmlElement(getUtamCssSelector(), getInnerTextFilter())));
    assertThat(
        e.getMessage(), containsString(String.format(ERR_ELEMENT_FILTER_NEEDS_LIST, ELEMENT_NAME)));
  }

  /**
   * The validateSimpleElement method with an element should succeed
   */
  @Test
  public void testValidateSimpleElementWithElement() {
    getAbstraction(getPublicHtmlElement(getUtamCssSelector(), null));
  }

  @Test
  public void testValidateRedundantProperties() {
    UtamElement externalElement = getPublicHtmlElement(getUtamCssSelector(), null);
    externalElement.isExternal = true;
    UtamError e = expectThrows(UtamError.class, () -> getAbstraction(externalElement));
    assertThat(e.getMessage(), is(equalTo(getBasicSupportedProperties())));
  }

  /**
   * The getAsSimpleElement method should return the proper value
   */
  @Test
  public void testGetAsSimpleElement() {
    TranslationContext context = getTestTranslationContext();
    ElementContext scopeElement = new ElementContext.Basic(ELEMENT_NAME);
    UtamElement.Traversal element =
        getAbstraction(getPublicHtmlElement(getUtamCssSelector(), null));
    ElementContext elementContext = element.traverse(context, scopeElement, false)[0];
    assertThat(elementContext.getType().getSimpleName(), is(equalTo(BASIC_TYPE_SIMPLE_NAME)));
    assertThat(context.getFields(), hasSize(1));
    FieldInfo createdFieldInfo = new FieldInfo(ELEMENT_NAME);
    createdFieldInfo.addAnnotations(String.format(
        "@ElementMarker.Find(css = \"%s\", scope = \"%s\")",
        UtamSelectorTests.SELECTOR_STRING, ELEMENT_NAME));
    createdFieldInfo.validateField(context.getFields().get(0));
  }

  /**
   * The getAsSimpleElement method should return the proper value for an element with a root scope
   * element
   */
  @Test
  public void testGetAsSimpleElementWithRootScope() {
    TranslationContext context = getTestTranslationContext();
    UtamElement.Traversal element =
        getAbstraction(getPublicHtmlElement(getUtamCssSelector(), null));
    ElementContext elementContext = element.testRootTraverse(context);
    assertThat(elementContext.getType().getSimpleName(), is(equalTo(BASIC_TYPE_SIMPLE_NAME)));
    assertThat(context.getFields(), hasSize(1));
    FieldInfo createdFieldInfo = new FieldInfo(ELEMENT_NAME);
    createdFieldInfo.addAnnotations(
        String.format("@ElementMarker.Find(css = \"%s\")", UtamSelectorTests.SELECTOR_STRING));
    createdFieldInfo.validateField(context.getFields().get(0));
  }

  /**
   * The getAsSimpleElement method should return the proper value for an element with a filter
   */
  @Test
  public void testGetAsSimpleElementWithFilter() {
    TranslationContext context = getTestTranslationContext();
    UtamElement.Traversal element =
        getAbstraction(getPublicHtmlElement(getListCssSelector(), getInnerTextFilter()));
    ElementContext elementContext = element.testRootTraverse(context);
    assertThat(elementContext.getType().getSimpleName(), is(equalTo(BASIC_TYPE_SIMPLE_NAME)));
    assertThat(context.getFields(), hasSize(1));
    FieldInfo createdFieldInfo = new FieldInfo(ELEMENT_NAME);
    createdFieldInfo.addAnnotations(
        String.format("@ElementMarker.Find(css = \"%s\")", UtamSelectorTests.SELECTOR_STRING));
    createdFieldInfo.validateField(context.getFields().get(0));
  }

  /**
   * The getAsSimpleElement method should return the proper value for a list selector
   */
  @Test
  public void testGetAsSimpleElementWithListSelector() {
    TranslationContext context = getTestTranslationContext();
    UtamElement.Traversal element =
        getAbstraction(getPublicHtmlElement(getUtamCssSelector(), null));
    element.testRootTraverse(context);
    assertThat(context.getFields(), hasSize(1));
    FieldInfo createdFieldInfo = new FieldInfo(ELEMENT_NAME);
    createdFieldInfo.addAnnotations(
        String.format("@ElementMarker.Find(css = \"%s\")", UtamSelectorTests.SELECTOR_STRING));
    createdFieldInfo.validateField(context.getFields().get(0));
  }

  /**
   * The getAsSimpleElement method should return the proper value for a list selector with a filter
   */
  @Test
  public void testGetAsSimpleElementWithListSelectorAndFilter() {
    TranslationContext context = getTestTranslationContext();
    UtamElement.Traversal element =
        getAbstraction(getPublicHtmlElement(getListCssSelector(), getInnerTextFilter()));
    element.testRootTraverse(context);
    assertThat(context.getFields(), hasSize(1));
    FieldInfo createdFieldInfo = new FieldInfo(ELEMENT_NAME);
    createdFieldInfo.addAnnotations(
        String.format("@ElementMarker.Find(css = \"%s\")", UtamSelectorTests.SELECTOR_STRING));
    createdFieldInfo.validateField(context.getFields().get(0));
  }

  /**
   * The getNextScope method should return the proper value for an element
   */
  @Test
  public void testGetNextScopeWithElement() {
    TranslationContext context = getTestTranslationContext();
    ElementContext scopeElement = new ElementContext.Basic("scope");
    assertThat(
        getAbstraction(getPublicHtmlElement(getUtamCssSelector(), null))
            .traverse(context, scopeElement, false)[0]
            .getType()
            .getSimpleName(),
        is(equalTo(BASIC_TYPE_SIMPLE_NAME)));
  }

  /**
   * The getDeclaredMethods method should return the proper value for an element not marked public
   */
  @Test
  public void testGetDeclaredMethodsWithComponentPrivateElement() {
    UtamElement element = UtamEntityCreator.createUtamElement(ELEMENT_NAME);
    element.selector = getUtamCssSelector();
    assertThat(getElementMethod(element), is(not(nullValue())));
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
  public void testElementNodeWithNestedElements() {
    DeserializerUtilities.Result res = new DeserializerUtilities()
        .getResultFromFile("element/nestedElements");
    TranslationContext context = res.getContext();
    ElementContext one = context.getElement("one");
    assertThat(one.getType().getSimpleName(), is(equalTo(BASIC_TYPE_SIMPLE_NAME)));
    ElementContext nested = context.getElement("nestedCustom");
    assertThat(nested.getType().getSimpleName(), is(equalTo("ComponentType")));
    ElementContext nestedInShadow = context.getElement("nestedInsideShadow");
    assertThat(nestedInShadow.getType().getSimpleName(), is(equalTo(BASIC_TYPE_SIMPLE_NAME)));

    PageObjectDeclaration objectDeclaration = res.getPageObject();
    List<PageClassField> fields = objectDeclaration.getImplementation().getFields();
    assertThat(fields.size(), is(equalTo(2)));
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
