/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.equalTo;
import static org.testng.Assert.expectThrows;
import static utam.compiler.grammar.UtamElement.ERR_ELEMENT_EXTERNAL_NOT_ALLOWED;
import static utam.compiler.grammar.UtamElement.ERR_ELEMENT_FILTER_NEEDS_LIST;
import static utam.compiler.grammar.UtamElement.ERR_ELEMENT_MISSING_SELECTOR_PROPERTY;
import static utam.compiler.grammar.UtamElement.Type;
import static utam.compiler.grammar.UtamElementFilter_Tests.getInnerTextFilter;
import static utam.compiler.grammar.UtamSelectorTests.getUtamCssSelector;

import java.util.List;
import org.testng.annotations.Test;
import utam.compiler.helpers.ElementContext;
import utam.compiler.helpers.TranslationContext;
import utam.compiler.helpers.TypeUtilities.FromString;
import utam.compiler.representation.PageObjectValidationTestHelper;
import utam.compiler.representation.PageObjectValidationTestHelper.MethodInfo;
import utam.compiler.representation.PageObjectValidationTestHelper.MethodParameterInfo;
import utam.core.declarative.representation.PageObjectMethod;
import utam.core.declarative.representation.TypeProvider;
import utam.core.framework.consumer.UtamError;
import utam.core.selenium.element.LocatorBy;

public class UtamElement_CustomTests {

  private static final String ELEMENT_NAME = "test";
  private static final String METHOD_NAME = "getTest";
  private static final TypeProvider EXPECTED_TYPE = new FromString(
      "utam.test.pageobjects.Component");
  private static final String COMPONENT_TYPE_URI = "utam-test/pageObjects/test/componentName";
  private static final String IMPORTED_LOCATOR_TYPE = LocatorBy.class.getName();
  private static final String IMPORTED_LIST_TYPE = List.class.getName();

  private static PageObjectMethod getTestMethod(String fileName) {
    return new DeserializerUtilities().getContext("element/" + fileName).getMethod(METHOD_NAME);
  }

  private static UtamElement getPublicComponentElement(UtamSelector selector) {
    UtamElement utamElement = TestUtilities.UtamEntityCreator
        .createUtamElement(ELEMENT_NAME, COMPONENT_TYPE_URI, selector);
    utamElement.isPublic = true;
    return utamElement;
  }

  private static String getCustomSupportedProperties() {
    return Type.CUSTOM.getSupportedPropertiesErr(ELEMENT_NAME);
  }

  /**
   * The validateComponentElement method with a component and a null selector should throw the
   * appropriate exception
   */
  @Test
  public void testValidateComponentElementWithNullSelectorThrows() {
    UtamElement element = getPublicComponentElement(null);
    UtamError e = expectThrows(UtamError.class, element::getAbstraction);
    assertThat(
        e.getMessage(), is(String.format(ERR_ELEMENT_MISSING_SELECTOR_PROPERTY, ELEMENT_NAME)));
  }

  /**
   * The validateComponentElement method with a filter and a non-list selector should throw the
   * appropriate exception
   */
  @Test
  public void testValidateComponentWithFilterAndNonListSelectorThrows() {
    UtamElement element = getPublicComponentElement(getUtamCssSelector());
    element.filter = getInnerTextFilter();
    UtamError e = expectThrows(UtamError.class, element::getAbstraction);
    assertThat(
        e.getMessage(), containsString(String.format(ERR_ELEMENT_FILTER_NEEDS_LIST, ELEMENT_NAME)));
  }

  /**
   * The validateComponentElement method with a component and nested elements should throw the
   * appropriate exception
   */
  @Test
  public void testValidateComponentElementWithInnerElementsThrows() {
    UtamElement element = getPublicComponentElement(getUtamCssSelector());
    element.elements = new UtamElement[]{};
    UtamError e = expectThrows(UtamError.class, element::getAbstraction);
    assertThat(e.getMessage(), containsString(getCustomSupportedProperties()));
  }

  /**
   * The validateComponentElement method with a component and nested shadow elements should throw
   * the appropriate exception
   */
  @Test
  public void testValidateComponentElementWithInnerShadowElementsThrows() {
    UtamElement element = getPublicComponentElement(getUtamCssSelector());
    element.shadow = new UtamShadowElement(new UtamElement[]{});
    UtamError e = expectThrows(UtamError.class, element::getAbstraction);
    assertThat(e.getMessage(), containsString(getCustomSupportedProperties()));
  }

  @Test
  public void testGetterWithExternalComponent() {
    MethodInfo expectedMethod = new MethodInfo(METHOD_NAME, "Component");
    expectedMethod.addCodeLines(
        "return inScope(this.root, LocatorBy.byCss(\"css\"), false).build(Component.class)");
    PageObjectMethod method = getTestMethod("customExternal");
    PageObjectValidationTestHelper.validateMethod(method, expectedMethod);
  }

  @Test
  public void testElementWithListCantBeExternal() {
    UtamError e = expectThrows(UtamError.class, () -> getTestMethod("customExternalListErr"));
    assertThat(
        e.getMessage(),
        containsString(String.format(ERR_ELEMENT_EXTERNAL_NOT_ALLOWED, ELEMENT_NAME)));
  }

  @Test
  public void testNestedCustomListWithFilterFindFirst() {
    TranslationContext context = new DeserializerUtilities()
        .getContext("element/customNestedWithFilter");
    ElementContext element = context.getElement(ELEMENT_NAME);
    PageObjectMethod method = element.getElementMethod();
    assertThat(element.getType().getFullName(), is(equalTo(EXPECTED_TYPE.getFullName())));
    MethodInfo expected = new MethodInfo(METHOD_NAME, "Component");
    expected.addParameter(new MethodParameterInfo("arg1", "String"));
    expected.addParameter(new MethodParameterInfo("arg2", "String"));
    expected.addCodeLines(
        "return inScope(this.element.setParameters(arg1), LocatorBy.byCss(\"css\"), false, false)"
            + ".build(Component.class, elm -> (elm.getText()!= null && elm.getText().contains(arg2)))");
    PageObjectValidationTestHelper.validateMethod(method, expected);
  }

  @Test
  public void testCustomListWithSelectorArgsInsideShadow() {
    MethodInfo expected = new MethodInfo(METHOD_NAME, "List<Component>");
    expected.addParameter(new MethodParameterInfo("index", "Integer"));
    expected.addCodeLine(
        "return inScope(this.root, LocatorBy.byCss(String.format(\"css[%d]\", index)), true, true)"
            +
            ".buildList(Component.class)");
    PageObjectMethod method = getTestMethod("customListWithSelectorArg");
    PageObjectValidationTestHelper.validateMethod(method, expected);
  }

  @Test
  public void testCustomNullableListWithFilter() {
    MethodInfo expected = new MethodInfo(METHOD_NAME, "List<Component>");
    expected.addCodeLine(
        "return inScope(this.root, LocatorBy.byCss(\".css\"), true, false)" +
            ".buildList(Component.class, elm -> Boolean.TRUE.equals(elm.isVisible()))");
    PageObjectMethod method = getTestMethod("customWithFilter");
    expected.addImportedTypes(EXPECTED_TYPE.getFullName(), IMPORTED_LIST_TYPE);
    expected.addImpliedImportedTypes(EXPECTED_TYPE.getFullName(), IMPORTED_LOCATOR_TYPE,
        IMPORTED_LIST_TYPE);
    PageObjectValidationTestHelper.validateMethod(method, expected);
  }

  @Test
  public void testCustomPublicNullableSingleElement() {
    MethodInfo expected = new MethodInfo(METHOD_NAME, "Component");
    expected.addCodeLine("return inScope(this.root, LocatorBy.byCss(\"css\"), true, false)" +
        ".build(Component.class)");
    expected.addImportedTypes(EXPECTED_TYPE.getFullName());
    expected.addImpliedImportedTypes(EXPECTED_TYPE.getFullName());
    expected.addImpliedImportedTypes(IMPORTED_LOCATOR_TYPE);
    PageObjectMethod method = getTestMethod("customPublicNullable");
    PageObjectValidationTestHelper.validateMethod(method, expected);
  }

  @Test
  public void testDuplicateArgsNamesThrows() {
    UtamError e =
        expectThrows(UtamError.class, () -> getTestMethod("customDuplicateArgs"));
    assertThat(e.getMessage(), containsString("duplicate parameters"));
  }
}
