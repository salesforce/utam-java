/*
 * Copyright (c) 2024, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static org.testng.Assert.expectThrows;
import static utam.compiler.grammar.TestUtilities.findField;
import static utam.compiler.helpers.PrimitiveType.NUMBER;
import static utam.compiler.helpers.TypeUtilities.BASIC_ELEMENT;
import static utam.compiler.helpers.TypeUtilities.BASIC_ELEMENT_IMPL_CLASS;
import static utam.compiler.helpers.TypeUtilities.LIST_TYPE;
import static utam.compiler.helpers.TypeUtilities.PAGE_OBJECT;
import static utam.compiler.helpers.TypeUtilities.SELECTOR;
import static utam.compiler.representation.PageObjectValidationTestHelper.validateMethod;

import java.util.List;
import java.util.stream.Collectors;
import org.testng.annotations.Test;
import utam.compiler.UtamCompilationError;
import utam.compiler.helpers.ElementContext;
import utam.compiler.helpers.TranslationContext;
import utam.compiler.helpers.TypeUtilities.FromString;
import utam.compiler.representation.PageObjectValidationTestHelper;
import utam.compiler.representation.PageObjectValidationTestHelper.FieldInfo;
import utam.compiler.representation.PageObjectValidationTestHelper.MethodInfo;
import utam.compiler.representation.PageObjectValidationTestHelper.MethodParameterInfo;
import utam.core.declarative.representation.PageClassField;
import utam.core.declarative.representation.PageObjectMethod;
import utam.core.declarative.representation.TypeProvider;

/**
 * Test class dedicated to testing custom element
 *
 * @author elizaveta.ivanova
 * @since 252
 */
public class UtamElementCustomTests {

  private static final String METHOD_NAME = "getTest";
  private static final TypeProvider EXPECTED_TYPE =
      new FromString("utam.test.pageobjects.Component");

  private static final String VALIDATION_DATA_ROOT = "validate/custom_element/";

  private static TranslationContext compileNestedElements() {
    return new DeserializerUtilities().getContext("custom/nestedElements");
  }

  @Test
  public void testDuplicateArgsNamesThrows() {
    UtamCompilationError e =
        expectThrows(
            UtamCompilationError.class,
            () ->
                new DeserializerUtilities()
                    .getContext(VALIDATION_DATA_ROOT + "customDuplicateArgs")
                    .getMethod(METHOD_NAME));
    assertThat(
        e.getMessage(),
        containsString(
            "error 107: method 'getTestElement' arguments: argument with name \"arg\" is already"
                + " declared"));
  }

  @Test
  public void testNestedElementsNamesCollision() {
    UtamCompilationError e =
        expectThrows(
            UtamCompilationError.class,
            () ->
                new DeserializerUtilities()
                    .getContext(VALIDATION_DATA_ROOT + "nestedElementNameCollision"));
    assertThat(
        e.getMessage(),
        containsString(
            "error 504: method \"getCustomRootScopeElement\": method with the same name was already"
                + " declared"));
  }

  @Test
  public void testCustomListWithSelectorArgsInsideShadow() {
    MethodInfo expected = new MethodInfo(METHOD_NAME, "List<Component>");
    expected.addParameter(new MethodParameterInfo("index", "Integer"));
    expected.addCodeLine("BasicElement root = this.getRootElement()");
    expected.addCodeLine(
        "return custom(root, this.test.setParameters(index)).buildList(Component.class)");
    PageObjectMethod method =
        new DeserializerUtilities()
            .getContext("custom/customListWithSelectorArg")
            .getMethod(METHOD_NAME);
    expected.addImportedTypes(EXPECTED_TYPE.getFullName(), LIST_TYPE.getFullName());
    expected.addImpliedImportedTypes(
        EXPECTED_TYPE.getFullName(), BASIC_ELEMENT.getFullName(), LIST_TYPE.getFullName());
    PageObjectValidationTestHelper.validateMethod(method, expected);
  }

  @Test
  public void testCustomPublicNullableSingleElement() {
    MethodInfo expected = new MethodInfo(METHOD_NAME, "Component");
    expected.addCodeLine("BasicElement root = this.getRootElement()");
    expected.addCodeLine("return custom(root, this.test).build(Component.class)");
    PageObjectMethod method =
        new DeserializerUtilities()
            .getContext("custom/customPublicNullable")
            .getMethod(METHOD_NAME);
    PageObjectValidationTestHelper.validateMethod(method, expected);
  }

  @Test
  public void testGeneratedScopeGetterForCustomElement() {
    TranslationContext context = compileNestedElements();

    // check that field for custom scope was NOT added
    assertThat(context.getFields(), hasSize(3));
    final String SCOPE_FIELD_NAME = "customRoot";
    FieldInfo createdFieldInfo = new FieldInfo(SCOPE_FIELD_NAME);
    createdFieldInfo.addAnnotations("@ElementMarker.Find(css = \".custom\", expand = true)");
    createdFieldInfo.validateField(findField(context, SCOPE_FIELD_NAME));

    // check generated method
    final String GETTER_NAME = "getCustomRootScopeElement";
    PageObjectMethod actualGetter = context.getMethod(GETTER_NAME);
    MethodInfo expectedGetter = new MethodInfo(GETTER_NAME, "BasicElement");
    expectedGetter.setNotPublic();
    expectedGetter.addCodeLine("BasicElement root = this.getRootElement()");
    expectedGetter.addCodeLine(
        "return basic(root, this.customRoot).build(BasicElement.class, BasePageElement.class)");
    expectedGetter.addImportedTypes(BASIC_ELEMENT.getFullName());
    expectedGetter.addImpliedImportedTypes(
        BASIC_ELEMENT_IMPL_CLASS.getFullName(), BASIC_ELEMENT.getFullName());
    validateMethod(actualGetter, expectedGetter);
  }

  @Test
  public void testNestedBasicElement() {
    TranslationContext context = compileNestedElements();
    ElementContext nestedBasic = context.getElement("nestedBasic");
    assertThat(nestedBasic.getType().getFullName(), is(BASIC_ELEMENT.getFullName()));
    MethodInfo expectedGetter = new MethodInfo("getNestedBasic", "BasicElement");
    expectedGetter.addCodeLine("BasicElement customRootScope = this.getCustomRootScopeElement()");
    expectedGetter.addCodeLine(
        "return basic(customRootScope, this.nestedBasic).build(BasicElement.class,"
            + " BasePageElement.class)");
    PageObjectMethod actualMethod = nestedBasic.getElementMethod();
    validateMethod(actualMethod, expectedGetter);
  }

  @Test
  public void testNestedCustomElement() {
    TranslationContext context = compileNestedElements();
    ElementContext nestedCustomList = context.getElement("nestedCustom");
    assertThat(nestedCustomList.getType().getSimpleName(), is(equalTo("Component")));
    MethodInfo expectedGetter = new MethodInfo("getNestedCustom", "List<Component>");
    expectedGetter.addCodeLine("BasicElement customRootScope = this.getCustomRootScopeElement()");
    expectedGetter.addCodeLine(
        "return custom(customRootScope, this.nestedCustom).buildList(Component.class)");
    validateMethod(nestedCustomList.getElementMethod(), expectedGetter);
  }

  @Test
  public void testNestedContainerElement() {
    TranslationContext context = compileNestedElements();
    ElementContext nestedCustomList = context.getElement("nestedContainer");
    assertThat(nestedCustomList.getType().getSimpleName(), is(equalTo("ContainerElement")));
    MethodInfo expectedGetter = new MethodInfo("getNestedContainer", "T");
    expectedGetter.addParameter(new MethodParameterInfo("pageObjectType", "Class<T>"));
    expectedGetter.addCodeLine("BasicElement customRootScope = this.getCustomRootScopeElement()");
    expectedGetter.addCodeLine(
        "LocatorBy nestedContainerLocator = LocatorBy.byCss(\":scope > *:first-child\")");
    expectedGetter.addCodeLine(
        "return this.container(customRootScope, true).load(pageObjectType,"
            + " nestedContainerLocator)");
    expectedGetter.addImportedTypes(PAGE_OBJECT.getFullName());
    expectedGetter.addImpliedImportedTypes(
        PAGE_OBJECT.getFullName(), SELECTOR.getFullName(), BASIC_ELEMENT.getFullName());
    validateMethod(nestedCustomList.getElementMethod(), expectedGetter);
  }

  @Test
  public void testGeneratedScopeGetterForCustomElementWithFilter() {
    TranslationContext context =
        new DeserializerUtilities().getContext("custom/nestedElementsFromFilter");
    final String NESTED_ELEMENT_NAME = "nestedBasicInsideFilter";

    // check that field for custom scope was NOT added
    List<String> declaredFields =
        context.getFields().stream().map(PageClassField::getName).collect(Collectors.toList());
    assertThat(declaredFields, hasSize(2));
    assertThat(declaredFields, containsInAnyOrder("customRootWithFilter", NESTED_ELEMENT_NAME));

    // check generated getter for scope
    MethodInfo expectedGetter =
        new MethodInfo("getCustomRootWithFilterScopeElement", "BasicElement");
    expectedGetter.setNotPublic();
    expectedGetter.addParameter(new MethodParameterInfo("itemText"));
    expectedGetter.addCodeLine("BasicElement root = this.getRootElement()");
    expectedGetter.addCodeLine(
        "return basic(root, this.customRootWithFilter).build(BasicElement.class,"
            + " BasePageElement.class, elm -> (elm.getText()!= null &&"
            + " elm.getText().contains(itemText)))");
    PageObjectMethod actualMethod = context.getMethod("getCustomRootWithFilterScopeElement");
    validateMethod(actualMethod, expectedGetter);

    // check generated getter for nested element
    expectedGetter = new MethodInfo("getNestedBasicInsideFilter", "BasicElement");
    expectedGetter.addParameter(new MethodParameterInfo("itemText"));
    expectedGetter.addCodeLine(
        "BasicElement customRootWithFilterScope ="
            + " this.getCustomRootWithFilterScopeElement(itemText)");
    expectedGetter.addCodeLine(
        "return basic(customRootWithFilterScope,"
            + " this.nestedBasicInsideFilter).build(BasicElement.class, BasePageElement.class)");
    actualMethod = context.getElement(NESTED_ELEMENT_NAME).getElementMethod();
    validateMethod(actualMethod, expectedGetter);
  }

  @Test
  public void testElementNestedInsideList() {
    TranslationContext context =
        new DeserializerUtilities().getContext("nestedlist/nestedCustomList");
    final String ELEMENT_METHOD_NAME = "getNestedBasic";
    PageObjectMethod nestedGetter = context.getMethod(ELEMENT_METHOD_NAME);
    MethodInfo expected = new MethodInfo(ELEMENT_METHOD_NAME, BASIC_ELEMENT.getSimpleName());
    expected.addParameter(new MethodParameterInfo("_customScopeIndex", NUMBER.getSimpleName()));
    expected.addCodeLine(
        "BasicElement customScope = this._index_getCustomScopeElement(_customScopeIndex)");
    expected.addCodeLine(
        "return basic(customScope, this.nestedBasic).build(BasicElement.class,"
            + " BasePageElement.class)");
    PageObjectValidationTestHelper.validateMethod(nestedGetter, expected);
  }
}
