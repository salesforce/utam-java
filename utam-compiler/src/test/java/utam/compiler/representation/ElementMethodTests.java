/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.representation;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.empty;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static utam.compiler.grammar.TestUtilities.findField;
import static utam.compiler.helpers.TypeUtilities.BASIC_ELEMENT;
import static utam.compiler.helpers.TypeUtilities.BASIC_ELEMENT_IMPL_CLASS;
import static utam.compiler.representation.ElementMethod.DOCUMENT_GETTER;
import static utam.compiler.representation.ElementMethod.NAVIGATION_GETTER;

import java.util.List;
import java.util.Objects;
import org.hamcrest.CoreMatchers;
import org.testng.annotations.Test;
import utam.compiler.grammar.DeserializerUtilities;
import utam.compiler.grammar.DeserializerUtilities.Result;
import utam.compiler.helpers.ElementContext;
import utam.compiler.helpers.TranslationContext;
import utam.compiler.representation.PageObjectValidationTestHelper.FieldInfo;
import utam.compiler.representation.PageObjectValidationTestHelper.MethodInfo;
import utam.compiler.representation.PageObjectValidationTestHelper.MethodParameterInfo;
import utam.core.declarative.representation.PageClassField;
import utam.core.declarative.representation.PageObjectDeclaration;
import utam.core.declarative.representation.PageObjectMethod;
import utam.core.declarative.representation.UnionType;

/**
 * Elements getters
 *
 * @author james.evans
 */
public class ElementMethodTests {

  private static final String ELEMENT_NAME = "test";
  private static final String ELEMENT_METHOD_NAME = "getTest";
  private static final String ELEMENT_METHOD_NAME_PRIVATE = "getTestElement";
  private static final String BASIC_ELEMENT_TYPE_IMPL = BASIC_ELEMENT_IMPL_CLASS.getFullName();
  private static final String BASIC_ELEMENT_TYPE = BASIC_ELEMENT.getFullName();
  private static final String LIST_TYPE = List.class.getName();

  private static BasicElementGetterMethod getElementMethod(TranslationContext context) {
    return (BasicElementGetterMethod)
        Objects.requireNonNull(context.getElement(ELEMENT_NAME)).getElementMethod();
  }

  @Test
  public void testDocumentGetter() {
    PageObjectMethod method = DOCUMENT_GETTER;
    assertThat(method.isPublic(), is(false));
    assertThat(method.getClassImports(), is(empty()));
    assertThat(method.getCodeLines().get(0), is(equalTo("this.getDocument()")));
  }

  @Test
  public void testNavigationGetter() {
    PageObjectMethod method = NAVIGATION_GETTER;
    assertThat(method.isPublic(), is(false));
    assertThat(method.getClassImports(), is(empty()));
    assertThat(method.getCodeLines().get(0), is(equalTo("this.getNavigation()")));
  }

  @Test
  public void testBasicPrivateElementDefaultType() {
    MethodInfo expected = new MethodInfo(ELEMENT_METHOD_NAME_PRIVATE, "BasicElement");
    Result result = new DeserializerUtilities().getResultFromFile("element/basicElement");
    TranslationContext context = result.getContext();
    BasicElementGetterMethod method = getElementMethod(context);
    assertThat(method.getClassUnionType(), is(nullValue()));
    assertThat(method.getInterfaceUnionType(), is(nullValue()));
    expected.addCodeLine("BasicElement root = this.getRootElement()");
    expected.addCodeLine(
        "return basic(root, this.test).build(BasicElement.class, BasePageElement.class)");
    expected.addImportedTypes(BASIC_ELEMENT_TYPE);
    expected.addImpliedImportedTypes(BASIC_ELEMENT_TYPE_IMPL, BASIC_ELEMENT_TYPE);
    expected.setNotPublic();
    PageObjectValidationTestHelper.validateMethod(method, expected);

    assertThat(context.getFields(), hasSize(1));
    FieldInfo createdFieldInfo = new FieldInfo(ELEMENT_NAME);
    createdFieldInfo.addAnnotations("@ElementMarker.Find(css = \".css\")");
    createdFieldInfo.validateField(findField(context, ELEMENT_NAME));
  }

  @Test
  public void testElementNodeWithNestedElements() {
    DeserializerUtilities.Result res =
        new DeserializerUtilities().getResultFromFile("element/nestedElements");
    TranslationContext context = res.getContext();
    ElementContext one = context.getElement("one");
    assertThat(one.getType().getFullName(), is(BASIC_ELEMENT_TYPE));
    ElementContext nested = context.getElement("nestedCustom");
    assertThat(
        nested.getType().getSimpleName(), CoreMatchers.is(CoreMatchers.equalTo("ComponentType")));
    ElementContext nestedInShadow = context.getElement("nestedInsideShadow");
    assertThat(nestedInShadow.getType().getFullName(), is(BASIC_ELEMENT_TYPE));

    PageObjectDeclaration objectDeclaration = res.getPageObject();
    List<PageClassField> fields = objectDeclaration.getImplementation().getFields();
    assertThat(fields, hasSize(3));
  }

  @Test
  public void testBasicPrivateElementUnionTypeInImplOnly() {
    MethodInfo expected = new MethodInfo(ELEMENT_METHOD_NAME_PRIVATE, "TestElement");
    Result result =
        new DeserializerUtilities().getResultFromFile("element/basicElementTypesImplOnly");
    TranslationContext context = result.getContext();
    BasicElementGetterMethod method = getElementMethod(context);
    expected.addCodeLine("BasicElement root = this.getRootElement()");
    expected.addCodeLine(
        "return basic(root, this.test).build(TestElement.class, TestElementImpl.class)");
    expected.setNotPublic();
    PageObjectValidationTestHelper.validateMethod(method, expected);
    List<UnionType> unionTypes = context.getClassUnionTypes();
    assertThat(unionTypes, hasSize(2));
    assertThat(
        unionTypes.get(0).getDeclarationCode().get(0),
        is(equalTo("interface TestElement extends Actionable {}")));
    assertThat(
        unionTypes.get(1).getDeclarationCode().get(0),
        is(
            equalTo(
                "public static class TestElementImpl extends BasePageElement implements TestElement"
                    + " {}")));

    assertThat(context.getFields(), hasSize(1));
    FieldInfo createdFieldInfo = new FieldInfo(ELEMENT_NAME);
    createdFieldInfo.addAnnotations("@ElementMarker.Find(css = \".css\")");
    createdFieldInfo.validateField(findField(context, ELEMENT_NAME));
  }

  @Test
  public void testBasicPublicElementUnionTypeInImplOnly() {
    MethodInfo expected = new MethodInfo(ELEMENT_METHOD_NAME, "GetTestElement");
    Result result =
        new DeserializerUtilities().getResultFromFile("element/publicElementTypesImplOnly");
    TranslationContext context = result.getContext();
    BasicElementGetterMethod method = getElementMethod(context);
    expected.addCodeLine("BasicElement root = this.getRootElement()");
    expected.addCodeLine(
        "return basic(root, this.test.setParameters(arg)).build(GetTestElement.class,"
            + " GetTestElementImpl.class)");
    expected.addParameter(new MethodParameterInfo("arg"));
    PageObjectValidationTestHelper.validateMethod(method, expected);
    List<UnionType> unionTypes = context.getClassUnionTypes();
    assertThat(unionTypes, hasSize(1));
    assertThat(
        unionTypes.get(0).getDeclarationCode().get(0),
        is(
            equalTo(
                "public static class GetTestElementImpl extends BasePageElement implements"
                    + " GetTestElement {}")));
  }

  @Test
  public void testListBasicPrivateElementDefaultType() {
    MethodInfo expected = new MethodInfo(ELEMENT_METHOD_NAME_PRIVATE, "List<BasicElement>");
    Result result = new DeserializerUtilities().getResultFromFile("element/basicListBasicElement");
    TranslationContext context = result.getContext();
    BasicElementGetterMethod method = getElementMethod(context);
    assertThat(method.getClassUnionType(), is(nullValue()));
    assertThat(method.getInterfaceUnionType(), is(nullValue()));
    expected.addCodeLine("BasicElement root = this.getRootElement()");
    expected.addCodeLine(
        "return basic(root, this.test).buildList(BasicElement.class, BasePageElement.class)");
    expected.addImportedTypes(LIST_TYPE, BASIC_ELEMENT_TYPE);
    expected.addImpliedImportedTypes(LIST_TYPE, BASIC_ELEMENT_TYPE_IMPL, BASIC_ELEMENT_TYPE);
    expected.setNotPublic();
    PageObjectValidationTestHelper.validateMethod(method, expected);
  }

  @Test
  public void testListBasicPublicElementDefaultType() {
    MethodInfo expected = new MethodInfo(ELEMENT_METHOD_NAME, "List<BasicElement>");
    Result result = new DeserializerUtilities().getResultFromFile("element/basicListPublicElement");
    TranslationContext context = result.getContext();
    BasicElementGetterMethod method = getElementMethod(context);
    assertThat(method.getClassUnionType(), is(nullValue()));
    assertThat(method.getInterfaceUnionType(), is(nullValue()));
    expected.addCodeLine("BasicElement root = this.getRootElement()");
    expected.addCodeLine(
        "return basic(root, this.test.setParameters(arg))"
            + ".buildList(BasicElement.class, BasePageElement.class)");
    expected.addImportedTypes(LIST_TYPE, BASIC_ELEMENT_TYPE);
    expected.addImpliedImportedTypes(LIST_TYPE, BASIC_ELEMENT_TYPE_IMPL, BASIC_ELEMENT_TYPE);
    expected.addParameter(new MethodParameterInfo("arg"));
    PageObjectValidationTestHelper.validateMethod(method, expected);
  }

  @Test
  public void testListBasicPrivateElementUnionType() {
    MethodInfo expected = new MethodInfo(ELEMENT_METHOD_NAME_PRIVATE, "List<TestElement>");
    Result result = new DeserializerUtilities().getResultFromFile("element/basicListPrivateUnion");
    TranslationContext context = result.getContext();
    BasicElementGetterMethod method = getElementMethod(context);
    expected.addCodeLine("BasicElement root = this.getRootElement()");
    expected.addCodeLine(
        "return basic(root, this.test).buildList(TestElement.class, TestElementImpl.class)");
    expected.addImportedTypes(LIST_TYPE);
    expected.addImpliedImportedTypes(LIST_TYPE, BASIC_ELEMENT_TYPE);
    expected.setNotPublic();
    PageObjectValidationTestHelper.validateMethod(method, expected);
    String unionClass = method.getClassUnionType().getDeclarationCode().get(0);
    assertThat(
        unionClass,
        is(
            equalTo(
                "public static class TestElementImpl extends BasePageElement implements TestElement"
                    + " {}")));
    String unionType = method.getInterfaceUnionType().getDeclarationCode().get(0);
    assertThat(unionType, is(equalTo("interface TestElement extends Actionable {}")));
  }

  @Test
  public void testListBasicPublicElementUnionType() {
    MethodInfo expected = new MethodInfo(ELEMENT_METHOD_NAME, "List<TestElement>");
    Result result = new DeserializerUtilities().getResultFromFile("element/basicListPublicUnion");
    TranslationContext context = result.getContext();
    BasicElementGetterMethod method = getElementMethod(context);
    expected.addImportedTypes(LIST_TYPE);
    expected.addImpliedImportedTypes(LIST_TYPE, BASIC_ELEMENT_TYPE);
    expected.addCodeLine("BasicElement root = this.getRootElement()");
    expected.addCodeLine(
        "return basic(root, this.test).buildList(TestElement.class, TestElementImpl.class)");
    PageObjectValidationTestHelper.validateMethod(method, expected);
    String unionClass = method.getClassUnionType().getDeclarationCode().get(0);
    assertThat(
        unionClass,
        is(
            equalTo(
                "public static class TestElementImpl extends BasePageElement implements TestElement"
                    + " {}")));
    String unionType = method.getInterfaceUnionType().getDeclarationCode().get(0);
    assertThat(unionType, is(equalTo("interface TestElement extends Editable, Clickable {}")));
  }

  @Test
  public void testNullableList() {
    MethodInfo expected = new MethodInfo(ELEMENT_METHOD_NAME, "List<BasicElement>");
    expected.addCodeLine("BasicElement parent = this.getParentElement()");
    expected.addCodeLine(
        "return basic(parent, this.test).buildList(BasicElement.class, BasePageElement.class)");
    Result result =
        new DeserializerUtilities().getResultFromFile("element/basicElementNullableList");
    TranslationContext context = result.getContext();
    assertThat(context.getFields(), hasSize(2));
    PageObjectValidationTestHelper.FieldInfo fieldInfo =
        new PageObjectValidationTestHelper.FieldInfo(ELEMENT_NAME);
    fieldInfo.addAnnotations("@ElementMarker.Find(css = \".css\", nullable = true)");
    fieldInfo.validateField(findField(context, ELEMENT_NAME));
    BasicElementGetterMethod method = getElementMethod(context);
    PageObjectValidationTestHelper.validateMethod(method, expected);
  }

  @Test
  public void testNullableSingle() {
    MethodInfo expected = new MethodInfo(ELEMENT_METHOD_NAME, "BasicElement");
    expected.addCodeLine("BasicElement root = this.getRootElement()");
    expected.addCodeLine(
        "return basic(root, this.test).build(BasicElement.class, BasePageElement.class)");
    Result result =
        new DeserializerUtilities().getResultFromFile("element/basicElementNullableSingle");
    TranslationContext context = result.getContext();
    BasicElementGetterMethod method = getElementMethod(context);
    PageObjectValidationTestHelper.validateMethod(method, expected);
  }

  @Test
  public void testFilterByGetAttribute() {
    TranslationContext context =
        new DeserializerUtilities().getContext("filter/basicFilterGetAttribute");
    PageObjectMethod method = context.getMethod(ELEMENT_METHOD_NAME);
    MethodInfo expected = new MethodInfo(ELEMENT_METHOD_NAME, "List<BasicElement>");
    expected.addParameter(new MethodParameterInfo("scopeArg"));
    expected.addParameter(new MethodParameterInfo("selectorArg"));
    expected.addParameter(new MethodParameterInfo("applyArg"));
    expected.addParameter(new MethodParameterInfo("matcherArg"));
    expected.addCodeLine("BasicElement scope = this.getScopeElement(scopeArg)");
    expected.addCodeLines(
        "return basic(scope, this.test.setParameters(selectorArg)).buildList(BasicElement.class,"
            + " BasePageElement.class, elm -> (elm.getAttribute(applyArg)!= null &&"
            + " elm.getAttribute(applyArg).contains(matcherArg)))");
    PageObjectValidationTestHelper.validateMethod(method, expected);
  }

  @Test
  public void testFilterByGetCssPropertyValue() {
    TranslationContext context =
        new DeserializerUtilities().getContext("filter/basicFilterGetCssPropertyValue");
    PageObjectMethod method = context.getMethod(ELEMENT_METHOD_NAME);
    MethodInfo expected = new MethodInfo(ELEMENT_METHOD_NAME, "List<BasicElement>");
    expected.addParameter(new MethodParameterInfo("scopeArg"));
    expected.addParameter(new MethodParameterInfo("selectorArg"));
    expected.addParameter(new MethodParameterInfo("applyArg"));
    expected.addParameter(new MethodParameterInfo("matcherArg"));
    expected.addCodeLine("BasicElement scope = this.getScopeElement(scopeArg)");
    expected.addCodeLines(
        "return basic(scope, this.test.setParameters(selectorArg)).buildList(BasicElement.class,"
            + " BasePageElement.class, elm -> (elm.getCssPropertyValue(applyArg)!= null &&"
            + " elm.getCssPropertyValue(applyArg).contains(matcherArg)))");
    PageObjectValidationTestHelper.validateMethod(method, expected);
  }

  @Test
  public void testFilterByIsVisibleFalseFindFirst() {
    TranslationContext context =
        new DeserializerUtilities().getContext("filter/basicFilterIsVisible");
    PageObjectMethod method = context.getMethod(ELEMENT_METHOD_NAME);
    MethodInfo expected = new MethodInfo(ELEMENT_METHOD_NAME, "TestElement");
    expected.addParameter(new MethodParameterInfo("scopeArg"));
    expected.addParameter(new MethodParameterInfo("selectorArg"));
    expected.addCodeLine("BasicElement scope = this.getScopeElement(scopeArg)");
    expected.addCodeLines(
        "return basic(scope, this.test.setParameters(selectorArg)).build(TestElement.class,"
            + " TestElementImpl.class, elm -> Boolean.FALSE.equals(elm.isVisible()))");
    PageObjectValidationTestHelper.validateMethod(method, expected);
  }

  @Test
  public void testFilterWithContainsElementSelectorArgument() {
    TranslationContext context =
        new DeserializerUtilities().getContext("filter/basicWithFilterContainsElement");
    PageObjectMethod method = context.getMethod(ELEMENT_METHOD_NAME);
    assertThat(
        method.getDeclaration().getCodeLine(), is(equalTo("BasicElement getTest(String value)")));
    MethodInfo expected = new MethodInfo(ELEMENT_METHOD_NAME, "BasicElement");
    expected.addParameter(new MethodParameterInfo("value", "String"));
    expected.addParameter(
        new MethodParameterInfo(
            "LocatorBy.byCss(String.format(\"input[value='%s']\", value))", "LocatorBy"));
    expected.addCodeLine("BasicElement root = this.getRootElement()");
    expected.addCodeLine(
        "return basic(root, this.test).build(BasicElement.class, BasePageElement.class, elm ->"
            + " Boolean.TRUE.equals(elm.containsElement(LocatorBy.byCss(String.format(\"input[value='%s']\","
            + " value)))))");
    expected.addImportedTypes("utam.core.element.BasicElement");
    expected.addImpliedImportedTypes("utam.core.element.BasicElement");
    expected.addImpliedImportedTypes("utam.core.framework.element.BasePageElement");
    expected.addImpliedImportedTypes("utam.core.selenium.element.LocatorBy");
    PageObjectValidationTestHelper.validateMethod(method, expected);
  }
}
