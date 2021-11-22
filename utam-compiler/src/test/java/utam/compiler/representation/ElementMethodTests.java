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
import static org.hamcrest.Matchers.emptyIterable;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static utam.compiler.helpers.TypeUtilities.BASIC_ELEMENT;
import static utam.compiler.helpers.TypeUtilities.BASIC_ELEMENT_IMPL_CLASS;
import static utam.compiler.representation.ElementMethod.DOCUMENT_GETTER;

import java.util.List;
import org.testng.annotations.Test;
import utam.compiler.grammar.DeserializerUtilities;
import utam.compiler.grammar.DeserializerUtilities.Result;
import utam.compiler.helpers.TranslationContext;
import utam.compiler.representation.PageObjectValidationTestHelper.MethodInfo;
import utam.compiler.representation.PageObjectValidationTestHelper.MethodParameterInfo;
import utam.core.declarative.representation.PageObjectMethod;
import utam.core.declarative.representation.UnionType;

/**
 * Provides tests for the ElementMethod class
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
    return (BasicElementGetterMethod) context.getElement(ELEMENT_NAME).getElementMethod();
  }

  @Test
  public void testDocumentGetter() {
    PageObjectMethod method = DOCUMENT_GETTER;
    assertThat(method.isPublic(), is(false));
    assertThat(method.getClassImports(), is(empty()));
    assertThat(method.getCodeLines().get(0), is(equalTo("this.getDocument()")));
  }

  @Test
  public void testBasicPrivateElementDefaultType() {
    MethodInfo expected = new MethodInfo(ELEMENT_METHOD_NAME_PRIVATE, "BasicElement");
    Result result = new DeserializerUtilities().getResultFromFile("element/basicElement");
    TranslationContext context = result.getContext();
    BasicElementGetterMethod method = getElementMethod(context);
    assertThat(method.getClassUnionType(), is(nullValue()));
    assertThat(method.getInterfaceUnionType(), is(nullValue()));
    expected.addCodeLine(
        "return element(this.test).build(BasicElement.class, BasePageElement.class)");
    expected.addImportedTypes(BASIC_ELEMENT_TYPE);
    expected.addImpliedImportedTypes(BASIC_ELEMENT_TYPE_IMPL, BASIC_ELEMENT_TYPE);
    expected.setNotPublic();
    PageObjectValidationTestHelper.validateMethod(method, expected);
  }

  @Test
  public void testBasicPrivateElementUnionTypeInImplOnly() {
    MethodInfo expected = new MethodInfo(ELEMENT_METHOD_NAME_PRIVATE, "TestElement");
    Result result = new DeserializerUtilities()
        .getResultFromFile("element/basicElementTypesImplOnly");
    TranslationContext context = result.getContext();
    BasicElementGetterMethod method = getElementMethod(context);
    expected
        .addCodeLine("return element(this.test).build(TestElement.class, TestElementImpl.class)");
    expected.setNotPublic();
    PageObjectValidationTestHelper.validateMethod(method, expected);
    List<UnionType> unionTypes = context.getClassUnionTypes();
    assertThat(unionTypes, hasSize(2));
    assertThat(unionTypes.get(0).getDeclarationCode().get(0),
        is(equalTo("interface TestElement extends Actionable {}")));
    assertThat(unionTypes.get(1).getDeclarationCode().get(0), is(equalTo(
        "public static class TestElementImpl extends BasePageElement implements TestElement {}")));
  }

  @Test
  public void testBasicPublicElementUnionTypeInImplOnly() {
    MethodInfo expected = new MethodInfo(ELEMENT_METHOD_NAME, "GetTestElement");
    Result result = new DeserializerUtilities()
        .getResultFromFile("element/publicElementTypesImplOnly");
    TranslationContext context = result.getContext();
    BasicElementGetterMethod method = getElementMethod(context);
    expected.addCodeLine(
        "return element(this.test).build(GetTestElement.class, GetTestElementImpl.class, arg)");
    expected.addParameter(new MethodParameterInfo("arg"));
    PageObjectValidationTestHelper.validateMethod(method, expected);
    List<UnionType> unionTypes = context.getClassUnionTypes();
    assertThat(unionTypes, hasSize(1));
    assertThat(unionTypes.get(0).getDeclarationCode().get(0), is(equalTo(
        "public static class GetTestElementImpl extends BasePageElement implements GetTestElement {}")));
  }

  @Test
  public void testListBasicPrivateElementDefaultType() {
    MethodInfo expected = new MethodInfo(ELEMENT_METHOD_NAME_PRIVATE, "List<BasicElement>");
    Result result = new DeserializerUtilities().getResultFromFile("element/basicListBasicElement");
    TranslationContext context = result.getContext();
    BasicElementGetterMethod method = getElementMethod(context);
    assertThat(method.getClassUnionType(), is(nullValue()));
    assertThat(method.getInterfaceUnionType(), is(nullValue()));
    expected.addCodeLine(
        "return element(this.test).buildList(BasicElement.class, BasePageElement.class)");
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
    expected.addCodeLine(
        "return element(this.test).buildList(BasicElement.class, BasePageElement.class, arg)");
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
    expected.addCodeLine("return element(this.test).buildList(TestElement.class, TestElementImpl.class)");
    expected.addImportedTypes(LIST_TYPE);
    expected.addImpliedImportedTypes(LIST_TYPE);
    expected.setNotPublic();
    PageObjectValidationTestHelper.validateMethod(method, expected);
    String unionClass = method.getClassUnionType().getDeclarationCode().get(0);
    assertThat(unionClass, is(equalTo(
        "public static class TestElementImpl extends BasePageElement implements TestElement {}")));
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
    expected.addImpliedImportedTypes(LIST_TYPE);
    expected.addCodeLine("return element(this.test).buildList(TestElement.class, TestElementImpl.class)");
    PageObjectValidationTestHelper.validateMethod(method, expected);
    String unionClass = method.getClassUnionType().getDeclarationCode().get(0);
    assertThat(unionClass, is(equalTo(
        "public static class TestElementImpl extends BasePageElement implements TestElement {}")));
    String unionType = method.getInterfaceUnionType().getDeclarationCode().get(0);
    assertThat(unionType, is(equalTo("interface TestElement extends Editable, Clickable {}")));
  }

  @Test
  public void testFilterBasicPublicElementUnionType() {
    MethodInfo expected = new MethodInfo(ELEMENT_METHOD_NAME, "TestElement");
    Result result = new DeserializerUtilities().getResultFromFile("element/basicFilterPublicUnion");
    TranslationContext context = result.getContext();
    BasicElementGetterMethod method = getElementMethod(context);
    expected.addParameter(new MethodParameterInfo("text"));
    assertThat(method.getClassImports(), is(emptyIterable()));
    assertThat(method.getDeclaration().getImports(), is(emptyIterable()));
    expected.addCodeLine(
        "return element(this.test).build(TestElement.class, TestElementImpl.class, elm -> text.equals(elm.getText()))");
    PageObjectValidationTestHelper.validateMethod(method, expected);
    String unionClass = context.getClassUnionTypes().get(0).getDeclarationCode().get(0);
    assertThat(unionClass, is(equalTo(
        "public static class TestElementImpl extends BasePageElement implements TestElement {}")));
    String unionType = context.getInterfaceUnionTypes().get(0).getDeclarationCode()
        .get(0);
    assertThat(unionType, is(equalTo("interface TestElement extends Editable, Clickable {}")));
  }

  @Test
  public void testFilterBasicPublicElementDefaultType() {
    MethodInfo expected = new MethodInfo(ELEMENT_METHOD_NAME, "List<BasicElement>");
    Result result = new DeserializerUtilities().getResultFromFile("element/basicFilterPublic");
    TranslationContext context = result.getContext();
    BasicElementGetterMethod method = getElementMethod(context);
    expected.addParameter(new MethodParameterInfo("text"));
    expected.addImportedTypes(LIST_TYPE, BASIC_ELEMENT_TYPE);
    expected.addImpliedImportedTypes(LIST_TYPE, BASIC_ELEMENT_TYPE_IMPL);
    expected.addCodeLine(
        "return element(this.test).buildList(BasicElement.class, BasePageElement.class, elm -> text.equals(elm.getText()))");
    PageObjectValidationTestHelper.validateMethod(method, expected);
    assertThat(context.getInterfaceUnionTypes(), is(emptyIterable()));
    assertThat(context.getClassUnionTypes(), is(emptyIterable()));
  }

  @Test
  public void testNullableList() {
    MethodInfo expected = new MethodInfo(ELEMENT_METHOD_NAME, "List<BasicElement>");
    expected.addCodeLine("return element(this.test).buildList(BasicElement.class, BasePageElement.class)");
    Result result = new DeserializerUtilities().getResultFromFile("element/basicElementNullableList");
    TranslationContext context = result.getContext();
    BasicElementGetterMethod method = getElementMethod(context);
    PageObjectValidationTestHelper.validateMethod(method, expected);
  }

  @Test
  public void testNullableListWithFilter() {
    MethodInfo expected = new MethodInfo(ELEMENT_METHOD_NAME, "BasicElement");
    expected.addCodeLine("return element(this.test)"
        + ".build(BasicElement.class, BasePageElement.class, "
        + "elm -> Boolean.TRUE.equals(elm.isVisible()))");
    Result result = new DeserializerUtilities().getResultFromFile("element/basicElementFilter");
    TranslationContext context = result.getContext();
    BasicElementGetterMethod method = getElementMethod(context);
    PageObjectValidationTestHelper.validateMethod(method, expected);
  }

  @Test
  public void testNullableSingle() {
    MethodInfo expected = new MethodInfo(ELEMENT_METHOD_NAME, "BasicElement");
    expected.addCodeLine("return element(this.test).build(BasicElement.class, BasePageElement.class)");
    Result result = new DeserializerUtilities().getResultFromFile("element/basicElementNullableSingle");
    TranslationContext context = result.getContext();
    BasicElementGetterMethod method = getElementMethod(context);
    PageObjectValidationTestHelper.validateMethod(method, expected);
  }
}
