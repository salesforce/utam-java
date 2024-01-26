/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.core.StringContains.containsString;
import static org.testng.Assert.expectThrows;
import static utam.compiler.helpers.TypeUtilities.COLLECTOR_IMPORT;
import static utam.compiler.helpers.TypeUtilities.PAGE_OBJECT;
import static utam.compiler.representation.PageObjectValidationTestHelper.validateMethodEmptyImports;

import java.util.List;
import org.testng.annotations.Test;
import utam.compiler.helpers.TranslationContext;
import utam.compiler.representation.PageObjectValidationTestHelper;
import utam.compiler.representation.PageObjectValidationTestHelper.MethodInfo;
import utam.compiler.representation.PageObjectValidationTestHelper.MethodParameterInfo;
import utam.core.declarative.representation.PageObjectMethod;
import utam.core.framework.consumer.UtamError;

/**
 * test validation errors in JSON files with getter
 *
 * @author elizaveta.ivanova
 * @since 236
 */
public class UtamMethodActionGetterTests {

  private static final String methodName = "test";
  private static final String LIST_IMPORT = List.class.getName();
  private static final String CUSTOM_TYPE_IMPORT = "my.pageobject.Foo";
  private static final String COLLECTOR_IMPORT_STR = COLLECTOR_IMPORT.getFullName();

  private static void testThrows(String jsonFile, String expectedError) {
    UtamError e =
        expectThrows(
            UtamError.class,
            () -> new DeserializerUtilities().getContext("validate/getter/" + jsonFile));
    assertThat(e.getMessage(), containsString(expectedError));
  }

  private static TranslationContext getContext(String filename) {
    return new DeserializerUtilities().getContext("compose/getter/" + filename);
  }

  @Test
  public void unknownElementNameThrows() {
    testThrows(
        "unknownElementName",
        "error 601: method \"test\" statement: unknown element with name \"test\" is referenced in"
            + " a compose statement");
  }

  @Test
  public void incorrectMatcherThrows() {
    String error =
        "error 1202: method \"test\" statement matcher: "
            + "applied method returns type \"BasicElement\", "
            + "which is only compatible with the following matchers - notNull";
    testThrows("incorrectMatcherType", error);
  }

  @Test
  public void redundantArgsThrows() {
    testThrows(
        "redundantArgs",
        "error 108: method \"test\" arguments: expected number of arguments is 0, found 1");
  }

  @Test
  public void testIncorrectStatementReturn() {
    String expectedError =
        "error 613: method \"test\" statement: incorrect return type; expected \"ElementElement\","
            + " provided is \"String\"";
    testThrows("incorrectStatementReturn", expectedError);
  }

  @Test
  public void testFirstChainStatement() {
    String expectedError =
        "error 616: method \"test\" statement: first statement can't be marked as chain";
    testThrows("firstChain", expectedError);
  }

  @Test
  public void testChainCantBeApplied() {
    String expectedError =
        "error 614: method \"test\" statement: to use chain, previous statement should return"
            + " custom type, but it returns \"BasicElement\"";
    testThrows("chainNotAllowed", expectedError);
  }

  @Test
  public void testChainNeedsReturn() {
    testThrows(
        "chainNeedsReturn",
        "error 605: method \"test\" statement: can't infer return type for \"element\", please"
            + " provide a \"returnType\"");
  }

  @Test
  public void testFrameElement() {
    TranslationContext getterContext = getContext("frameGetter");
    PageObjectMethod method = getterContext.getMethod(methodName);
    MethodInfo expected = new MethodInfo(methodName, "FrameElement");
    expected.addParameter(new MethodParameterInfo("selectorArgs"));
    expected.addCodeLine("FrameElement statement0 = this.getFrameElement(selectorArgs)");
    expected.addCodeLine("return statement0");
    PageObjectValidationTestHelper.validateMethod(method, expected);
  }

  @Test
  public void testFrameElementWithMatcher() {
    TranslationContext getterContext = getContext("frameGetterWithMatcher");
    PageObjectMethod method = getterContext.getMethod(methodName);
    MethodInfo expected = new MethodInfo(methodName, "Boolean");
    expected.addCodeLine("Object statement0 = this.getFrameElement(\"hardcoded\")");
    expected.addCodeLine("Boolean matcher0 = statement0 != null");
    expected.addCodeLine("return matcher0");
    PageObjectValidationTestHelper.validateMethod(method, expected);
  }

  @Test
  public void testCustomElement() {
    TranslationContext context = getContext("customPrivateSingle");
    PageObjectMethod method = context.getMethod(methodName);
    MethodInfo expected = new MethodInfo(methodName, "Foo");
    expected.addImportedTypes(CUSTOM_TYPE_IMPORT);
    expected.addImpliedImportedTypes(CUSTOM_TYPE_IMPORT);
    expected.addCodeLine("Foo statement0 = this.getTestElement()");
    expected.addCodeLine("return statement0");
    PageObjectValidationTestHelper.validateMethod(method, expected);
  }

  @Test
  public void testCustomReturnsList() {
    TranslationContext context = getContext("customList");
    PageObjectMethod method = context.getMethod(methodName);
    MethodInfo expected = new MethodInfo(methodName, "List<Foo>");
    expected.addImportedTypes(CUSTOM_TYPE_IMPORT, LIST_IMPORT);
    expected.addImpliedImportedTypes(CUSTOM_TYPE_IMPORT, LIST_IMPORT);
    expected.addCodeLine("List<Foo> statement0 = this.getPublicList()");
    expected.addCodeLine("return statement0");
    PageObjectValidationTestHelper.validateMethod(method, expected);
  }

  @Test
  public void testCustomFindFirst() {
    TranslationContext context = getContext("customFindFirst");
    PageObjectMethod method = context.getMethod(methodName);
    MethodInfo expected = new MethodInfo(methodName, "Foo");
    expected.addParameter(new MethodParameterInfo("filterArg"));
    expected.addImportedTypes(CUSTOM_TYPE_IMPORT);
    expected.addImpliedImportedTypes(CUSTOM_TYPE_IMPORT);
    expected.addCodeLine("Foo statement0 = this.getFindFirstElement(filterArg)");
    expected.addCodeLine("return statement0");
    PageObjectValidationTestHelper.validateMethod(method, expected);
  }

  @Test
  public void testCustomFindAll() {
    TranslationContext context = getContext("customFindAll");
    PageObjectMethod method = context.getMethod(methodName);
    MethodInfo expected = new MethodInfo(methodName, "List<Foo>");
    expected.addImportedTypes(CUSTOM_TYPE_IMPORT, LIST_IMPORT);
    expected.addImpliedImportedTypes(CUSTOM_TYPE_IMPORT, LIST_IMPORT);
    expected.addCodeLine("List<Foo> statement0 = this.getFindAllElement(\"hardcoded\")");
    expected.addCodeLine("return statement0");
    PageObjectValidationTestHelper.validateMethod(method, expected);
  }

  @Test
  public void testCustomFindFirstWithMatcher() {
    TranslationContext context = getContext("customWithMatcher");
    PageObjectMethod actualMethod = context.getMethod(methodName);
    MethodInfo expected = new MethodInfo(methodName, "Boolean");
    expected.addParameter(new MethodParameterInfo("filterArg"));
    expected.addImpliedImportedTypes(CUSTOM_TYPE_IMPORT);
    expected.addCodeLine("this.getFindFirstElement(filterArg)");
    expected.addCodeLine("Object statement1 = this.getFindFirstElement(filterArg)");
    expected.addCodeLine("Boolean matcher1 = statement1 != null");
    expected.addCodeLine("return matcher1");
    PageObjectValidationTestHelper.validateMethod(actualMethod, expected);
  }

  @Test
  public void testCustomElementListGetterReused() {
    TranslationContext context = getContext("customListReused");
    PageObjectMethod actualMethod = context.getMethod(methodName);
    MethodInfo expected = new MethodInfo(methodName, "List<Foo>");
    expected.addParameter(new MethodParameterInfo("filterArg"));
    expected.addImportedTypes(CUSTOM_TYPE_IMPORT, LIST_IMPORT);
    expected.addImpliedImportedTypes(CUSTOM_TYPE_IMPORT, LIST_IMPORT);
    expected.addCodeLine("this.getFindAllElement(filterArg)");
    expected.addCodeLine("List<Foo> statement1 = this.getFindAllElement(filterArg)");
    expected.addCodeLine("return statement1");
    PageObjectValidationTestHelper.validateMethod(actualMethod, expected);
  }

  @Test
  public void testBasicNoTypePrivateElement() {
    TranslationContext context = getContext("basicPrivateNoType");
    PageObjectMethod method = context.getMethod(methodName);
    MethodInfo expected = new MethodInfo(methodName, "BasicElement");
    expected.addCodeLine("BasicElement statement0 = this.getNoTypePrivateElement()");
    expected.addCodeLine("return statement0");
    PageObjectValidationTestHelper.validateMethod(method, expected);
  }

  @Test
  public void testBasicElementWithFilterFindFirst() {
    TranslationContext context = getContext("basicFindFirst");
    PageObjectMethod method = context.getMethod(methodName);
    assertThat(method.getDeclaration().getCodeLine(), is("BasicElement test(String filterArg)"));
    MethodInfo expected = new MethodInfo(methodName, "BasicElement");
    expected.addParameter(new MethodParameterInfo("filterArg"));
    expected.addCodeLine("BasicElement statement0 = this.getFilterFindFirstElement(filterArg)");
    expected.addCodeLine("return statement0");
    PageObjectValidationTestHelper.validateMethod(method, expected);
  }

  @Test
  public void testBasicElementWithMatcherAndReturnType() {
    TranslationContext context = getContext("basicWithMatcher");
    PageObjectMethod method = context.getMethod(methodName);
    MethodInfo expected = new MethodInfo(methodName, "Boolean");
    expected.addCodeLine("Object statement0 = this.getNoTypePrivateElement()");
    expected.addCodeLine("Boolean matcher0 = statement0 != null");
    expected.addCodeLine("return matcher0");
    PageObjectValidationTestHelper.validateMethod(method, expected);
  }

  @Test
  public void testCustomChainReturnAll() {
    TranslationContext context = getContext("chainReturnAll");
    PageObjectMethod method = context.getMethod(methodName);
    MethodInfo expected = new MethodInfo(methodName, "List<FooGrandChild>");
    expected.addImpliedImportedTypes(
        LIST_IMPORT,
        COLLECTOR_IMPORT_STR,
        CUSTOM_TYPE_IMPORT,
        "my.pageobject.FooChild",
        "my.pageobject.FooGrandChild");
    expected.addImportedTypes(LIST_IMPORT, "my.pageobject.FooGrandChild");
    expected.addCodeLine("List<Foo> statement0 = this.getListElement()");
    expected.addCodeLine(
        "List<FooChild> statement1 = statement0.stream().flatMap(element ->"
            + " element.getChild().stream()).collect(Collectors.toList())");
    expected.addCodeLine(
        "List<FooGrandChild> statement2 = statement1.stream().map(element ->"
            + " element.getGrandChild()).collect(Collectors.toList())");
    expected.addCodeLine("return statement2");
    PageObjectValidationTestHelper.validateMethod(method, expected);
  }

  @Test
  public void testContainerElementWithChain() {
    TranslationContext context = getContext("containerChain");
    PageObjectMethod method = context.getMethod(methodName);
    MethodInfo expected = new MethodInfo(methodName, "Boolean");
    expected.addParameter(new MethodParameterInfo("arg1"));
    expected.addParameter(new MethodParameterInfo("arg2"));
    expected.addImpliedImportedTypes(CUSTOM_TYPE_IMPORT);
    expected.addCodeLine("Foo statement0 = this.getContentElement(Foo.class)");
    expected.addCodeLine("Foo statement1 = statement0.getSelf(arg1)");
    expected.addCodeLine("String statement2 = statement1.getString()");
    expected.addCodeLine("Boolean matcher2 = arg2.equals(statement2)");
    expected.addCodeLine("return matcher2");
    PageObjectValidationTestHelper.validateMethod(method, expected);
  }

  @Test
  public void testContainerElementWithTypeNamesCollision() {
    TranslationContext context = getContext("containerNamesCollision");
    PageObjectMethod method1 = context.getMethod(methodName);
    MethodInfo expected1 = new MethodInfo(methodName, "Foo");
    expected1.addImpliedImportedTypes("my1.pageobject.Foo");
    expected1.addImportedTypes("my1.pageobject.Foo");
    expected1.addCodeLine("Foo statement0 = this.getContentElement(Foo.class)");
    expected1.addCodeLine("return statement0");
    PageObjectValidationTestHelper.validateMethod(method1, expected1);

    PageObjectMethod method2 = context.getMethod("test2");
    MethodInfo expected2 = new MethodInfo("test2", "my2.pageobject.Foo");
    validateMethodEmptyImports(method2);
    expected2.addCodeLine(
        "my2.pageobject.Foo statement0 = this.getContentElement(my2.pageobject.Foo.class)");
    expected2.addCodeLine("return statement0");
    PageObjectValidationTestHelper.validateMethod(method2, expected2);
  }

  @Test
  public void testChainWithNestedArgs() {
    TranslationContext context = getContext("chainNestedArgs");
    PageObjectMethod method = context.getMethod(methodName);
    MethodInfo expected = new MethodInfo(methodName, "Foo");
    expected.addParameter(new MethodParameterInfo("selectorArgName"));
    expected.addImpliedImportedTypes(CUSTOM_TYPE_IMPORT);
    expected.addCodeLine("Foo statement0 = this.getContentElement()");
    expected.addCodeLine("Foo statement1 = statement0.getNext(selectorArgName)");
    expected.addCodeLine("Foo statement2 = statement1.getNext(\"selectorArgValue\")");
    expected.addCodeLine("return statement2");
    PageObjectValidationTestHelper.validateMethod(method, expected);
  }

  @Test
  public void testCustomChainWithArgs() {
    TranslationContext context = getContext("argsReference");
    PageObjectMethod method = context.getMethod(methodName);
    MethodInfo expected = new MethodInfo(methodName, "List<Foo>");
    expected.addParameter(new MethodParameterInfo("elementArg"));
    expected.addCodeLine("List<Foo> statement0 = this.getSections(elementArg)");
    expected.addCodeLine(
        "List<Foo> statement1 = statement0.stream().flatMap(element ->"
            + " element.getRows(elementArg).stream()).collect(Collectors.toList())");
    expected.addCodeLine("return statement1");
    PageObjectValidationTestHelper.validateMethod(method, expected);
  }

  @Test
  public void testReuseSameElementWithArgInsidePredicate() {
    TranslationContext context = getContext("reuseSameArg");
    PageObjectMethod method = context.getMethod(methodName);
    MethodInfo expected = new MethodInfo(methodName);
    expected.addParameter(new MethodParameterInfo("elementArg", "Integer"));
    expected.addParameter(new MethodParameterInfo("anotherArg", "Integer"));
    expected.addCodeLine(
        "this.waitFor(() -> {\n"
            + "RecordLayoutSection pstatement0 = this.getOneElement(elementArg);\n"
            + "return pstatement0;\n"
            + "})");
    expected.addCodeLine("RecordLayoutSection statement1 = this.getOneElement(elementArg)");
    expected.addCodeLine("statement1.something(anotherArg)");
    PageObjectValidationTestHelper.validateMethod(method, expected);
  }

  @Test
  public void testLiteralGetterArgument() {
    PageObjectMethod method =
        new DeserializerUtilities()
            .getContext("generated/args/literalGetterArg.utam")
            .getMethod("testGetter");
    MethodInfo expected = new MethodInfo("testGetter", "SettingsPanelElement");
    expected.addCodeLine("SettingsPanelElement statement0 = this.getSettingsPanel()");
    expected.addCodeLine("return statement0");
    PageObjectValidationTestHelper.validateMethod(method, expected);
  }

  @Test
  public void testContainerGetterWithoutArgs() {
    String methodName = "composeContainer";
    PageObjectMethod method = getContext("containerCompose").getMethod(methodName);
    MethodInfo expected = new MethodInfo(methodName, "T");
    expected.addParameter(new MethodParameterInfo("pageObjectType", "Class<T>"));
    expected.addCodeLine("T statement0 = this.getContainerElement(pageObjectType)");
    expected.addCodeLine("return statement0");
    expected.addImportedTypes(PAGE_OBJECT.getFullName());
    expected.addImpliedImportedTypes(PAGE_OBJECT.getFullName());
    PageObjectValidationTestHelper.validateMethod(method, expected);
  }
}
