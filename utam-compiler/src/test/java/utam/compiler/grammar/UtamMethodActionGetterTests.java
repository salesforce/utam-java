/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.core.StringContains.containsString;
import static org.testng.Assert.expectThrows;
import static utam.compiler.grammar.UtamArgument.ERR_ARGS_WRONG_COUNT;
import static utam.compiler.grammar.UtamMethodAction.ERR_CHAIN_REQUIRES_CUSTOM_RETURN;
import static utam.compiler.grammar.UtamMethodAction.ERR_FIRST_STATEMENT_CANT_BE_MARKED_AS_CHAIN;
import static utam.compiler.grammar.UtamMethodAction.ERR_INCORRECT_RETURN_TYPE;
import static utam.compiler.grammar.UtamMethodActionGetter.ERR_CONTAINER_INVOCATION_NEEDS_RETURN_TYPE;
import static utam.compiler.grammar.UtamMethodActionGetter.ERR_RETURN_TYPE_CANT_BE_INFERRED;
import static utam.compiler.helpers.TranslationContext.ERR_CONTEXT_ELEMENT_NOT_FOUND;
import static utam.compiler.helpers.TypeUtilities.COLLECTOR_IMPORT;
import static utam.compiler.helpers.TypeUtilities.PAGE_OBJECT_PARAMETER;

import org.testng.annotations.Test;
import utam.compiler.helpers.MatcherType;
import utam.compiler.helpers.TranslationContext;
import utam.compiler.helpers.TypeUtilities;
import utam.compiler.helpers.TypeUtilities.FromString;
import utam.compiler.representation.PageObjectValidationTestHelper;
import utam.compiler.representation.PageObjectValidationTestHelper.MethodInfo;
import utam.compiler.representation.PageObjectValidationTestHelper.MethodParameterInfo;
import utam.core.declarative.representation.PageObjectMethod;
import utam.core.declarative.representation.TypeProvider;
import utam.core.framework.consumer.UtamError;

/**
 * test validation errors in JSON files with getter
 *
 * @author elizaveta.ivanova
 * @since 236
 */
public class UtamMethodActionGetterTests {

  private static final String methodName = "test";
  private static final String LIST_IMPORT = TypeUtilities.LIST_IMPORT.getFullName();
  private static final String CUSTOM_TYPE_IMPORT = "my.pageobject.Foo";
  private static final String COLLECTOR_IMPORT_STR = COLLECTOR_IMPORT.getFullName();

  private static void testThrows(String jsonFile, String expectedError) {
    UtamError e = expectThrows(UtamError.class,
        () -> new DeserializerUtilities().getContext("validate/getter/" + jsonFile));
    assertThat(e.getMessage(), containsString(expectedError));
  }

  private static TranslationContext getContext(String filename) {
    return new DeserializerUtilities().getContext("compose/getter/" + filename);
  }

  @Test
  public void unknownElementNameThrows() {
    testThrows("unknownElementName", String.format(ERR_CONTEXT_ELEMENT_NOT_FOUND,
        methodName));
  }

  @Test
  public void incorrectMatcherThrows() {
    TypeProvider returnedType = new FromString("TestElement");
    testThrows("incorrectMatcherType",
        MatcherType.isTrue.getIncorrectTypeError(returnedType));
  }

  @Test
  public void redundantArgsThrows() {
    testThrows("redundantArgs",
        String.format(ERR_ARGS_WRONG_COUNT, "method 'test'", 0, 1));
  }

  @Test
  public void testIncorrectStatementReturn() {
    String expectedError = String
        .format(ERR_INCORRECT_RETURN_TYPE, methodName, "ElementElement", "String");
    testThrows("incorrectStatementReturn", expectedError);
  }

  @Test
  public void testFirstChainStatement() {
    String expectedError = String.format(ERR_FIRST_STATEMENT_CANT_BE_MARKED_AS_CHAIN, methodName);
    testThrows("firstChain", expectedError);
  }

  @Test
  public void testChainCantBeApplied() {
    String expectedError = String
        .format(ERR_CHAIN_REQUIRES_CUSTOM_RETURN, methodName, "ElementElement");
    testThrows("chainNotAllowed", expectedError);
  }

  @Test
  public void testChainNeedsReturn() {
    String expectedError = String
        .format(ERR_RETURN_TYPE_CANT_BE_INFERRED, methodName, "element");
    testThrows("chainNeedsReturn", expectedError);
  }

  @Test
  public void testContainerElementNeedsReturn() {
    String expectedError = String
        .format(ERR_CONTAINER_INVOCATION_NEEDS_RETURN_TYPE, methodName);
    testThrows("containerNeedsReturn", expectedError);
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
    expected.addCodeLine("Foo statement0 = this.getPrivateElement()");
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
    MethodInfo expected = new MethodInfo(methodName, "NoTypePrivateElement");
    expected.addCodeLine("NoTypePrivateElement statement0 = this.getNoTypePrivateElement()");
    expected.addCodeLine("return statement0");
    PageObjectValidationTestHelper.validateMethod(method, expected);
  }

  @Test
  public void testBasicElementWithFilterFindFirst() {
    TranslationContext context = getContext("basicFindFirst");
    PageObjectMethod method = context.getMethod(methodName);
    MethodInfo expected = new MethodInfo(methodName, "FilterFindFirstElement");
    expected.addParameter(new MethodParameterInfo("filterArg"));
    expected.addCodeLine(
        "FilterFindFirstElement statement0 = this.getFilterFindFirstElement(filterArg)");
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
    expected.addImpliedImportedTypes(LIST_IMPORT,
        COLLECTOR_IMPORT_STR,
        CUSTOM_TYPE_IMPORT,
        "my.pageobject.FooChild",
        "my.pageobject.FooGrandChild");
    expected.addImportedTypes(LIST_IMPORT, "my.pageobject.FooGrandChild");
    expected.addCodeLine("List<Foo> statement0 = this.getListElement()");
    expected.addCodeLine(
        "List<FooChild> statement1 = statement0.stream().flatMap(element -> element.getChild().stream()).collect(Collectors.toList())");
    expected.addCodeLine(
        "List<FooGrandChild> statement2 = statement1.stream().map(element -> element.getGrandChild()).collect(Collectors.toList())");
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
  public void testContainerWithNonLiteralArg() {
    TranslationContext context = getContext("containerNonLiteralArg");
    PageObjectMethod method = context.getMethod(methodName);
    MethodInfo expected = new MethodInfo(methodName, "Foo");
    expected.addParameter(new MethodParameterInfo("pageObjectType", PAGE_OBJECT_PARAMETER.getSimpleName()));
    String importedType = PAGE_OBJECT_PARAMETER.getImportableTypes().get(0).getFullName();
    expected.addImpliedImportedTypes(CUSTOM_TYPE_IMPORT, importedType);
    expected.addImportedTypes(CUSTOM_TYPE_IMPORT, importedType);
    expected.addCodeLine("Foo statement0 = this.getContentElement(pageObjectType)");
    expected.addCodeLine("return statement0");
    PageObjectValidationTestHelper.validateMethod(method, expected);
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
    expected.addCodeLine("List<Foo> statement1 = statement0.stream().flatMap(element -> element.getRows(elementArg).stream()).collect(Collectors.toList())");
    expected.addCodeLine("return statement1");
    PageObjectValidationTestHelper.validateMethod(method, expected);
  }
}
