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
import static org.hamcrest.core.StringContains.containsString;
import static org.testng.Assert.expectThrows;

import org.testng.annotations.Test;
import utam.compiler.UtamCompilationError;
import utam.compiler.helpers.TranslationContext;
import utam.compiler.representation.PageObjectValidationTestHelper;
import utam.compiler.representation.PageObjectValidationTestHelper.MethodInfo;
import utam.compiler.representation.PageObjectValidationTestHelper.MethodParameterInfo;
import utam.core.declarative.representation.PageObjectMethod;
import utam.core.framework.consumer.UtamError;

/**
 * test composed predicates
 *
 * @author elizaveta.ivanova
 * @since 236
 */
public class UtamMethodActionWaitForTests {

  private static final String methodName = "test";

  private static TranslationContext getContext(String fileName) {
    return new DeserializerUtilities().getContext("compose/predicate/" + fileName);
  }

  @Test
  public void nestedPredicateThrows() {
    String expectedError = "error 615: method \"test\" statement: nested waitFor is not supported";
    UtamError e =
        expectThrows(
            UtamCompilationError.class,
            () -> new DeserializerUtilities().getContext("validate/compose/nestedWait"));
    assertThat(e.getMessage(), containsString(expectedError));
  }

  @Test
  public void predicateWithIncorrectSecondArgsThrows() {
    String expectedError =
        "error 109: method \"test\" arguments: argument \"true\" has incorrect type, expected"
            + " \"literal string\", found \"Boolean\"";
    UtamError e =
        expectThrows(
            UtamCompilationError.class,
            () ->
                new DeserializerUtilities()
                    .getContext("validate/compose/waitForIncorrectMessageArg"));
    assertThat(e.getMessage(), containsString(expectedError));
  }

  @Test
  public void testWaitForCustomElement() {
    TranslationContext context = getContext("waitForCustom");
    PageObjectMethod method = context.getMethod(methodName);
    assertThat(
        method.getDeclaration().getCodeLine(),
        is("Boolean test(LocatorBy selectorArg, String matcherArg)"));
    MethodInfo methodInfo = new MethodInfo(methodName, "Boolean");
    methodInfo.addParameter(new MethodParameterInfo("selectorArg", "LocatorBy"));
    methodInfo.addParameter(new MethodParameterInfo("matcherArg", "String"));
    methodInfo.addCodeLine(
        "Boolean statement0 = this.waitFor(() -> {\n"
            + "Custom pcustom0 = this.getCustomElement();\n"
            + "if (pcustom0 == null) { return false; }\n"
            + "String pstatement0 = pcustom0.returnsString(selectorArg);\n"
            + "Boolean pmatcher0 = (pstatement0!= null && pstatement0.contains(matcherArg));\n"
            + "return pmatcher0;\n"
            + "})");
    methodInfo.addCodeLine("return statement0");
    PageObjectValidationTestHelper.validateMethod(method, methodInfo);
  }

  @Test
  public void testWaitForRootElement() {
    TranslationContext context = getContext("waitForRoot");
    MethodInfo methodInfo = new MethodInfo(methodName, "Boolean");
    methodInfo.addParameter(new MethodParameterInfo("matcherArg"));
    methodInfo.addCodeLine(
        "Boolean statement0 = this.waitFor(() -> {\n"
            + "BasePageElement proot0 = this.getRootElement();\n"
            + "String pstatement0 = proot0.getText();\n"
            + "Boolean pmatcher0 = matcherArg.equals(pstatement0);\n"
            + "return pmatcher0;\n"
            + "})");
    methodInfo.addCodeLine("return statement0");
    PageObjectValidationTestHelper.validateMethod(context.getMethod(methodName), methodInfo);
  }

  @Test
  public void testWaitForList() {
    TranslationContext context = getContext("waitForBasicList");
    PageObjectMethod method = context.getMethod(methodName);
    MethodInfo methodInfo = new MethodInfo(methodName, "List<String>");
    methodInfo.addCodeLine(
        "List<String> statement0 = this.waitFor(() -> {\n"
            + "List<BasicElement> plist0 = this.getListElement();\n"
            + "List<String> pstatement0 = plist0.stream().map(element ->"
            + " element.getText()).collect(Collectors.toList());\n"
            + "return pstatement0;\n"
            + "})");
    methodInfo.addCodeLine("return statement0");
    PageObjectValidationTestHelper.validateMethod(method, methodInfo);
  }

  @Test
  public void testComposeWaitForReturnSelf() {
    TranslationContext context = getContext("waitReturnSelf");
    PageObjectMethod method = context.getMethod(methodName);
    MethodInfo methodInfo = new MethodInfo(methodName, "Test");
    methodInfo.addCodeLine(
        "Test statement0 = this.waitFor(() -> {\n"
            + "RootElement proot0 = this.getRoot();\n"
            + "proot0.focus();\n"
            + "return this;\n"
            + "})");
    methodInfo.addCodeLine("return statement0");
    PageObjectValidationTestHelper.validateMethod(method, methodInfo);
  }

  @Test
  public void testComposeWaitForBasicVoidAction() {
    TranslationContext context = getContext("waitVoidAction");
    PageObjectMethod method = context.getMethod(methodName);
    MethodInfo methodInfo = new MethodInfo(methodName);
    methodInfo.addCodeLine(
        "this.waitFor(() -> {\n"
            + "RootElement proot0 = this.getRoot();\n"
            + "proot0.focus();\n"
            + "return true;\n"
            + "}, \"custom error message\")");
    PageObjectValidationTestHelper.validateMethod(method, methodInfo);
  }

  @Test
  public void testWaitForWithChain() {
    TranslationContext context = getContext("waitForWithChain");
    PageObjectMethod method = context.getMethod(methodName);
    MethodInfo methodInfo = new MethodInfo(methodName, "Custom");
    methodInfo.addCodeLine(
        "Custom statement0 = this.waitFor(() -> {\n"
            + "Custom pstatement0 = this.getCustomElement();\n"
            + "Custom pstatement1 = pstatement0.method1();\n"
            + "return pstatement1;\n"
            + "})");
    methodInfo.addCodeLine("Custom statement1 = statement0.method2()");
    methodInfo.addCodeLine("return statement1");
    String importType = "utam.test.pageobjects.Custom";
    methodInfo.addImportedTypes(importType);
    methodInfo.addImpliedImportedTypes(importType);
    PageObjectValidationTestHelper.validateMethod(method, methodInfo);
  }

  @Test
  public void testSameElementReusedInsideAndOutsidePredicate() {
    TranslationContext context = getContext("reuseElement");
    PageObjectMethod method = context.getMethod(methodName);
    MethodInfo methodInfo = new MethodInfo(methodName, "Boolean");
    methodInfo.addCodeLine("BasePageElement root0 = this.getRootElement()");
    methodInfo.addCodeLine("root0.isPresent()");
    methodInfo.addCodeLine(
        "this.waitFor(() -> {\n"
            + "BasePageElement pstatement0 = this.getRootElement();\n"
            + "return pstatement0;\n"
            + "})");
    methodInfo.addCodeLine("Boolean statement2 = root0.isPresent()");
    methodInfo.addCodeLine("return statement2");
    PageObjectValidationTestHelper.validateMethod(method, methodInfo);
  }
}
