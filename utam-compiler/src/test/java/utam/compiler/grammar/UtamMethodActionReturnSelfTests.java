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

import org.testng.annotations.Test;
import utam.compiler.helpers.TranslationContext;
import utam.compiler.representation.PageObjectValidationTestHelper;
import utam.compiler.representation.PageObjectValidationTestHelper.MethodInfo;
import utam.core.declarative.representation.PageObjectMethod;
import utam.core.framework.consumer.UtamError;

/**
 * test composed "apply" : "returnSelf" statements
 *
 * @author elizaveta.ivanova
 * @since 236
 */
public class UtamMethodActionReturnSelfTests {

  private static final String methodName = "test";

  private static MethodInfo getMethodInfo() {
    return new MethodInfo(methodName, "Test");
  }

  private static TranslationContext getContext(String fileName) {
    return new DeserializerUtilities().getContext("compose/returnSelf/" + fileName);
  }

  @Test
  public void testReturnSelfWithMethod() {
    TranslationContext context = getContext("returnSelf");
    PageObjectMethod method = context.getMethod(methodName);
    MethodInfo expected = getMethodInfo();
    expected.addCodeLine("this.someMethod()");
    expected.addCodeLine("return this");
    PageObjectValidationTestHelper.validateMethod(method, expected);
  }

  @Test
  public void testReturnSelfOnly() {
    TranslationContext context = getContext("returnSelfOnly");
    PageObjectMethod method = context.getMethod(methodName);
    MethodInfo expected = getMethodInfo();
    expected.addCodeLine("return this");
    PageObjectValidationTestHelper.validateMethod(method, expected);
  }

  @Test
  public void testReturnSelfInsidePredicate() {
    TranslationContext context = getContext("predicateReturnSelf");
    PageObjectMethod method = context.getMethod(methodName);
    MethodInfo expected = getMethodInfo();
    expected.addCodeLine(
        "Test statement0 = this.waitFor(() -> {\n"
            + "BasePageElement proot0 = this.getRootElement();\n"
            + "proot0.getText();\n"
            + "return this;\n"
            + "})");
    expected.addCodeLine("return statement0");
    PageObjectValidationTestHelper.validateMethod(method, expected);
  }

  @Test
  public void returnSelfRedundantAttributes() {
    UtamError e = expectThrows(UtamError.class, () -> getContext("returnSelfError"));
    assertThat(
        e.getMessage(),
        containsString("error 600: method \"test\" statement: incorrect compose statement format"));
  }

  @Test
  public void returnSelfNotLastStatementThrows() {
    UtamError e = expectThrows(UtamError.class, () -> getContext("notLastStatement"));
    assertThat(
        e.getMessage(),
        containsString(
            "error 608: method \"test\" statement: "
                + "\"returnSelf\" can only be invoked from the last statement"));
  }

  @Test
  public void returnSelfNotLastStatementPredicateThrows() {
    UtamError e = expectThrows(UtamError.class, () -> getContext("notLastStatementPredicate"));
    assertThat(
        e.getMessage(),
        containsString(
            "error 608: method \"test\" statement: "
                + "\"returnSelf\" can only be invoked from the last statement"));
  }

  @Test
  public void returnSelfNotLastStatementBeforeLoadThrows() {
    UtamError e = expectThrows(UtamError.class, () -> getContext("notLastStatementBeforeLoad"));
    assertThat(
        e.getMessage(),
        containsString(
            "error 608: method \"load\" statement: "
                + "\"returnSelf\" can only be invoked from the last statement"));
  }
}
