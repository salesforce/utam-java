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
import static utam.compiler.grammar.UtamMethod.ERR_BEFORE_LOAD_HAS_NO_ARGS;
import static utam.compiler.grammar.UtamPageObject.ERR_DISALLOWED_ELEMENT;
import static utam.compiler.helpers.MethodContext.BEFORE_LOAD_METHOD_NAME;

import org.testng.annotations.Test;
import utam.compiler.helpers.TranslationContext;
import utam.compiler.representation.PageObjectValidationTestHelper;
import utam.compiler.representation.PageObjectValidationTestHelper.MethodInfo;
import utam.core.declarative.representation.PageObjectMethod;
import utam.core.framework.consumer.UtamError;

public class UtamPageObjectBeforeLoadTests {

  private static final String METHOD_NAME = BEFORE_LOAD_METHOD_NAME;

  private static TranslationContext getContext(String filename) {
    return new DeserializerUtilities().getContext("beforeload/" + filename);
  }

  private static MethodInfo getExpectedMethod() {
    return new MethodInfo(METHOD_NAME, "Object");
  }

  @Test
  public void testBeforeLoadApplyBasicActionsToRootReturnsSelf() {
    MethodInfo methodInfo = getExpectedMethod();
    TranslationContext context = getContext("rootApplyReturnSelf");
    PageObjectMethod method = context.getMethod(METHOD_NAME);
    assertThat(method.getDeclaration().getCodeLine(), is("Object load()"));
    methodInfo.addCodeLine("BasePageElement root0 = this.getRootElement()");
    methodInfo.addCodeLine("root0.isPresent()");
    methodInfo.addCodeLine("root0.getText()");
    methodInfo.addCodeLine("return this");
    PageObjectValidationTestHelper.validateMethod(method, methodInfo);
  }

  @Test
  public void testBeforeLoadApplyBasicActionsWithMatcher() {
    MethodInfo methodInfo = getExpectedMethod();
    TranslationContext context = getContext("rootApplyWithMatcher");
    PageObjectMethod method = context.getMethod(METHOD_NAME);
    assertThat(method.getDeclaration().getCodeLine(), is("Object load()"));
    methodInfo.addCodeLine("BasicElement root0 = this.getRoot()");
    methodInfo.addCodeLine("String statement0 = root0.getText()");
    methodInfo.addCodeLine("Boolean matcher0 = \"text\".equals(statement0)");
    methodInfo.addCodeLine("return matcher0");
    PageObjectValidationTestHelper.validateMethod(method, methodInfo);
  }

  @Test
  public void testBeforeLoadWaitDocumentUrlWithMatcher() {
    TranslationContext context = getContext("waitForUrlWithMatcher");
    MethodInfo methodInfo = getExpectedMethod();
    methodInfo.addCodeLine("this.waitFor(() -> {\n"
        + "String pstatement0 = this.getDocument().getUrl();\n"
        + "Boolean pmatcher0 = (pstatement0!= null && pstatement0.contains(\"home\"));\n"
        + "return pmatcher0;\n"
        + "})");
    methodInfo.addCodeLine("this.getDocument().waitForDocumentReady()");
    methodInfo.addCodeLine("return this");
    PageObjectMethod method = context.getMethod(METHOD_NAME);
    PageObjectValidationTestHelper.validateMethod(method, methodInfo);
  }

  @Test
  public void testBeforeLoadWait() {
    TranslationContext context = getContext("waitForRootPresence");
    MethodInfo methodInfo = getExpectedMethod();
    methodInfo.addCodeLine("Boolean statement0 = this.waitFor(() -> {\n"
        + "BasePageElement proot0 = this.getRootElement();\n"
        + "Boolean pstatement0 = proot0.isPresent();\n"
        + "return pstatement0;\n"
        + "})");
    methodInfo.addCodeLine("return statement0");
    PageObjectValidationTestHelper.validateMethod(context.getMethod(METHOD_NAME), methodInfo);
  }

  @Test
  public void testWaitForRootPresenceReturnsSelf() {
    TranslationContext context = getContext("waitForRootReturnsSelf");
    MethodInfo methodInfo = getExpectedMethod();
    methodInfo.addCodeLine("Test statement0 = this.waitFor(() -> {\n"
        + "BasePageElement proot0 = this.getRootElement();\n"
        + "proot0.isPresent();\n"
        + "return this;\n"
        + "})");
    methodInfo.addCodeLine("return statement0");
    PageObjectValidationTestHelper.validateMethod(context.getMethod(METHOD_NAME), methodInfo);
  }

  @Test
  public void testCantHaveArgs() {
    UtamError e = expectThrows(UtamError.class,
        () -> new DeserializerUtilities().getContext("validate/beforeload/withArgs"));
    assertThat(e.getMessage(), containsString(ERR_BEFORE_LOAD_HAS_NO_ARGS));
  }

  @Test
  public void testUnallowedElement() {
    UtamError e = expectThrows(UtamError.class,
        () -> new DeserializerUtilities().getContext("validate/beforeload/wrongElement"));
    assertThat(e.getMessage(), containsString(ERR_DISALLOWED_ELEMENT));
  }
}
