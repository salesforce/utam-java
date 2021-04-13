/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import utam.compiler.helpers.ClickableActionType;
import utam.compiler.helpers.TranslationContext;
import utam.compiler.representation.ComposeMethod;
import utam.core.declarative.representation.PageObjectMethod;
import utam.compiler.representation.PageObjectValidationTestHelper;
import utam.compiler.representation.PageObjectValidationTestHelper.MethodInfo;
import utam.core.framework.consumer.UtamError;
import org.testng.annotations.Test;

import static org.testng.Assert.assertThrows;
import static utam.compiler.grammar.TestUtilities.getElementPrivateMethodCalled;
import static utam.compiler.grammar.UtamMethod.*;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;
import static org.testng.Assert.expectThrows;

/**
 * Provides tests for the UtamElementFilter class
 *
 * @author james.evans
 */
public class UtamMethod_ComposeTests {

  private static final String METHOD_NAME = "testMethod";
  private static final String ELEMENT_NAME = "testElement";

  private static void setClickableElementContext(TranslationContext context) {
    new UtamElement(ELEMENT_NAME, "clickable", new UtamSelector(".fakeSelector"))
        .testTraverse(context);
  }

  private static String getErr(String message) {
    return String.format(message, METHOD_NAME);
  }

  /** The getComposeMethod method should return the proper value */
  @Test
  public void testGetComposeMethod() {
    TranslationContext context = TestUtilities.getTestTranslationContext();
    setClickableElementContext(context); // creates element in context
    UtamMethod method =
        new UtamMethod(
            METHOD_NAME,
            new UtamMethodAction[] {new UtamMethodAction(ELEMENT_NAME, "click")});
    MethodInfo methodInfo = new MethodInfo(METHOD_NAME, "void");
    methodInfo.addCodeLine(getElementPrivateMethodCalled(ELEMENT_NAME) + "().click()");
    PageObjectValidationTestHelper.validateMethod(method.getComposeMethod(context), methodInfo);
  }

  /** The getComposeMethod method should throw the proper exception if a return type is specified */
  @Test
  public void testGetComposeMethodWithIncorrectReturnTypeThrows() {
    TranslationContext context = TestUtilities.getTestTranslationContext();
    setClickableElementContext(context); // creates element in context
    UtamMethod method =
        new UtamMethod(
            "testMethod",
            new UtamMethodAction[] {new UtamMethodAction("testElement", "click")});
    method.returnStr = "unsupported type";
    assertThrows(UtamError.class, () -> method.getComposeMethod(context));
  }

  /** The getComposeMethod method should throw the proper exception if arguments specified */
  @Test
  public void testGetComposeMethodWithEmptyStatementListThrows() {
    TranslationContext context = TestUtilities.getTestTranslationContext();
    UtamMethod method = new UtamMethod("test", new UtamMethodAction[] {});

    UtamError e = expectThrows(UtamError.class, () -> method.getComposeMethod(context));
    assertThat(e.getMessage(), containsString(String.format(ERR_METHOD_EMPTY_STATEMENTS, "test")));
  }

  /** The getMethod method should return a valid value for a compose method */
  @Test
  public void testGetMethodForComposeMethod() {
    TranslationContext context = TestUtilities.getTestTranslationContext();
    // traverses
    setClickableElementContext(context);
    UtamMethod method =
        new UtamMethod(
            "testMethod",
            new UtamMethodAction[] {new UtamMethodAction(ELEMENT_NAME, "click")});
    MethodInfo methodInfo = new MethodInfo("testMethod", "void");
    methodInfo.addCodeLine(getElementPrivateMethodCalled(ELEMENT_NAME) + "().click()");

    PageObjectMethod methodObject = method.getMethod(context);
    assertThat(methodObject, is(instanceOf(ComposeMethod.class)));
    PageObjectValidationTestHelper.validateMethod(methodObject, methodInfo);
  }

  @Test
  public void testElementWithSelectorParameter() {
    TranslationContext context = TestUtilities.getTestTranslationContext();
    UtamElement scopeElement =
        new UtamElement(
            ELEMENT_NAME,
            "clickable",
            new UtamSelector(
                ".fakeSelector[%s]",
                new UtamArgument[] {new UtamArgument("selectorParameter", "string")}));
    scopeElement.testTraverse(context);
    UtamMethodAction action =
        new UtamMethodAction(
            ELEMENT_NAME, ClickableActionType.click.toString());
    UtamMethod method = new UtamMethod(METHOD_NAME, new UtamMethodAction[] { action, action });
    MethodInfo methodInfo = new MethodInfo(METHOD_NAME, "void");
    methodInfo.addParameter(
        new PageObjectValidationTestHelper.MethodParameterInfo("selectorParameter", "String"));
    String code = getElementPrivateMethodCalled(ELEMENT_NAME) + "(selectorParameter).click()";
    methodInfo.addCodeLine(code);
    methodInfo.addCodeLine(code);
    PageObjectMethod methodObject = method.getMethod(context);
    PageObjectValidationTestHelper.validateMethod(methodObject, methodInfo);
  }

  @Test
  public void testComposeWithListThrows() {
    TranslationContext context = TestUtilities.getTestTranslationContext();
    UtamMethod method = new UtamMethod(METHOD_NAME, new UtamMethodAction[] {});
    method.isReturnList = true;
    UtamError e = expectThrows(UtamError.class, () -> method.getMethod(context));
    assertThat(e.getMessage(), containsString(getErr(ERR_METHOD_RETURN_ALL_REDUNDANT)));
  }

  @Test
  public void testComposeArgsRedundant() {
    TranslationContext context = TestUtilities.getTestTranslationContext();
    UtamMethod method = new UtamMethod(METHOD_NAME, new UtamMethodAction[] {});
    method.args = new UtamArgument[0];
    UtamError e = expectThrows(UtamError.class, () -> method.getComposeMethod(context));
    assertThat(e.getMessage(), containsString(getErr(ERR_ARGS_NOT_ALLOWED)));
  }

  @Test
  public void testComposeRedundantChain() {
    TranslationContext context = TestUtilities.getTestTranslationContext();
    UtamMethod method = new UtamMethod(METHOD_NAME, new UtamMethodAction[] {});
    method.chain = new UtamMethodChainLink[0];
    UtamError e = expectThrows(UtamError.class, () -> method.getMethod(context));
    assertThat(e.getMessage(), containsString(getErr(ERR_METHOD_REDUNDANT_TYPE)));
  }
}
