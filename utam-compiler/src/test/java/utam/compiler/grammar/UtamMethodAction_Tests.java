/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;
import static org.testng.Assert.expectThrows;
import static utam.compiler.grammar.TestUtilities.TEST_PAGE_OBJECT;
import static utam.compiler.grammar.TestUtilities.TEST_URI;
import static utam.compiler.grammar.TestUtilities.getElementPrivateMethodCalled;
import static utam.compiler.helpers.PrimitiveType.NUMBER;
import static utam.compiler.helpers.TypeUtilities.SELECTOR;
import static utam.compiler.helpers.TypeUtilities.VOID;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.testng.annotations.Test;
import utam.compiler.grammar.UtamMethodAction.Custom;
import utam.compiler.helpers.ActionableActionType;
import utam.compiler.helpers.ClickableActionType;
import utam.compiler.helpers.ElementContext.Root;
import utam.compiler.helpers.MethodContext;
import utam.compiler.helpers.ParameterUtils.Literal;
import utam.compiler.helpers.ParameterUtils.Regular;
import utam.compiler.helpers.PrimitiveType;
import utam.compiler.helpers.TranslationContext;
import utam.compiler.representation.ComposeMethodStatement;
import utam.compiler.representation.ComposeMethodStatement.Single;
import utam.compiler.representation.RootElementMethod;
import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.TypeProvider;
import utam.core.framework.consumer.UtamError;

public class UtamMethodAction_Tests {

  private static final String ELEMENT_NAME = "testElement";

  private static MethodContext getMethodContext(TypeProvider returns) {
    return new MethodContext("testMethod", returns, false);
  }

  private static MethodContext getVoidMethodContext() {
    return getMethodContext(VOID);
  }

  private static UtamSelector getSelector() {
    return new UtamSelector(".css");
  }

  private static UtamSelector getListSelector() {
    return new UtamSelector(".css", true);
  }

  private static ComposeMethodStatement getVoidStatement(UtamMethodAction action,
      TranslationContext context) {
    return action.getComposeAction(context, getVoidMethodContext(), false);
  }

  private static void setupRoot(TranslationContext context, UtamElement customElement) {
    context.setElement(new Root(TEST_PAGE_OBJECT));
    context.getRootElement().setElementMethod(new RootElementMethod.Protected());
    customElement.traverse(context, context.getRootElement(), false);
  }

  private static String getSingleCodeLine(ComposeMethodStatement statement) {
    assertThat(statement.getCodeLines(), is(hasSize(1)));
    return statement.getCodeLines().get(0);
  }

  /**
   * Adding imperative extension removed some Jackson validations in deserialization
   * This validation is now logic at the start of the getComposeAction method.
   * Test that the element property missing when apply is present throws
   */
  @Test
  public void testGetComposeActionInvalidStatement() {
    TranslationContext context = TestUtilities.getTestTranslationContext();
    new UtamElement(ELEMENT_NAME, "clickable", getSelector()).testTraverse(context);
    UtamMethodAction action =
            new UtamMethodAction(
                    null, ClickableActionType.click.toString());
    UtamError e = expectThrows(
            UtamError.class, () -> action.getComposeAction(context, getMethodContext(VOID), false )
    );
    assertThat(e.getMessage(), is(equalTo("Statements for compose method 'testMethod' should either have 'element' and 'apply' or 'applyExternal' properties")));
  }

  /**
   * Test that getComposeAction throws when applyExternal and apply are set
   */
  @Test
  public void testGetComposeActionInvalidUtilityStatement() {
    TranslationContext context = TestUtilities.getTestTranslationContext();
    new UtamElement(ELEMENT_NAME, "clickable", getSelector()).testTraverse(context);
    UtamMethodAction action =
            new UtamMethodAction(null);
    UtamError e = expectThrows(
            UtamError.class, () -> action.getComposeAction(context, getMethodContext(VOID), false )
    );
    assertThat(e.getMessage(), is(equalTo("Statements for compose method 'testMethod' should either have 'element' and 'apply' or 'applyExternal' properties")));
  }

  /**
   * The getComposeAction method should return the proper value
   */
  @Test
  public void testGetComposeAction() {
    TranslationContext context = TestUtilities.getTestTranslationContext();
    new UtamElement(ELEMENT_NAME, "clickable", getSelector()).testTraverse(context);
    UtamMethodAction action =
        new UtamMethodAction(
            ELEMENT_NAME, ClickableActionType.click.toString());
    ComposeMethodStatement actionObject = getVoidStatement(action, context);
    assertThat(actionObject, is(instanceOf(Single.class)));
    assertThat(actionObject.getReturnType().isSameType(VOID), is(true));
    assertThat(
        getSingleCodeLine(actionObject),
        is(equalTo(getElementPrivateMethodCalled(ELEMENT_NAME) + "().click()")));
  }

  /**
   * The getComposeAction method should return the proper value with a list element and an applied
   * list action that returns void
   */
  @Test
  public void testGetComposeActionWithListElementAndVoidListAction() {
    TranslationContext context = TestUtilities.getTestTranslationContext();
    new UtamElement(ELEMENT_NAME, "clickable", getListSelector())
        .testTraverse(context);
    UtamMethodAction action =
        new UtamMethodAction(
            ELEMENT_NAME, ClickableActionType.click.toString());
    ComposeMethodStatement actionObject = getVoidStatement(action, context);
    assertThat(actionObject, is(instanceOf(ComposeMethodStatement.VoidList.class)));
    assertThat(actionObject.getReturnType().isSameType(VOID), is(true));
    assertThat(
        getSingleCodeLine(actionObject),
        is(
            equalTo(
                getElementPrivateMethodCalled(ELEMENT_NAME)
                    + "().forEach(element -> element.click())")));
  }

  /**
   * The getComposeAction method should return the proper value with a list element and an applied
   * list action that returns a value for each element
   */
  @Test
  public void testGetComposeActionWithListElementAndListAction() {
    TranslationContext context = TestUtilities.getTestTranslationContext();
    new UtamElement(ELEMENT_NAME, "clickable", getListSelector())
        .testTraverse(context);
    UtamMethodAction action =
        new UtamMethodAction(
            ELEMENT_NAME, ActionableActionType.getText.toString());
    ComposeMethodStatement actionObject =
        action.getComposeAction(context, getMethodContext(PrimitiveType.STRING), false);
    assertThat(actionObject, is(instanceOf(ComposeMethodStatement.ReturnsList.class)));
    assertThat(actionObject.getReturnType().getSimpleName(), is(equalTo("List<String>")));
    assertThat(
        getSingleCodeLine(actionObject),
        is(
            equalTo(
                getElementPrivateMethodCalled(ELEMENT_NAME)
                    + "().stream().map(element -> element.getText()).collect(Collectors.toList())")));
  }

  @Test
  public void testGetSizeActionWithListElement() {
    TranslationContext context = TestUtilities.getTestTranslationContext();
    new UtamElement(ELEMENT_NAME, "clickable", getListSelector())
        .testTraverse(context);
    UtamMethodAction action =
        new UtamMethodAction(
            ELEMENT_NAME, ActionableActionType.size.toString());
    ComposeMethodStatement statement = action.getComposeAction(context, getMethodContext(NUMBER), false);
    assertThat(statement, is(instanceOf(Single.class)));
  }

  @Test
  public void testGetComposeActionCustomElement() {
    TranslationContext context = TestUtilities.getTestTranslationContext();
    UtamElement utamElement = new UtamElement(ELEMENT_NAME, TEST_URI, getSelector());
    setupRoot(context, utamElement);
    UtamMethodAction action = new UtamMethodAction(ELEMENT_NAME, "myMethod");
    ComposeMethodStatement actionObject = getVoidStatement(action, context);
    assertThat(actionObject, is(instanceOf(ComposeMethodStatement.Single.class)));
    assertThat(actionObject.getReturnType().isSameType(VOID), is(true));
    assertThat(
        getSingleCodeLine(actionObject),
        is(equalTo(getElementPrivateMethodCalled(ELEMENT_NAME) + "().myMethod()")));
  }

  @Test
  public void testGetComposeActionCustomElementListWithParameters() {
    TranslationContext context = TestUtilities.getTestTranslationContext();
    UtamElement utamElement = new UtamElement(ELEMENT_NAME, TEST_URI, getListSelector());
    setupRoot(context, utamElement);
    UtamMethodAction action = new UtamMethodAction(ELEMENT_NAME, "myMethod");
    action.args = new UtamArgument[]{
        new UtamArgument("strParameter", "string")
    };
    MethodContext methodContext = getMethodContext(PrimitiveType.STRING);
    ComposeMethodStatement actionObject =
        action.getComposeAction(context, methodContext, false);
    assertThat(actionObject, is(instanceOf(ComposeMethodStatement.ReturnsList.class)));
    assertThat(actionObject.getReturnType().getSimpleName(), is(equalTo("List<String>")));
    assertThat(
        getSingleCodeLine(actionObject),
        is(equalTo(getElementPrivateMethodCalled(ELEMENT_NAME)
            + "().stream().map(element -> element.myMethod(strParameter)).collect(Collectors.toList())")));
  }

  @Test
  public void testGetComposeActionUtility() {
    TranslationContext context = TestUtilities.getTestTranslationContext();
    UtamUtilityMethodAction applyExternal = new UtamUtilityMethodAction(
            "utam-test/utils/test/testClassUtility",
            "testStaticMethod",
            null
    );
    UtamMethodAction action = new UtamMethodAction(applyExternal);
    TypeProvider type = context.getUtilityType(applyExternal.getExternalClassPath());
    ComposeMethodStatement actionObject = getVoidStatement(action, context);
    assertThat(actionObject, is(instanceOf(ComposeMethodStatement.Utility.class)));
    assertThat(actionObject.getReturnType().isSameType(VOID), is(true));
    assertThat(
            getSingleCodeLine(actionObject),
            is(equalTo(type.getSimpleName() + ".testStaticMethod(new UtamUtilitiesContext(this))")));
  }

  @Test
  public void testGetComposeActionUtilityWithArgs() {
    TranslationContext context = TestUtilities.getTestTranslationContext();
    UtamUtilityMethodAction applyExternal = new UtamUtilityMethodAction(
            "utam-test/utils/test/testClassUtility",
            "testStaticMethod",
            null
    );
    applyExternal.args = new UtamArgument[]{
            new UtamArgument("strParameter", "string")
    };
    UtamMethodAction action = new UtamMethodAction(applyExternal);
    TypeProvider type = context.getUtilityType(applyExternal.getExternalClassPath());
    ComposeMethodStatement actionObject = getVoidStatement(action, context);
    assertThat(actionObject, is(instanceOf(ComposeMethodStatement.Utility.class)));
    assertThat(actionObject.getReturnType().isSameType(VOID), is(true));
    assertThat(actionObject.getParameters().get(0).getValue(), is(equalTo("strParameter")));
    assertThat(
            getSingleCodeLine(actionObject),
            is(equalTo(type.getSimpleName() + ".testStaticMethod(new UtamUtilitiesContext(this), strParameter)")));
  }

  @Test
  public void testGetComposeActionUtilityWithPrimitiveReturnType() {
    TranslationContext context = TestUtilities.getTestTranslationContext();
    UtamUtilityMethodAction applyExternal = new UtamUtilityMethodAction(
            "utam-test/utils/test/testClassUtility",
            "testStaticMethod",
            null
    );
    UtamMethodAction action = new UtamMethodAction(applyExternal);
    MethodContext methodContext = getMethodContext(PrimitiveType.STRING);
    ComposeMethodStatement actionObject =
            action.getComposeAction(context, methodContext, false);
    assertThat(actionObject, is(instanceOf(ComposeMethodStatement.Utility.class)));
    assertThat(actionObject.getReturnType().isSameType(PrimitiveType.STRING), is(true));
  }

  @Test
  public void testGetParameterTypesForCustomAction() {
    List<MethodParameter> parameters = Stream.of(
        new Literal(true, PrimitiveType.BOOLEAN),
        new Regular("name", SELECTOR)
    ).collect(Collectors.toList());
    assertThat(new Custom("customMethod", VOID, parameters).getParametersTypes(),
        is(containsInAnyOrder(PrimitiveType.BOOLEAN, SELECTOR)));
  }
}
