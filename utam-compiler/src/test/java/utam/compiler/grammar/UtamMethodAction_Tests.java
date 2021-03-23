package utam.compiler.grammar;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.instanceOf;
import static org.hamcrest.Matchers.is;
import static org.testng.Assert.assertThrows;
import static utam.compiler.grammar.TestUtilities.TEST_PAGE_OBJECT;
import static utam.compiler.grammar.TestUtilities.TEST_URI;
import static utam.compiler.grammar.TestUtilities.getElementPrivateMethodCalled;

import org.testng.annotations.Test;
import utam.compiler.grammar.UtamMethodAction.MethodContext;
import utam.compiler.helpers.ActionableActionType;
import utam.compiler.helpers.ClickableActionType;
import utam.compiler.helpers.ElementContext.Root;
import utam.compiler.helpers.PrimitiveType;
import utam.compiler.helpers.TranslationContext;
import utam.compiler.helpers.TypeUtilities;
import utam.compiler.representation.ComposeMethodStatement;
import utam.compiler.representation.ComposeMethodStatement.Single;
import utam.compiler.representation.RootElementMethod;
import utam.core.declarative.representation.TypeProvider;

public class UtamMethodAction_Tests {

  private static final String ELEMENT_NAME = "testElement";

  private static MethodContext getMethodContext(TypeProvider returns) {
    return new MethodContext("testMethod", returns);
  }

  private static MethodContext getVoidMethodContext() {
    return getMethodContext(TypeUtilities.VOID);
  }

  private static UtamSelector getSelector() {
    return new UtamSelector(".css");
  }

  private static UtamSelector getListSelector() {
    return new UtamSelector(".css", true);
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
   * The getComposeAction method should return the proper value
   */
  @Test
  public void testGetComposeAction() {
    TranslationContext context = TestUtilities.getTestTranslationContext();
    new UtamElement(ELEMENT_NAME, "clickable", getSelector()).testTraverse(context);
    UtamMethodAction action =
        new UtamMethodAction(
            ELEMENT_NAME, ClickableActionType.click.toString(), new UtamArgument[]{});
    ComposeMethodStatement actionObject =
        action.getComposeAction(context, getVoidMethodContext(), true);
    assertThat(actionObject, is(instanceOf(Single.class)));
    assertThat(actionObject.getReturnType().getSimpleName(), is(equalTo("void")));
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
            ELEMENT_NAME, ClickableActionType.click.toString(), new UtamArgument[]{});
    ComposeMethodStatement actionObject =
        action.getComposeAction(context, getVoidMethodContext(), true);
    assertThat(actionObject, is(instanceOf(ComposeMethodStatement.VoidList.class)));
    assertThat(actionObject.getReturnType().getSimpleName(), is(equalTo("void")));
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
            ELEMENT_NAME, ActionableActionType.getText.toString(), new UtamArgument[]{});
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
  public void testGetComposeActionWithListElementThrows() {
    TranslationContext context = TestUtilities.getTestTranslationContext();
    new UtamElement(ELEMENT_NAME, "clickable", getListSelector())
        .testTraverse(context);
    UtamMethodAction action =
        new UtamMethodAction(
            ELEMENT_NAME, ActionableActionType.size.toString(), new UtamArgument[]{});
    assertThrows(() -> action.getComposeAction(context, getVoidMethodContext(), true));
  }

  @Test
  public void testGetComposeActionCustomElement() {
    TranslationContext context = TestUtilities.getTestTranslationContext();
    UtamElement utamElement = new UtamElement(ELEMENT_NAME, TEST_URI, getSelector());
    setupRoot(context, utamElement);
    UtamMethodAction action = new UtamMethodAction(ELEMENT_NAME, "myMethod", new UtamArgument[]{});
    ComposeMethodStatement actionObject =
        action.getComposeAction(context, getVoidMethodContext(), true);
    assertThat(actionObject, is(instanceOf(ComposeMethodStatement.Single.class)));
    assertThat(actionObject.getReturnType().getSimpleName(), is(equalTo("void")));
    assertThat(
        getSingleCodeLine(actionObject),
        is(equalTo(getElementPrivateMethodCalled(ELEMENT_NAME) + "().myMethod()")));
  }

  @Test
  public void testGetComposeActionCustomElementListWithParameters() {
    TranslationContext context = TestUtilities.getTestTranslationContext();
    UtamElement utamElement = new UtamElement(ELEMENT_NAME, TEST_URI, getListSelector());
    setupRoot(context, utamElement);
    UtamMethodAction action = new UtamMethodAction(ELEMENT_NAME, "myMethod", new UtamArgument[]{
        new UtamArgument("strParameter", "string")
    });
    ComposeMethodStatement actionObject =
        action.getComposeAction(context, getMethodContext(PrimitiveType.STRING), true);
    assertThat(actionObject, is(instanceOf(ComposeMethodStatement.ReturnsList.class)));
    assertThat(actionObject.getReturnType().getSimpleName(), is(equalTo("List<String>")));
    assertThat(
        getSingleCodeLine(actionObject),
        is(equalTo(getElementPrivateMethodCalled(ELEMENT_NAME)
            + "().stream().map(element -> element.myMethod(strParameter)).collect(Collectors.toList())")));
  }
}
