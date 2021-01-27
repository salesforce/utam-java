package declarative.grammar;

import declarative.helpers.ActionableActionType;
import declarative.helpers.ClickableActionType;
import declarative.helpers.TranslationContext;
import declarative.representation.ComposeMethod;
import org.testng.annotations.Test;

import java.util.HashSet;
import java.util.Set;

import static declarative.grammar.TestUtilities.getElementPrivateMethod;
import static declarative.grammar.TestUtilities.getElementPrivateMethodCalled;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;

public class UtamMethodAction_Tests {

  private static final String ELEMENT_NAME = "testElement";

  /** The getComposeAction method should return the proper value */
  @Test
  public void testGetComposeAction() {
    TranslationContext context = TestUtilities.getTestTranslationContext();
    new UtamElement(ELEMENT_NAME, "clickable", new UtamSelector("selector")).testTraverse(context);
    UtamMethodAction action =
        new UtamMethodAction(
            ELEMENT_NAME, ClickableActionType.click.toString(), new UtamArgument[] {});
    Set<String> elementNames = new HashSet<>();
    ComposeMethod.ElementAction actionObject =
        action.getComposeAction(elementNames, context, "testMethod");
    assertThat(actionObject, is(instanceOf(ComposeMethod.ElementAction.class)));
    assertThat(actionObject.getReturnType().getSimpleName(), is(equalTo("void")));
    assertThat(
        actionObject.getCodeLine(),
        is(equalTo(getElementPrivateMethodCalled(ELEMENT_NAME) + "().click()")));
  }

  /**
   * The getComposeAction method should return the proper value with a list element and an applied
   * list action that returns void
   */
  @Test
  public void testGetComposeActionWithListElementAndVoidListAction() {
    TranslationContext context = TestUtilities.getTestTranslationContext();
    new UtamElement(ELEMENT_NAME, "clickable", new UtamSelector("fakeSelector", true))
        .testTraverse(context);
    UtamMethodAction action =
        new UtamMethodAction(
            ELEMENT_NAME, ClickableActionType.click.toString(), new UtamArgument[] {});
    ComposeMethod.ElementAction actionObject =
        action.getComposeAction(new HashSet<>(), context, "testMethod");
    assertThat(actionObject, is(instanceOf(ComposeMethod.VoidListAction.class)));
    assertThat(actionObject.getReturnType().getSimpleName(), is(equalTo("void")));
    assertThat(
        actionObject.getCodeLine(),
        is(
            equalTo(
                getElementPrivateMethodCalled(ELEMENT_NAME) + "().forEach(element -> element.click())")));
  }

  /**
   * The getComposeAction method should return the proper value with a list element and an applied
   * list action that returns a value for each element
   */
  @Test
  public void testGetComposeActionWithListElementAndListAction() {
    TranslationContext context = TestUtilities.getTestTranslationContext();
    new UtamElement(ELEMENT_NAME, "clickable", new UtamSelector("fakeSelector", true))
        .testTraverse(context);
    UtamMethodAction action =
        new UtamMethodAction(
            ELEMENT_NAME, ActionableActionType.getText.toString(), new UtamArgument[] {});
    Set<String> elementNames = new HashSet<>();
    ComposeMethod.ElementAction actionObject =
        action.getComposeAction(elementNames, context, "testMethod");
    assertThat(actionObject, is(instanceOf(ComposeMethod.ElementAction.class)));
    assertThat(actionObject.getReturnType().getSimpleName(), is(equalTo("List<String>")));
    assertThat(
        actionObject.getCodeLine(),
        is(
            equalTo(
                getElementPrivateMethodCalled(ELEMENT_NAME)
                    + "().stream().map(element -> element.getText()).collect(Collectors.toList())")));
  }

  /**
   * The getComposeAction method should return the proper value with a list element and an applied
   * list action that applies to the list as a whole
   */
  @Test
  public void testGetComposeActionWithListElementAndWholeListAction() {
    TranslationContext context = TestUtilities.getTestTranslationContext();
    new UtamElement(ELEMENT_NAME, "clickable", new UtamSelector("fakeSelector"))
        .testTraverse(context);
    UtamMethodAction action =
        new UtamMethodAction(
            ELEMENT_NAME, ActionableActionType.size.toString(), new UtamArgument[] {});
    Set<String> elementNames = new HashSet<>();
    ComposeMethod.ElementAction actionObject =
        action.getComposeAction(elementNames, context, "testMethod");
    assertThat(actionObject, is(instanceOf(ComposeMethod.ElementAction.class)));
    assertThat(actionObject.getReturnType().getSimpleName(), is(equalTo("Integer")));
    assertThat(
        actionObject.getCodeLine(),
        is(equalTo(getElementPrivateMethodCalled(ELEMENT_NAME) + "().size()")));
  }
}
