package utam.compiler.representation;

import declarative.representation.MethodParameter;
import declarative.representation.TypeProvider;
import utam.compiler.grammar.UtamElement;
import utam.compiler.grammar.UtamSelector;
import utam.compiler.helpers.ActionableActionType;
import utam.compiler.helpers.ElementContext;
import utam.compiler.helpers.TranslationContext;
import utam.compiler.helpers.TypeUtilities;
import framework.consumer.UtamError;
import org.testng.annotations.Test;
import selenium.element.Actionable;

import java.util.Collections;

import static utam.compiler.grammar.TestUtilities.getElementPrivateMethodCalled;
import static utam.compiler.grammar.TestUtilities.getTestTranslationContext;
import static utam.compiler.helpers.ParameterUtils.EMPTY_PARAMETERS;
import static utam.compiler.helpers.TypeUtilities.Element.actionable;
import static utam.compiler.translator.TranslationUtilities.EMPTY_COMMENTS;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.collection.IsCollectionWithSize.hasSize;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.testng.Assert.expectThrows;

/**
 * Provides tests for the ComposeMethod class
 *
 * @author james.evans
 */
public class ComposeMethodTests {

  private static final String METHOD_NAME = "fakeMethodName";
  private static final String ELEMENT_NAME = "fakeElementName";

  private static ComposeMethod getComposeMethod(ComposeMethod.ElementAction statement) {
    // used in tests
    return new ComposeMethod(
        METHOD_NAME, Collections.singletonList(statement), EMPTY_PARAMETERS, EMPTY_COMMENTS);
  }

  @Test
  public void testComposeMethodCreation() {
    PageObjectValidationTestHelper.MethodInfo info =
        new PageObjectValidationTestHelper.MethodInfo(METHOD_NAME, "FakeElementType");
    info.addCodeLine("return this.fakeElement");

    TypeProvider fakeReturnType =
        new TypeUtilities.FromString("FakeElementType", "test.FakeElementType");
    ComposeMethod.ElementAction methodAction = mock(ComposeMethod.ElementAction.class);
    when(methodAction.getCodeLine()).thenReturn("return this.fakeElement");
    when(methodAction.getReturnType()).thenReturn(fakeReturnType);
    when(methodAction.getImports()).thenReturn(Collections.singletonList(fakeReturnType));

    ComposeMethod method = getComposeMethod(methodAction);
    PageObjectValidationTestHelper.validateMethod(method, info);
    assertThat(method.getClassImports(), hasSize(0));
  }

  @Test
  public void testComposeMethodWithVoidRootStatement() {
    PageObjectValidationTestHelper.MethodInfo info =
        new PageObjectValidationTestHelper.MethodInfo(METHOD_NAME, "void");
    info.addCodeLine("this.getRoot().focus()");

    TypeProvider elementType = new TypeUtilities.FromClass(Actionable.class);
    TranslationContext context = getTestTranslationContext();
    ElementContext element = new ElementContext.Root(elementType, actionable.getType(), null);
    element.setElementMethod(new RootElementMethod.Public(actionable.getType()));
    ComposeMethod.ElementAction action =
        new ComposeMethod.ElementAction(
            Collections.EMPTY_SET,
            element,
            ActionableActionType.focus,
            EMPTY_PARAMETERS);
    ComposeMethod method = getComposeMethod(action);
    PageObjectValidationTestHelper.validateMethod(method, info);
  }

  @Test
  public void testComposeMethodWithVoidListStatement() {
    PageObjectValidationTestHelper.MethodInfo info =
        new PageObjectValidationTestHelper.MethodInfo(METHOD_NAME, "void");
    info.addCodeLine(
        getElementPrivateMethodCalled(ELEMENT_NAME) + "().forEach(element -> element.focus())");
    TranslationContext context = getTestTranslationContext();
    new UtamElement(ELEMENT_NAME, new UtamSelector("css")).testTraverse(context);
    ElementContext element = context.getElement(ELEMENT_NAME);
    ComposeMethod.ElementAction action =
        new ComposeMethod.VoidListAction(Collections.EMPTY_SET, element, ActionableActionType.focus, EMPTY_PARAMETERS);
    ComposeMethod method = getComposeMethod(action);
    PageObjectValidationTestHelper.validateMethod(method, info);
  }

  @Test
  public void testComposeMethodWithSimpleListStatement() {
    PageObjectValidationTestHelper.MethodInfo info =
        new PageObjectValidationTestHelper.MethodInfo(METHOD_NAME, "Integer");
    info.addCodeLine(getElementPrivateMethodCalled(ELEMENT_NAME) + "().size()");
    TranslationContext context = getTestTranslationContext();
    new UtamElement(ELEMENT_NAME, new UtamSelector("css")).testTraverse(context);
    ElementContext element = context.getElement(ELEMENT_NAME);
    ComposeMethod.ElementAction action =
        new ComposeMethod.SimpleListAction(Collections.EMPTY_SET, element, ActionableActionType.size, EMPTY_PARAMETERS);
    ComposeMethod method = getComposeMethod(action);
    PageObjectValidationTestHelper.validateMethod(method, info);
  }

  @Test
  public void testComposeMethodWithListStatement() {
    PageObjectValidationTestHelper.MethodInfo info =
        new PageObjectValidationTestHelper.MethodInfo(METHOD_NAME, "List<String>");
    info.addCodeLine(
        getElementPrivateMethodCalled(ELEMENT_NAME)
            + "().stream().map(element -> element.getText()).collect(Collectors.toList())");
    TranslationContext context = getTestTranslationContext();
    new UtamElement(ELEMENT_NAME, new UtamSelector("css")).testTraverse(context);
    ElementContext element = context.getElement(ELEMENT_NAME);
    ComposeMethod.ElementAction action =
        new ComposeMethod.ListAction(
            Collections.EMPTY_SET,
            element,
            ActionableActionType.getText,
            EMPTY_PARAMETERS);
    ComposeMethod method = getComposeMethod(action);
    PageObjectValidationTestHelper.validateMethod(method, info);
  }

  @Test
  public void testComposeMethodWithElementStatement() {
    PageObjectValidationTestHelper.MethodInfo info =
        new PageObjectValidationTestHelper.MethodInfo(METHOD_NAME, "String");
    info.addCodeLine(getElementPrivateMethodCalled(ELEMENT_NAME) + "().getText()");
    TranslationContext context = getTestTranslationContext();
    new UtamElement(ELEMENT_NAME, new UtamSelector("css")).testTraverse(context);
    ElementContext element = context.getElement(ELEMENT_NAME);
    ComposeMethod.ElementAction action =
        new ComposeMethod.ElementAction(
            Collections.EMPTY_SET,
            element,
            ActionableActionType.getText,
            EMPTY_PARAMETERS);
    ComposeMethod method = getComposeMethod(action);
    PageObjectValidationTestHelper.validateMethod(method, info);
  }

  @Test
  public void testComposeMethodWithElementStatementWithParameters() {
    PageObjectValidationTestHelper.MethodInfo info =
        new PageObjectValidationTestHelper.MethodInfo(METHOD_NAME, "String");
    info.addCodeLine(getElementPrivateMethodCalled(ELEMENT_NAME) + "().getText(paramName)");
    info.addParameter(
        new PageObjectValidationTestHelper.MethodParameterInfo("paramName", "String"));

    TypeProvider paramType = new TypeUtilities.FromString("String", "String");
    MethodParameter parameter = mock(MethodParameter.class);
    when(parameter.getValue()).thenReturn("paramName");
    when(parameter.getType()).thenReturn(paramType);

    TranslationContext context = getTestTranslationContext();
    new UtamElement(ELEMENT_NAME, new UtamSelector("css")).testTraverse(context);
    ElementContext element = context.getElement(ELEMENT_NAME);

    ComposeMethod.ElementAction action =
        new ComposeMethod.ElementAction(
            Collections.EMPTY_SET,
            element,
            ActionableActionType.getText,
            Collections.singletonList(parameter));
    ComposeMethod method =
        new ComposeMethod(
            METHOD_NAME, Collections.singletonList(action), action.getParameters(), EMPTY_COMMENTS);
    PageObjectValidationTestHelper.validateMethod(method, info);
  }

  @Test
  public void testComposeMethodWithListStatementWithParametersThrows() {
    TranslationContext context = getTestTranslationContext();
    new UtamElement(ELEMENT_NAME, new UtamSelector("css")).testTraverse(context);
    ElementContext element = context.getElement(ELEMENT_NAME);

    MethodParameter parameter = mock(MethodParameter.class);
    UtamError e =
        expectThrows(
            UtamError.class,
            () ->
                new ComposeMethod.SimpleListAction(
                    Collections.EMPTY_SET,
                    element,
                    ActionableActionType.size,
                    Collections.singletonList(parameter)));
    assertThat(e.getMessage(), is(equalTo("parameters are not supported for list action")));
  }
}
