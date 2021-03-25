package utam.compiler.representation;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.collection.IsCollectionWithSize.hasSize;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static utam.compiler.grammar.TestUtilities.getElementPrivateMethodCalled;
import static utam.compiler.grammar.TestUtilities.getTestTranslationContext;
import static utam.compiler.helpers.ParameterUtils.EMPTY_PARAMETERS;
import static utam.compiler.helpers.TypeUtilities.Element.actionable;
import static utam.compiler.translator.TranslationUtilities.EMPTY_COMMENTS;

import java.util.Collections;
import org.testng.annotations.Test;
import utam.compiler.grammar.UtamElement;
import utam.compiler.grammar.UtamSelector;
import utam.compiler.helpers.ActionType;
import utam.compiler.helpers.ActionableActionType;
import utam.compiler.helpers.ElementContext;
import utam.compiler.helpers.MethodContext;
import utam.compiler.helpers.ParameterUtils.Primitive;
import utam.compiler.helpers.PrimitiveType;
import utam.compiler.helpers.TranslationContext;
import utam.compiler.helpers.TypeUtilities;
import utam.compiler.representation.ComposeMethodStatement.BasicElementOperation;
import utam.compiler.representation.ComposeMethodStatement.Operand;
import utam.compiler.representation.ComposeMethodStatement.Operation;
import utam.compiler.representation.ComposeMethodStatement.ReturnsList;
import utam.compiler.representation.ComposeMethodStatement.Single;
import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.TypeProvider;
import utam.core.selenium.element.Actionable;

/**
 * Provides tests for the ComposeMethod class
 *
 * @author james.evans
 */
public class ComposeMethodTests {

  private static final String METHOD_NAME = "test";
  private static final String ELEMENT_NAME = "fakeElementName";

  private static MethodContext getMethodContext() {
    return new MethodContext(METHOD_NAME, null, false);
  }

  private static ComposeMethod getComposeMethod(ComposeMethodStatement statement) {
    // used in tests
    return new ComposeMethod(
        getMethodContext(),
        Collections.singletonList(statement),
        EMPTY_PARAMETERS,
        EMPTY_COMMENTS);
  }

  private static Operation getBasicElementOperation(ActionType actionType) {
    return new BasicElementOperation(actionType, Collections.EMPTY_LIST, false);
  }

  private static Operation getBasicElementOperation(ActionType actionType,
      MethodParameter parameter) {
    return new BasicElementOperation(actionType, Collections.singletonList(parameter), false);
  }

  @Test
  public void testComposeMethodCreation() {
    PageObjectValidationTestHelper.MethodInfo info =
        new PageObjectValidationTestHelper.MethodInfo(METHOD_NAME, "FakeElementType");
    info.addCodeLine("return this.fakeElement");

    TypeProvider fakeReturnType =
        new TypeUtilities.FromString("FakeElementType", "test.FakeElementType");
    ComposeMethodStatement methodAction = mock(ComposeMethodStatement.class);
    when(methodAction.getCodeLines())
        .thenReturn(Collections.singletonList("return this.fakeElement"));
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
    ElementContext element = new ElementContext.Root(elementType, actionable, null);
    element.setElementMethod(new RootElementMethod.Public(actionable));
    ComposeMethodStatement action =
        new Single(new Operand(element),
            getBasicElementOperation(ActionableActionType.focus));
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
    ComposeMethodStatement action =
        new ComposeMethodStatement.VoidList(new Operand(element),
            getBasicElementOperation(ActionableActionType.focus));
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
    ComposeMethodStatement action =
        new Single(new Operand(element),
            getBasicElementOperation(ActionableActionType.size));
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
    ComposeMethodStatement action =
        new ReturnsList(
            new Operand(element),
            getBasicElementOperation(ActionableActionType.getText));
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
    ComposeMethodStatement action = new Single(new Operand(element),
        getBasicElementOperation(ActionableActionType.getText));
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

    MethodParameter parameter = new Primitive("paramName", PrimitiveType.STRING);

    TranslationContext context = getTestTranslationContext();
    new UtamElement(ELEMENT_NAME, new UtamSelector("css")).testTraverse(context);
    ElementContext element = context.getElement(ELEMENT_NAME);

    ComposeMethodStatement action =
        new Single(
            new Operand(element),
            getBasicElementOperation(ActionableActionType.getText, parameter));
    ComposeMethod method =
        new ComposeMethod(
            getMethodContext(), Collections.singletonList(action),
            action.getParameters(), EMPTY_COMMENTS);
    PageObjectValidationTestHelper.validateMethod(method, info);
  }

  @Test
  public void testComposeMethodWithListAndParameter() {
    TranslationContext context = getTestTranslationContext();
    UtamElement utamElement = new UtamElement(ELEMENT_NAME, new UtamSelector("css", true));
    utamElement.testTraverse(context);
    ElementContext element = context.getElement(ELEMENT_NAME);
    PageObjectValidationTestHelper.MethodInfo info =
        new PageObjectValidationTestHelper.MethodInfo(METHOD_NAME, "List<String>");
    info.addCodeLine(getElementPrivateMethodCalled(ELEMENT_NAME)
        + "().stream().map(element -> element.getText(paramName)).collect(Collectors.toList())");
    info.addParameter(
        new PageObjectValidationTestHelper.MethodParameterInfo("paramName", "String"));

    MethodParameter parameter = new Primitive("paramName", PrimitiveType.STRING);
    ComposeMethodStatement action = new ComposeMethodStatement.ReturnsList(
        new Operand(element),
        getBasicElementOperation(ActionableActionType.getText, parameter));
    ComposeMethod method =
        new ComposeMethod(
            getMethodContext(), Collections.singletonList(action),
            action.getParameters(), EMPTY_COMMENTS);
    PageObjectValidationTestHelper.validateMethod(method, info);
  }
}
