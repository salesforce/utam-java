package utam.compiler.helpers;

import utam.core.declarative.representation.TypeProvider;
import utam.core.framework.consumer.ContainerElement;
import utam.core.framework.consumer.UtamError;
import org.testng.annotations.Test;
import utam.core.selenium.element.Actionable;
import utam.core.selenium.expectations.DriverExpectationsUtil;

import java.lang.reflect.Method;
import java.lang.reflect.ParameterizedType;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.function.Consumer;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static utam.compiler.grammar.TestUtilities.getCssSelector;
import static utam.compiler.helpers.ActionableActionType.ERR_NOT_HTML_ELEMENT;
import static utam.compiler.helpers.ActionableActionType.ERR_UNKNOWN_ACTION;
import static utam.core.framework.UtamLogger.info;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;
import static org.testng.Assert.expectThrows;

/**
 * Provides tests for the TranslatableAction enum
 *
 * @author james.evans
 */
public class ActionableActionTypeTests {

  private static final String BOOLEAN_TYPE_NAME = "Boolean";
  private static final String VOID_TYPE_NAME = "void";
  private static final String NUMBER_TYPE_NAME = "Integer";
  private static final String STRING_TYPE_NAME = "String";
  private static final String ELEMENT_NAME = "name";

  @SuppressWarnings({"unchecked", "rawtypes"})
  private static Method getMethod(Class clazz, String methodName, Class[] parameters) {
    try {
      return clazz.getDeclaredMethod(methodName, parameters);
    } catch (Exception e) {
      throw new AssertionError(
          String.format("method '%s' not found in class %s", methodName, clazz.getName()), e);
    }
  }

  private static ElementContext.Basic getElementContext(TypeUtilities.Element type) {
    return new ElementContext.Basic(ELEMENT_NAME, type.getType(), getCssSelector("selector"));
  }

  private static ElementContext.Basic getEditableElementContext() {
    return new ElementContext.Basic(
        ELEMENT_NAME, TypeUtilities.Element.editable.getType(), getCssSelector("selector"));
  }

  private static void validateAction(ActionType action, String returnType, boolean isListAction) {
    assertThat(action.getParametersTypes(), is(empty()));
    assertThat(action.getReturnType().getSimpleName(), is(equalTo(returnType)));
    assertThat(action.isListAction(), is(equalTo(isListAction)));
  }

  private static void validateParameterizedAction(
      ActionType action, String returnType, List<String> parameterTypes) {
    Set<String> parameterTypeStrings =
        action.getParametersTypes().stream()
            .filter((type) -> !type.getSimpleName().isEmpty())
            .map(TypeProvider::getSimpleName)
            .collect(Collectors.toSet());

    assertThat(parameterTypeStrings, containsInAnyOrder(parameterTypes.toArray()));
    assertThat(parameterTypeStrings, hasSize(parameterTypes.size()));
    assertThat(action.getReturnType().getSimpleName(), is(equalTo(returnType)));
    assertThat(action.isListAction(), is(equalTo(false)));
  }

  @Test
  public void checkSupportedActions() {
    for (Method method : Actionable.class.getDeclaredMethods()) {
      // skip scrollTo for now
      if (method.getName().equals("scrollTo")) {
        continue;
      }
      if (method.getName().equals(ActionableActionType.getClass.getInvokeMethodName())) {
        continue;
      }
      checkTranslatorValue(method, ActionableActionType::valueOf);
    }
  }

  private void checkTranslatorValue(Method method, Consumer<String> consumer) {
    consumer.accept(method.getName());
  }

  @Test
  public void testScrollToCenter() {
    validateAction(ActionableActionType.scrollToCenter, VOID_TYPE_NAME, false);
  }

  @Test
  public void testHasFocus() {
    validateAction(ActionableActionType.isFocused, BOOLEAN_TYPE_NAME, false);
  }

  /** The absence member should return the proper value */
  @Test
  public void testAbsence() {
    validateAction(ActionableActionType.waitForAbsence, VOID_TYPE_NAME, true);
  }

  /** The visibility member should return the proper value */
  @Test
  public void testVisibility() {
    validateAction(ActionableActionType.waitForVisible, VOID_TYPE_NAME, true);
  }

  /** The focus member should return the proper value */
  @Test
  public void testFocus() {
    validateAction(ActionableActionType.focus, VOID_TYPE_NAME, false);
  }

  /** The getAttribute member should return the proper value */
  @Test
  public void testGetAttribute() {
    validateParameterizedAction(
        ActionableActionType.getAttribute,
        STRING_TYPE_NAME,
        Collections.singletonList(STRING_TYPE_NAME));
  }

  /** The getText member should return the proper value */
  @Test
  public void testGetText() {
    validateAction(ActionableActionType.getText, STRING_TYPE_NAME, false);
  }

  /** The getTitle member should return the proper value */
  @Test
  public void testGetTitle() {
    validateAction(ActionableActionType.getTitle, STRING_TYPE_NAME, false);
  }

  /** The invisibility member should return the proper value */
  @Test
  public void testInvisibility() {
    validateAction(ActionableActionType.waitForInvisible, VOID_TYPE_NAME, true);
  }

  /** The isDisplayed member should return the proper value */
  @Test
  public void testIsDisplayed() {
    validateAction(ActionableActionType.isVisible, BOOLEAN_TYPE_NAME, true);
  }

  /** The isPresent member should return the proper value */
  @Test
  public void testIsPresent() {
    validateAction(ActionableActionType.isPresent, BOOLEAN_TYPE_NAME, true);
  }

  /** The moveTo member should return the proper value */
  @Test
  public void testMoveTo() {
    validateAction(ActionableActionType.moveTo, VOID_TYPE_NAME, false);
  }

  /** The scrollTo member should return the proper value */
  @Test
  public void testScrollTo() {
    validateAction(ActionableActionType.scrollToTop, VOID_TYPE_NAME, false);
  }

  /** The blur member should return the proper value */
  @Test
  public void testBlur() {
    validateAction(ActionableActionType.blur, VOID_TYPE_NAME, false);
  }

  /** The size member should return the proper value */
  @Test
  public void testSize() {
    validateAction(ActionableActionType.size, NUMBER_TYPE_NAME, true);
  }

  @Test
  public void testGetClass() {
    validateAction(ActionableActionType.getClass, STRING_TYPE_NAME, false);
  }

  @SuppressWarnings("rawtypes")
  @Test
  public void driverActions() {
    for (DriverExpectationsUtil.Type action : DriverExpectationsUtil.Type.values()) {
      Method method =
          getMethod(DriverExpectationsUtil.class, action.name(), action.getParameterTypes());
      // method returns expectations, so we need generic type parameter
      Class expectationReturns =
          (Class) ((ParameterizedType) method.getGenericReturnType()).getActualTypeArguments()[0];
      // check that code is set
      // N.B., currently, all actions return a valid type; if an action is added that
      // will return null, something similar to the following will be required:
      // Class expected = action.getReturnType() == null ? WebElement.class :
      // action.getReturnType();
      Class expected = action.getReturnType();
      assertThat(expected, is(equalTo(expectationReturns)));
    }
  }

  @Test
  @SuppressWarnings("rawtypes")
  public void testStandardActionsMethods() {
    Stream.of(ActionableActionType.values())
        .filter(ActionableActionType::hasMethodToTest)
        .forEach(
            action -> {
              info(String.format("test element action '%s'", action));
              Method method =
                  getMethod(
                      action.getElementClass(),
                      action.getInvokeMethodName(),
                      action.getParameterClasses());
              assertThat(
                  String.format(
                      "action '%s' returns '%s', method returns '%s'",
                      action, action.getReturnType().name(), method.getReturnType().getName()),
                  action.getReturnType().equals(method.getReturnType()),
                  is(true));
              Class[] params = action.getParameterClasses();
              assertThat(
                  String.format("number of actual parameters is %d", params.length),
                  params.length,
                  is(equalTo(method.getParameterCount())));
              for (int i = 0; i < params.length; i++) {
                assertThat(params[i], is(equalTo(method.getParameterTypes()[i])));
              }
            });
  }

  @Test
  public void testGetActionFromStringForActionable() {
    final ActionType waitForAbsence = ActionableActionType.waitForAbsence;
    ElementContext elementContext = getElementContext(TypeUtilities.Element.actionable);
    assertThat(
        ActionableActionType.getActionType(
            waitForAbsence.getApplyString(), elementContext.getType(), elementContext.getName()),
        is(equalTo(waitForAbsence)));
    elementContext = getEditableElementContext();
    assertThat(
        ActionableActionType.getActionType(
            ActionableActionType.getClass.getApplyString(),
            elementContext.getType(),
            elementContext.getName()),
        is(equalTo(ActionableActionType.getClass)));
    elementContext = getElementContext(TypeUtilities.Element.clickable);
    assertThat(
        ActionableActionType.getActionType(
            waitForAbsence.getApplyString(), elementContext.getType(), elementContext.getName()),
        is(equalTo(waitForAbsence)));
  }

  @Test
  public void testGetActionFromStringForClickable() {
    final ActionType ACTION = ClickableActionType.click;
    ElementContext elementContext = getEditableElementContext();
    assertThat(
        ActionableActionType.getActionType(
            ACTION.getApplyString(), elementContext.getType(), elementContext.getName()),
        is(equalTo(ACTION)));
    elementContext = getElementContext(TypeUtilities.Element.clickable);
    assertThat(
        ActionableActionType.getActionType(
            ACTION.getApplyString(), elementContext.getType(), elementContext.getName()),
        is(equalTo(ACTION)));
  }

  @Test
  public void testGetActionFromStringForEditable() {
    final ActionType ACTION = EditableActionType.clear;
    ElementContext elementContext = getEditableElementContext();
    assertThat(
        ActionableActionType.getActionType(
            ACTION.getApplyString(), elementContext.getType(), elementContext.getName()),
        is(equalTo(ACTION)));
  }

  @Test
  public void testActionFromStringWrongTypeError() {
    ElementContext elementContext = new ElementContext.Container(ELEMENT_NAME);
    UtamError e =
        expectThrows(
            UtamError.class,
            () ->
                ActionableActionType.getActionType(
                    ActionableActionType.waitForAbsence.name(),
                    elementContext.getType(),
                    elementContext.getName()));
    assertThat(
        e.getMessage(),
        containsString(
            String.format(ERR_NOT_HTML_ELEMENT, ELEMENT_NAME, ContainerElement.class.getName())));
  }

  @Test
  public void testActionFromStringWrongActionNameError() {
    final String ACTION_NAME = "error";
    ElementContext elementContext = getElementContext(TypeUtilities.Element.actionable);
    UtamError e =
        expectThrows(
            UtamError.class,
            () ->
                ActionableActionType.getActionType(
                    ACTION_NAME, elementContext.getType(), elementContext.getName()));
    assertThat(
        e.getMessage(),
        containsString(
            String.format(
                ERR_UNKNOWN_ACTION,
                ACTION_NAME,
                TypeUtilities.Element.actionable.name(),
                ELEMENT_NAME)));
  }

  @Test
  public void testActionFromStringWrongActionTypeError() {
    final String ACTION_NAME = ClickableActionType.click.name();
    ElementContext elementContext = getElementContext(TypeUtilities.Element.actionable);
    UtamError e =
        expectThrows(
            UtamError.class,
            () ->
                ActionableActionType.getActionType(
                    ACTION_NAME, elementContext.getType(), elementContext.getName()));
    assertThat(
        e.getMessage(),
        containsString(
            String.format(
                ERR_UNKNOWN_ACTION,
                ACTION_NAME,
                TypeUtilities.Element.actionable.name(),
                ELEMENT_NAME)));
  }
}
