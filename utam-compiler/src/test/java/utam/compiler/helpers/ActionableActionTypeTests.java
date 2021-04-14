/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.helpers;

import java.util.Objects;
import utam.core.declarative.representation.TypeProvider;
import utam.core.framework.consumer.UtamError;
import org.testng.annotations.Test;
import utam.core.element.Actionable;

import java.lang.reflect.Method;
import java.util.Set;
import java.util.function.Consumer;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static utam.compiler.grammar.TestUtilities.getCssSelector;
import static utam.compiler.helpers.ActionableActionType.ERR_NOT_HTML_ELEMENT;
import static utam.compiler.helpers.ActionableActionType.ERR_UNKNOWN_ACTION;
import static utam.compiler.helpers.TypeUtilities.CONTAINER_ELEMENT;
import static utam.compiler.helpers.TypeUtilities.GENERIC_TYPE;
import static utam.compiler.helpers.TypeUtilities.VOID;
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
          String.format("method '%s' with parameters {%s} not found in class %s",
              methodName,
              Stream.of(parameters).filter(Objects::nonNull).map(Class::getSimpleName).collect(Collectors.joining(",")),
              clazz.getName()), e);
    }
  }

  private static ElementContext.Basic getElementContext(TypeUtilities.Element type) {
    return new ElementContext.Basic(ELEMENT_NAME, type, getCssSelector("selector"));
  }

  private static ElementContext.Basic getEditableElementContext() {
    return new ElementContext.Basic(
        ELEMENT_NAME, TypeUtilities.Element.editable, getCssSelector("selector"));
  }

  private static ElementContext.Basic getTouchableElementContext() {
    return new ElementContext.Basic(
        ELEMENT_NAME, TypeUtilities.Element.touchable, getCssSelector("selector"));
  }

  private static void validateAction(ActionType action, String returnType) {
    assertThat(action.getParametersTypes(), is(empty()));
    assertThat(action.getReturnType().getSimpleName(), is(equalTo(returnType)));
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
    validateAction(ActionableActionType.scrollToCenter, VOID_TYPE_NAME);
  }

  @Test
  public void testHasFocus() {
    validateAction(ActionableActionType.isFocused, BOOLEAN_TYPE_NAME);
  }

  /** The absence member should return the proper value */
  @Test
  public void testAbsence() {
    validateAction(ActionableActionType.waitForAbsence, VOID_TYPE_NAME);
  }

  /** The visibility member should return the proper value */
  @Test
  public void testVisibility() {
    validateAction(ActionableActionType.waitForVisible, VOID_TYPE_NAME);
  }

  /** The focus member should return the proper value */
  @Test
  public void testFocus() {
    validateAction(ActionableActionType.focus, VOID_TYPE_NAME);
  }

  /** The getAttribute member should return the proper value */
  @Test
  public void testGetAttribute() {
    ActionType action = ActionableActionType.getAttribute;
    Set<String> parameterTypeStrings =
        action.getParametersTypes().stream()
            .filter((type) -> !type.getSimpleName().isEmpty())
            .map(TypeProvider::getSimpleName)
            .collect(Collectors.toSet());

    assertThat(parameterTypeStrings, hasSize(1));
    assertThat(parameterTypeStrings.iterator().next(), is(equalTo(STRING_TYPE_NAME)));
    assertThat(action.getReturnType().getSimpleName(), is(equalTo(STRING_TYPE_NAME)));
  }

  /** The getText member should return the proper value */
  @Test
  public void testGetText() {
    validateAction(ActionableActionType.getText, STRING_TYPE_NAME);
  }

  /** The getTitle member should return the proper value */
  @Test
  public void testGetTitle() {
    validateAction(ActionableActionType.getTitle, STRING_TYPE_NAME);
  }

  /** The invisibility member should return the proper value */
  @Test
  public void testInvisibility() {
    validateAction(ActionableActionType.waitForInvisible, VOID_TYPE_NAME);
  }

  /** The isDisplayed member should return the proper value */
  @Test
  public void testIsDisplayed() {
    validateAction(ActionableActionType.isVisible, BOOLEAN_TYPE_NAME);
  }

  /** The isPresent member should return the proper value */
  @Test
  public void testIsPresent() {
    validateAction(ActionableActionType.isPresent, BOOLEAN_TYPE_NAME);
  }

  /** The moveTo member should return the proper value */
  @Test
  public void testMoveTo() {
    validateAction(ActionableActionType.moveTo, VOID_TYPE_NAME);
  }

  /** The scrollTo member should return the proper value */
  @Test
  public void testScrollTo() {
    validateAction(ActionableActionType.scrollToTop, VOID_TYPE_NAME);
  }

  /** The blur member should return the proper value */
  @Test
  public void testBlur() {
    validateAction(ActionableActionType.blur, VOID_TYPE_NAME);
  }

  /** The size member should return the proper value */
  @Test
  public void testSize() {
    validateAction(ActionableActionType.size, NUMBER_TYPE_NAME);
  }

  @Test
  public void testGetClass() {
    validateAction(ActionableActionType.getClass, STRING_TYPE_NAME);
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
                      action, action.getReturnType().getSimpleName(), method.getReturnType().getName()),
                  sameType(action.getReturnType(), method.getReturnType()),
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

  static boolean sameType(TypeProvider actual, Class expected) {
    if(actual.isSameType(VOID)) {
      return expected.getName().toLowerCase().contains("void");
    }
    if(actual.isSameType(GENERIC_TYPE)) {
      return expected.equals(Object.class);
    }
    if(expected.equals(actual.getClassType())) {
      return true;
    }
    if(actual == PrimitiveType.NUMBER) {
      return expected.getName().toLowerCase().startsWith("int");
    }
    if(actual == PrimitiveType.BOOLEAN) {
      return expected.getName().equalsIgnoreCase(actual.getSimpleName());
    }
    return false;
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
  public void testGetActionFromStringForTouchable() {
    final ActionType ACTION = TouchableActionType.flick;
    ElementContext elementContext = getTouchableElementContext();
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
            String.format(ERR_NOT_HTML_ELEMENT, ELEMENT_NAME, CONTAINER_ELEMENT.getSimpleName())));
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
  public void testActionFromStringForTouchableWrongActionNameError() {
    final String ACTION_NAME = "error";
    ElementContext elementContext = getElementContext(TypeUtilities.Element.touchable);
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
                TypeUtilities.Element.touchable.name(),
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
