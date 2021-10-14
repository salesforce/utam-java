/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.helpers;

import java.util.Objects;
import utam.core.framework.consumer.UtamError;
import org.testng.annotations.Test;
import utam.core.element.Actionable;

import java.lang.reflect.Method;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static utam.compiler.grammar.TestUtilities.getCssSelector;
import static utam.compiler.helpers.BasicElementActionTypeTests.sameType;
import static utam.compiler.helpers.ActionableActionType.ERR_NOT_HTML_ELEMENT;
import static utam.compiler.helpers.ActionableActionType.ERR_UNKNOWN_ACTION;
import static utam.compiler.helpers.TypeUtilities.CONTAINER_ELEMENT;
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
    } catch (NoSuchMethodException e) {
      // some methods are declared in parent interface
      Class parent = clazz.getInterfaces().length > 0? clazz.getInterfaces()[0] : null;
      try {
        return parent.getDeclaredMethod(methodName, parameters);
      } catch (Exception eParent) {
        throw new AssertionError(
            String.format("method '%s' with parameters {%s} not found in class %s or its parent %s",
                methodName,
                Stream.of(parameters).filter(Objects::nonNull).map(Class::getSimpleName)
                    .collect(Collectors.joining(",")),
                clazz.getName(), parent.getName()));
      }
    }
  }

  private static ElementContext.Basic getElementContext(TypeUtilities.BasicElementInterface type) {
    return new ElementContext.Basic(ELEMENT_NAME, type, getCssSelector("selector"));
  }

  private static ElementContext.Basic getEditableElementContext() {
    return new ElementContext.Basic(
        ELEMENT_NAME, TypeUtilities.BasicElementInterface.editable, getCssSelector("selector"));
  }

  private static ElementContext.Basic getTouchableElementContext() {
    return new ElementContext.Basic(
        ELEMENT_NAME, TypeUtilities.BasicElementInterface.touchable, getCssSelector("selector"));
  }

  private static void validateAction(ActionType action, String returnType) {
    assertThat(action.getParametersTypes(), is(empty()));
    assertThat(action.getReturnType().getSimpleName(), is(equalTo(returnType)));
  }

  @Test
  public void checkSupportedActions() {
    for (Method method : Actionable.class.getDeclaredMethods()) {
      ActionableActionType.valueOf(method.getName());
    }
  }

  @Test
  public void testScrollToCenter() {
    validateAction(ActionableActionType.scrollToCenter, VOID_TYPE_NAME);
  }

  @Test
  public void testHasFocus() {
    validateAction(BasicElementActionType.isFocused, BOOLEAN_TYPE_NAME);
  }

  /** The focus member should return the proper value */
  @Test
  public void testFocus() {
    validateAction(ActionableActionType.focus, VOID_TYPE_NAME);
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
    validateAction(BasicElementActionType.size, NUMBER_TYPE_NAME);
  }

  @Test
  public void testGetClass() {
    validateAction(BasicElementActionType.getClassAttribute, STRING_TYPE_NAME);
  }

  @Test
  @SuppressWarnings("rawtypes")
  public void testStandardActionsMethods() {
    Stream.of(ActionableActionType.values())
        .forEach(
            action -> {
              info(String.format("test element action '%s'", action));
              Method method =
                  getMethod(
                      action.getElementClass(),
                      action.getApplyString(),
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

  @Test
  public void testGetActionFromStringForActionable() {
    final ActionType waitForAbsence = BasicElementActionType.waitForAbsence;
    ElementContext elementContext = getElementContext(TypeUtilities.BasicElementInterface.actionable);
    assertThat(
        BasicElementActionType.getActionType(
            waitForAbsence.getApplyString(), elementContext.getType(), elementContext.getName()),
        is(equalTo(waitForAbsence)));
    elementContext = getEditableElementContext();
    assertThat(
        BasicElementActionType.getActionType(
            BasicElementActionType.getClassAttribute.getApplyString(),
            elementContext.getType(),
            elementContext.getName()),
        is(equalTo(BasicElementActionType.getClassAttribute)));
    elementContext = getElementContext(TypeUtilities.BasicElementInterface.clickable);
    assertThat(
        BasicElementActionType.getActionType(
            waitForAbsence.getApplyString(), elementContext.getType(), elementContext.getName()),
        is(equalTo(waitForAbsence)));
  }

  @Test
  public void testGetActionFromStringForClickable() {
    final ActionType ACTION = ClickableActionType.click;
    ElementContext elementContext = getElementContext(TypeUtilities.BasicElementInterface.clickable);
    assertThat(
        BasicElementActionType.getActionType(
            ACTION.getApplyString(), elementContext.getType(), elementContext.getName()),
        is(equalTo(ACTION)));
  }

  @Test
  public void testGetActionFromStringForEditable() {
    final ActionType ACTION = EditableActionType.clear;
    ElementContext elementContext = getEditableElementContext();
    assertThat(
        BasicElementActionType.getActionType(
            ACTION.getApplyString(), elementContext.getType(), elementContext.getName()),
        is(equalTo(ACTION)));
  }

  @Test
  public void testGetActionFromStringForTouchable() {
    final ActionType ACTION = TouchableActionType.flick;
    ElementContext elementContext = getTouchableElementContext();
    assertThat(
        BasicElementActionType.getActionType(
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
                BasicElementActionType.getActionType(
                    BasicElementActionType.waitForAbsence.name(),
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
    ElementContext elementContext = getElementContext(TypeUtilities.BasicElementInterface.actionable);
    UtamError e =
        expectThrows(
            UtamError.class,
            () ->
                BasicElementActionType.getActionType(
                    ACTION_NAME, elementContext.getType(), elementContext.getName()));
    assertThat(
        e.getMessage(),
        containsString(
            String.format(
                ERR_UNKNOWN_ACTION,
                ACTION_NAME,
                ELEMENT_NAME,
                "declared interfaces " + TypeUtilities.BasicElementInterface.actionable.name())));
  }

  @Test
  public void testActionFromStringForTouchableWrongActionNameError() {
    final String ACTION_NAME = "error";
    ElementContext elementContext = getElementContext(TypeUtilities.BasicElementInterface.touchable);
    UtamError e =
        expectThrows(
            UtamError.class,
            () ->
                BasicElementActionType.getActionType(
                    ACTION_NAME, elementContext.getType(), elementContext.getName()));
    assertThat(
        e.getMessage(),
        containsString(
            String.format(
                ERR_UNKNOWN_ACTION,
                ACTION_NAME,
                ELEMENT_NAME,
                "declared interfaces " + TypeUtilities.BasicElementInterface.touchable.name())));
  }

  @Test
  public void testActionFromStringWrongActionTypeError() {
    final String ACTION_NAME = ClickableActionType.click.name();
    ElementContext elementContext = getElementContext(TypeUtilities.BasicElementInterface.actionable);
    UtamError e =
        expectThrows(
            UtamError.class,
            () ->
                BasicElementActionType.getActionType(
                    ACTION_NAME, elementContext.getType(), elementContext.getName()));
    assertThat(
        e.getMessage(),
        containsString(
            String.format(
                ERR_UNKNOWN_ACTION,
                ACTION_NAME,
                ELEMENT_NAME,
                "declared interfaces " + TypeUtilities.BasicElementInterface.actionable.name())));
  }
}
