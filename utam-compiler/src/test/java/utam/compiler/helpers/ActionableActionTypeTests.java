/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.helpers;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.empty;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static utam.compiler.grammar.TestUtilities.getCssSelector;
import static utam.compiler.helpers.BasicElementActionType.getClassAttribute;
import static utam.compiler.helpers.BasicElementActionTypeTests.sameType;
import static utam.compiler.types.BasicElementUnionType.asBasicOrUnionType;
import static utam.core.framework.UtamLogger.info;

import java.lang.reflect.Method;
import java.util.Objects;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.testng.annotations.Test;
import utam.compiler.types.BasicElementInterface;
import utam.core.declarative.representation.TypeProvider;
import utam.core.element.Actionable;

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
      Class parent = clazz.getInterfaces().length > 0 ? clazz.getInterfaces()[0] : null;
      try {
        return parent.getDeclaredMethod(methodName, parameters);
      } catch (Exception eParent) {
        throw new AssertionError(
            String.format(
                "method '%s' with parameters {%s} not found in class %s or its parent %s",
                methodName,
                Stream.of(parameters)
                    .filter(Objects::nonNull)
                    .map(Class::getSimpleName)
                    .collect(Collectors.joining(",")),
                clazz.getName(),
                parent.getName()));
      }
    }
  }

  private static ElementContext.Basic getElementContext(BasicElementInterface type) {
    TypeProvider unionType = asBasicOrUnionType(ELEMENT_NAME, new String[] {type.name()}, false);
    return new ElementContext.Basic(ELEMENT_NAME, unionType, getCssSelector("selector"));
  }

  private static void validateAction(ActionType action, String returnType) {
    assertThat(action.getParametersTypes("test", 0), is(empty()));
    assertThat(action.getReturnType().getSimpleName(), is(equalTo(returnType)));
  }

  private static ActionType getActionType(String apply, ElementContext elementContext) {
    TypeProvider elementType = elementContext.getType();
    return BasicElementActionType.getActionType(apply, elementType, "elementName", null);
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
    validateAction(getClassAttribute, STRING_TYPE_NAME);
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
                      action,
                      action.getReturnType().getSimpleName(),
                      method.getReturnType().getName()),
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
    ElementContext elementContext = getElementContext(BasicElementInterface.actionable);
    assertThat(
        getActionType(waitForAbsence.getApplyString(), elementContext),
        is(equalTo(waitForAbsence)));
    elementContext = getElementContext(BasicElementInterface.actionable);
    assertThat(
        getActionType(getClassAttribute.getApplyString(), elementContext),
        is(equalTo(getClassAttribute)));
    elementContext = getElementContext(BasicElementInterface.clickable);
    assertThat(
        getActionType(waitForAbsence.getApplyString(), elementContext),
        is(equalTo(waitForAbsence)));
  }

  @Test
  public void testGetActionFromStringForClickable() {
    final ActionType ACTION = ClickableActionType.click;
    ElementContext elementContext = getElementContext(BasicElementInterface.clickable);
    assertThat(getActionType(ACTION.getApplyString(), elementContext), is(equalTo(ACTION)));
  }

  @Test
  public void testGetActionFromStringForEditable() {
    final ActionType ACTION = EditableActionType.clear;
    ElementContext elementContext = getElementContext(BasicElementInterface.editable);
    assertThat(getActionType(ACTION.getApplyString(), elementContext), is(equalTo(ACTION)));
  }

  @Test
  public void testGetActionFromStringForTouchable() {
    final ActionType ACTION = TouchableActionType.flick;
    ElementContext elementContext = getElementContext(BasicElementInterface.touchable);
    assertThat(getActionType(ACTION.getApplyString(), elementContext), is(equalTo(ACTION)));
  }
}
