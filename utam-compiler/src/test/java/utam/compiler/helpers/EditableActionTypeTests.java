/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.helpers;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static utam.compiler.helpers.BasicElementActionTypeTests.sameType;
import static utam.core.framework.UtamLogger.info;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.function.Consumer;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.testng.annotations.Test;
import utam.core.declarative.representation.TypeProvider;
import utam.core.element.Editable;

/**
 * Provides tests for the TranslatableAction enum
 *
 * @author james.evans
 */
public class EditableActionTypeTests {

  private static final String VOID_TYPE_NAME = "void";
  private static final String STRING_TYPE_NAME = "String";

  @SuppressWarnings({"unchecked", "rawtypes"})
  private static Method getMethod(Class clazz, String methodName, Class[] parameters) {
    try {
      return clazz.getDeclaredMethod(methodName, parameters);
    } catch (Exception e) {
      throw new AssertionError(
          String.format("method '%s' not found in class %s", methodName, clazz.getName()), e);
    }
  }

  private static void validateAction(ActionType action, List<String> parameterTypes) {
    Set<String> parameterTypeStrings =
        action.getParametersTypes("test", parameterTypes.size()).stream()
            .map(TypeProvider::getSimpleName)
            .filter(simpleName -> !simpleName.isEmpty())
            .collect(Collectors.toSet());

    assertThat(parameterTypeStrings, containsInAnyOrder(parameterTypes.toArray()));
    assertThat(parameterTypeStrings, hasSize(parameterTypes.size()));
    assertThat(
        action.getReturnType().getSimpleName(),
        is(equalTo(EditableActionTypeTests.VOID_TYPE_NAME)));
  }

  @Test
  public void checkSupportedActions() {
    for (Method method : Editable.class.getDeclaredMethods()) {
      checkTranslatorValue(method, EditableActionType::valueOf);
    }
  }

  private void checkTranslatorValue(Method method, Consumer<String> consumer) {
    consumer.accept(method.getName());
  }

  /** The clear member should return the proper value */
  @Test
  public void testClear() {
    validateAction(EditableActionType.clear, new ArrayList<>());
  }

  /** The clearAndType member should return the proper value */
  @Test
  public void testClearAndType() {
    validateAction(EditableActionType.clearAndType, Collections.singletonList("String"));
  }

  /** The setText member should return the proper value */
  @Test
  public void testSetText() {
    validateAction(EditableActionType.setText, Collections.singletonList(STRING_TYPE_NAME));
  }

  @Test
  public void testGetParameterClasses() {
    List<String> list =
        Arrays.stream(EditableActionType.setText.getParameterClasses())
            .map(Class::getSimpleName)
            .collect(Collectors.toList());
    assertThat(list, containsInAnyOrder(STRING_TYPE_NAME));
  }

  @SuppressWarnings("rawtypes")
  @Test
  public void testStandardActionsMethods() {
    Stream.of(EditableActionType.values())
        // method "press" has different parameter types and can't be found
        .filter(type -> type != EditableActionType.press)
        .forEach(
            action -> {
              info(String.format("test element action '%s'", action));
              Method method =
                  getMethod(action.getElementClass(), action.name(), action.getParameterClasses());
              assertThat(
                  String.format(
                      "action '%s' returns '%s', method returns '%s'",
                      action.name(),
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
}
