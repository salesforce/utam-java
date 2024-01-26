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
import java.util.List;
import java.util.Set;
import java.util.function.Consumer;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.testng.annotations.Test;
import utam.core.declarative.representation.TypeProvider;
import utam.core.element.Clickable;

/**
 * Provides tests for the TranslatableAction enum
 *
 * @author james.evans
 */
public class ClickableActionTypeTests {

  private static final String VOID_TYPE_NAME = "void";

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
        is(equalTo(ClickableActionTypeTests.VOID_TYPE_NAME)));
  }

  @Test
  public void checkSupportedActions() {
    for (Method method : Clickable.class.getDeclaredMethods()) {
      checkTranslatorValue(method, ClickableActionType::valueOf);
    }
  }

  private void checkTranslatorValue(Method method, Consumer<String> consumer) {
    consumer.accept(method.getName());
  }

  /** The click member should return the proper value */
  @Test
  public void testClick() {
    validateAction(ClickableActionType.click, new ArrayList<>());
  }

  @Test
  @SuppressWarnings({"rawtypes"})
  public void testStandardActionsMethods() {
    Stream.of(ClickableActionType.values())
        .forEach(
            action -> {
              info(String.format("test element action '%s'", action));
              Class[] paramClasses = action.getParameterClasses();
              Method method = getMethod(action.getElementClass(), action.name(), paramClasses);
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
