/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.helpers;

import utam.core.declarative.representation.TypeProvider;
import org.testng.annotations.Test;
import utam.core.element.Clickable;

import java.lang.reflect.Method;
import java.util.*;
import java.util.function.Consumer;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static utam.compiler.helpers.BasicElementActionTypeTests.sameType;
import static utam.core.framework.UtamLogger.info;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;

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
  
  /** The javascriptClick member should return the proper value */
  @Test
  public void testJavascriptClick() {
    validateAction(ClickableActionType.javascriptClick, new ArrayList<>());
  }

  private void validateAction(ActionType action, List<String> parameterTypes) {
    Set<String> parameterTypeStrings =
        action.getParametersTypes().stream()
            .filter((type) -> !type.getSimpleName().isEmpty())
            .map(TypeProvider::getSimpleName)
            .collect(Collectors.toSet());

    assertThat(parameterTypeStrings, containsInAnyOrder(parameterTypes.toArray()));
    assertThat(parameterTypeStrings, hasSize(parameterTypes.size()));
    assertThat(action.getReturnType().getSimpleName(), is(equalTo(ClickableActionTypeTests.VOID_TYPE_NAME)));
  }

  @Test
  @SuppressWarnings({"rawtypes"})
  public void testStandardActionsMethods() {
    Stream.of(ClickableActionType.values())
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
              // For now, all clickable element actions have zero parameters. Should
              // a method exist that requires parameters the block of code below will
              // be needed to validate their types.
              // for (int i = 0; i < params.length; i++) {
              //   assertThat(params[i], is(equalTo(method.getParameterTypes()[i])));
              // }
            });
  }
}
