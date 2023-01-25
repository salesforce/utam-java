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
import java.util.List;
import java.util.function.Consumer;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.testng.annotations.Test;
import utam.core.declarative.representation.TypeProvider;
import utam.core.element.Touchable;

/**
 * Tests for the TouchableActionType enum
 *
 * @author r.rajasekaran
 */
public class TouchableActionTypeTests {

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
    for (Method method : Touchable.class.getDeclaredMethods()) {
      checkTranslatorValue(method, TouchableActionType::valueOf);
    }
  }

  private void checkTranslatorValue(Method method, Consumer<String> consumer) {
    consumer.accept(method.getName());
  }

  /** The flick member should return the proper value */
  @Test
  public void testFlick() {
    TouchableActionType action = TouchableActionType.flick;
    List<String> parametersTypes = List.of("Integer", "Integer");
    List<String> parameterTypeStrings =
        action.getParametersTypes("test", 2).stream()
            .map(TypeProvider::getSimpleName)
            .filter(simpleName -> !simpleName.isEmpty())
            .collect(Collectors.toList());

    assertThat(parameterTypeStrings, containsInAnyOrder(parametersTypes.toArray()));
    assertThat(parameterTypeStrings, hasSize(parametersTypes.size()));
    assertThat(action.getReturnType().getSimpleName(), is(equalTo("void")));
    assertThat(action.getApplyString(), is(equalTo(action.name())));
  }

  @SuppressWarnings("rawtypes")
  @Test
  public void testStandardActionsMethods() {
    Stream.of(TouchableActionType.values())
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
