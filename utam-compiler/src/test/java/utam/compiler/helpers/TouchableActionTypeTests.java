package utam.compiler.helpers;

import utam.core.declarative.representation.TypeProvider;
import org.testng.annotations.Test;
import utam.core.selenium.element.Touchable;

import java.lang.reflect.Method;
import java.util.*;
import java.util.function.Consumer;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static utam.core.framework.UtamLogger.info;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;

/**
 * Tests for the TouchableActionType enum
 *
 * @author r.rajasekaran
 */
public class TouchableActionTypeTests {

  private static final String VOID_TYPE_NAME = "void";
  private static final String BOOLEAN_TYPE_NAME = "Boolean";

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
    validateAction(
        TouchableActionType.flick,
            VOID_TYPE_NAME,
            Collections.unmodifiableList(List.of("Integer", "Integer")));
  }

  /** The flickItems member should return the proper value */
  @Test
  public void testFlickItems() {
    validateAction(
        TouchableActionType.flickItems,
            BOOLEAN_TYPE_NAME,
            Collections.singletonList("String"));
  }

  private void validateAction(
          ActionType action, String returnType, List<String> parameterTypes) {
    List<String> parameterTypeStrings =
        action.getParametersTypes().stream()
            .filter((type) -> !type.getSimpleName().isEmpty())
            .map(TypeProvider::getSimpleName)
            .collect(Collectors.toList());

    assertThat(parameterTypeStrings, containsInAnyOrder(parameterTypes.toArray()));
    assertThat(parameterTypeStrings, hasSize(parameterTypes.size()));
    assertThat(action.getReturnType().getSimpleName(), is(equalTo(returnType)));
    assertThat(action.isListAction(), is(equalTo(false)));
    assertThat(action.getInvokeMethodName(), is(equalTo(((TouchableActionType)action).name())));
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
                      action.getReturnType().name(),
                      method.getReturnType().getName()),
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
}
