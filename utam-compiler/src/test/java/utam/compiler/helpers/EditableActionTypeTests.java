package utam.compiler.helpers;

import utam.core.declarative.representation.TypeProvider;
import org.testng.annotations.Test;
import utam.core.selenium.element.Editable;
import utam.core.selenium.expectations.DriverExpectationsUtil;

import java.lang.reflect.Method;
import java.lang.reflect.ParameterizedType;
import java.util.*;
import java.util.function.Consumer;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static utam.compiler.helpers.ActionableActionTypeTests.sameType;
import static utam.core.framework.UtamLogger.info;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;

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
    validateAction(
        EditableActionType.clearAndType,
            Collections.singletonList("String"));
  }

  /** The setText member should return the proper value */
  @Test
  public void testSetText() {
    validateAction(
        EditableActionType.setText,
            Collections.singletonList(STRING_TYPE_NAME));
  }

  @Test
  public void testGetParameterClasses() {
    List<String> list =
        Arrays.stream(EditableActionType.setText.getParameterClasses())
            .map(Class::getSimpleName)
            .collect(Collectors.toList());
    assertThat(list, containsInAnyOrder(STRING_TYPE_NAME));
  }

  private void validateAction(
          ActionType action, List<String> parameterTypes) {
    Set<String> parameterTypeStrings =
        action.getParametersTypes().stream()
            .filter((type) -> !type.getSimpleName().isEmpty())
            .map(TypeProvider::getSimpleName)
            .collect(Collectors.toSet());

    assertThat(parameterTypeStrings, containsInAnyOrder(parameterTypes.toArray()));
    assertThat(parameterTypeStrings, hasSize(parameterTypes.size()));
    assertThat(action.getReturnType().getSimpleName(), is(equalTo(EditableActionTypeTests.VOID_TYPE_NAME)));
    assertThat(action.isSingleCardinality(), is(equalTo(false)));
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
      // Class expected = action.getReturnType() == null ? WebElement.class : action.getReturnType();
      Class expected = action.getReturnType();
      assertThat(expected, is(equalTo(expectationReturns)));
    }
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
