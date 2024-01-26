package utam.compiler.helpers;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;
import static utam.compiler.helpers.TypeUtilities.VOID;
import static utam.compiler.helpers.TypeUtilities.getClassFromFullName;
import static utam.core.framework.UtamLogger.info;

import java.lang.reflect.Method;
import java.util.Objects;
import java.util.Set;
import java.util.function.Consumer;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.testng.annotations.Test;
import utam.core.declarative.representation.TypeProvider;
import utam.core.element.BasicElement;

public class BasicElementActionTypeTests {

  private static final String VOID_TYPE_NAME = "void";
  private static final String BOOLEAN_TYPE_NAME = "Boolean";
  private static final String STRING_TYPE_NAME = "String";

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

  private static void validateAction(ActionType action, String returnType) {
    assertThat(action.getParametersTypes("test", 0), is(empty()));
    assertThat(action.getReturnType().getSimpleName(), is(equalTo(returnType)));
  }

  @Test
  @SuppressWarnings("rawtypes")
  public void testStandardActionsMethods() {
    Stream.of(BasicElementActionType.values())
        .filter(BasicElementActionType::hasMethodToTest)
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
  public void checkSupportedActions() {
    for (Method method : BasicElement.class.getDeclaredMethods()) {
      checkTranslatorValue(method, BasicElementActionType::valueOf);
    }
  }

  private void checkTranslatorValue(Method method, Consumer<String> consumer) {
    consumer.accept(method.getName());
  }

  /** The absence member should return the proper value */
  @Test
  public void testAbsence() {
    validateAction(BasicElementActionType.waitForAbsence, VOID_TYPE_NAME);
  }

  /** The visibility member should return the proper value */
  @Test
  public void testVisibility() {
    validateAction(BasicElementActionType.waitForVisible, VOID_TYPE_NAME);
  }

  /** The getAttribute member should return the proper value */
  @Test
  public void testGetAttribute() {
    ActionType action = BasicElementActionType.getAttribute;
    Set<String> parameterTypeStrings =
        action.getParametersTypes("test", 1).stream()
            .map(TypeProvider::getSimpleName)
            .filter(simpleName -> !simpleName.isEmpty())
            .collect(Collectors.toSet());

    assertThat(parameterTypeStrings, hasSize(1));
    assertThat(parameterTypeStrings.iterator().next(), is(equalTo(STRING_TYPE_NAME)));
    assertThat(action.getReturnType().getSimpleName(), is(equalTo(STRING_TYPE_NAME)));
  }

  /** The getCssPropertyValue member should return the proper value */
  @Test
  public void testGetCssPropertyValue() {
    ActionType action = BasicElementActionType.getCssPropertyValue;
    Set<String> parameterTypeStrings =
        action.getParametersTypes("test", 1).stream()
            .map(TypeProvider::getSimpleName)
            .filter(simpleName -> !simpleName.isEmpty())
            .collect(Collectors.toSet());

    assertThat(parameterTypeStrings, hasSize(1));
    assertThat(parameterTypeStrings.iterator().next(), is(equalTo(STRING_TYPE_NAME)));
    assertThat(action.getReturnType().getSimpleName(), is(equalTo(STRING_TYPE_NAME)));
  }

  /** The getText member should return the proper value */
  @Test
  public void testGetText() {
    validateAction(BasicElementActionType.getText, STRING_TYPE_NAME);
  }

  /** The getTitle member should return the proper value */
  @Test
  public void testGetTitle() {
    validateAction(BasicElementActionType.getTitle, STRING_TYPE_NAME);
  }

  /** The invisibility member should return the proper value */
  @Test
  public void testInvisibility() {
    validateAction(BasicElementActionType.waitForInvisible, VOID_TYPE_NAME);
  }

  /** The isDisplayed member should return the proper value */
  @Test
  public void testIsDisplayed() {
    validateAction(BasicElementActionType.isVisible, BOOLEAN_TYPE_NAME);
  }

  /** The isPresent member should return the proper value */
  @Test
  public void testIsPresent() {
    validateAction(BasicElementActionType.isPresent, BOOLEAN_TYPE_NAME);
  }

  static boolean sameType(TypeProvider actual, Class expected) {
    if (actual.isSameType(VOID)) {
      return expected.getName().toLowerCase().contains("void");
    }
    if (expected.equals(getClassFromFullName(actual))) {
      return true;
    }
    if (actual == PrimitiveType.NUMBER) {
      return expected.getName().toLowerCase().startsWith("int");
    }
    if (actual == PrimitiveType.BOOLEAN) {
      return expected.getName().equalsIgnoreCase(actual.getSimpleName());
    }
    return false;
  }
}
