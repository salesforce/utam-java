package utam.core.framework.element;

import java.util.function.Supplier;
import utam.core.driver.Expectations;
import utam.core.element.Element.ScrollOptions;
import utam.core.framework.UtamLogger;

/**
 * utilities for element actions
 *
 * @author elizaveta.ivanova
 * @since 234
 */
public abstract class ElementExpectations {

  static Expectations<Boolean> absence() {
    return new ExpectationsImpl<>(
        "wait for absence", (driver, element) -> !element.isExisting());
  }

  static Expectations<Boolean> visibility(boolean isVisible) {
    return new ExpectationsImpl<>(
        "wait for element " + (isVisible ? "visibility" : "invisibility"),
        (driver, element) -> isVisible == element.isDisplayed());
  }

  static Expectations<Boolean> clear() {
    return new ExpectationsImpl<>("clear content of the element",
        (driver, element) -> {
          element.clear();
          return true;
        });
  }

  static Expectations<Boolean> clearAndType(String text) {
    return new ExpectationsImpl<>(
        String.format("clear and type '%s'", text),
        (driver, element) -> {
          element.clear();
          element.setText(text);
          return true;
        });
  }

  static Expectations<String> getAttribute(String attrName) {
    return new ExpectationsImpl<>(
        String.format("get attribute '%s'", attrName),
        (driver, element) -> {
          String res = element.getAttribute(attrName);
          if (res != null) {
            UtamLogger.info(String.format("attribute '%s' has value '%s'", attrName, res));
          }
          return res;
        });
  }

  static Expectations<String> getText() {
    return new ExpectationsImpl<>(
        "get element text",
        (driver, element) -> {
          String res = element.getText();
          if (res != null) {
            UtamLogger.info(String.format("text is '%s'", res));
          }
          return res;
        });
  }

  public static Expectations<Boolean> setText(String str) {
    return new ExpectationsImpl<>(
        String.format("set element text to '%s'", str), (driver, element) -> {
      element.setText(str);
      return true;
    });
  }

  static Expectations<Match> isPresent() {
    return new ExpectationsImpl<>("check element presence",
        (driver, element) -> Match.from(element.isExisting()));
  }

  static Expectations<Boolean> javascriptClick() {
    return new ExpectationsImpl<>(
        "deprecated javascript click",
        (driver, element) -> {
          element.deprecatedClick(driver);
          return true;
        });
  }

  static Expectations<Boolean> click() {
    return new ExpectationsImpl<>("click", (driver, element) -> {
      try {
        element.click();
      } catch (Exception e) {
        if (e.getMessage()
            .contains("javascript error: Cannot read property 'defaultView' of undefined")) {
          UtamLogger.error(
              "Error from WebElement.click(), attempting to execute javascript click instead...");
          UtamLogger.error(e);
          element.deprecatedClick(driver);
        } else {
          throw e;
        }
      }
      return true;
    });
  }

  static Expectations<Boolean> moveTo() {
    return new ExpectationsImpl<>(
        "move to element",
        (driver, element) -> {
          element.moveTo(driver);
          return true;
        });
  }

  static Expectations<Boolean> focus() {
    return new ExpectationsImpl<>(
        "focus on the element",
        (driver, element) -> {
          element.focus(driver);
          return true;
        });
  }

  static Expectations<Boolean> scrollTo(ScrollOptions options) {
    return new ExpectationsImpl<>(
        String.format("scroll to %s", options.name().toLowerCase()),
        (driver, element) -> {
          element.scrollIntoView(driver, options);
          return true;
        });
  }

  static Expectations<Boolean> blur() {
    return new ExpectationsImpl<>(
        "blur",
        (driver, element) -> {
          element.blur(driver);
          return true;
        });
  }

  static <T> Expectations<T> waitFor(Supplier<T> condition) {
    return new ExpectationsImpl<>("wait for condition", (driver, element) -> condition
        .get());
  }

  /**
   * fluentWait does not allow to return false, so this enum is a wrapper for boolean methods to be
   * able to return false instead throwing error
   *
   * @author elizaveta.ivanova
   */
  public enum Match {
    TRUE,
    FALSE;

    static Match from(boolean is) {
      return is ? TRUE : FALSE;
    }

    public boolean is() {
      return this == TRUE;
    }
  }
}
