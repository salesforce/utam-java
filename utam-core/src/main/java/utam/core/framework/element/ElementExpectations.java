package utam.core.framework.element;

import java.util.function.BiConsumer;
import java.util.function.BiFunction;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Supplier;
import utam.core.driver.Driver;
import utam.core.element.Element;
import utam.core.element.Element.ScrollOptions;
import utam.core.framework.UtamLogger;

/**
 * utilities for element actions
 *
 * @author elizaveta.ivanova
 * @since 234
 */
public final class ElementExpectations<R> extends ExpectationsImpl<Element, R> {


  ElementExpectations(String log, BiFunction<Driver, Element, R> apply) {
    super(log, apply, null);
  }

  private static ElementExpectations<String> getReturnsString(String log,
      Function<Element, String> apply) {
    return new ElementExpectations<>(log, (d, e) -> apply.apply(e));
  }

  private static ElementExpectations<Element> getReturnsElementBiConsumer(String log,
      BiConsumer<Driver, Element> apply) {
    return new ElementExpectations<>(log, (d, e) -> {
      apply.accept(d, e);
      return e;
    });
  }

  private static ElementExpectations<Element> getReturnsElement(String log, Consumer<Element> apply) {
    return new ElementExpectations<>(log, (d, e) -> {
      apply.accept(e);
      return e;
    });
  }

  public static ElementExpectations<Boolean> absence() {
    return new ElementExpectations<>(
        "wait for absence", ((driver, element) -> !element.isExisting()));
  }

  public static ElementExpectations<Boolean> visibility(boolean isVisible) {
    return new ElementExpectations<>(
        "wait for element " + (isVisible ? "visibility" : "invisibility"),
        (driver, element) -> isVisible == element.isDisplayed());
  }

  public static ElementExpectations<Element> clear() {
    return getReturnsElement("clear content of the element", Element::clear);
  }

  public static ElementExpectations<Element> clearAndType(String text) {
    return getReturnsElement(
        String.format("clear and type '%s'", text),
        element -> {
          element.clear();
          element.setText(text);
        });
  }

  public static ElementExpectations<String> getAttribute(String attrName) {
    return getReturnsString(
        String.format("get attribute '%s'", attrName),
        element -> {
          String res = element.getAttribute(attrName);
          if (res != null) {
            UtamLogger.info(String.format("attribute '%s' has value '%s'", attrName, res));
          }
          return res;
        });
  }

  public static ElementExpectations<String> getText() {
    return getReturnsString(
        "get element text",
        element -> {
          String res = element.getText();
          if (res != null) {
            UtamLogger.info(String.format("text is '%s'", res));
          }
          return res;
        });
  }

  public static ElementExpectations<Element> setText(String str) {
    return getReturnsElement(
        String.format("set element text to '%s'", str), element -> element.setText(str));
  }

  public static ElementExpectations<Match> isPresent() {
    return new ElementExpectations<>("check element presence",
        (driver, element) -> Match.from(element.isExisting()));
  }

  public static ElementExpectations<Element> javascriptClick() {
    return getReturnsElementBiConsumer(
        "deprecated javascript click",
        (driver, element) -> element.deprecatedClick(driver));
  }

  public static ElementExpectations<Element> click() {
    return getReturnsElementBiConsumer("click", (driver, element) -> {
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
    });
  }

  public static ElementExpectations<Element> moveTo() {
    return getReturnsElementBiConsumer(
        "move to element",
        (driver, element) -> element.moveTo(driver));
  }

  public static ElementExpectations<Element> focus() {
    return getReturnsElementBiConsumer(
        "focus on the element",
        (driver, element) -> element.focus(driver));
  }

  public static ElementExpectations<Element> scrollTo(ScrollOptions options) {
    return getReturnsElementBiConsumer(
        String.format("scroll to %s", options.name().toLowerCase()),
        (driver, element) -> element
            .scrollIntoView(driver, options));
  }

  public static ElementExpectations<Element> blur() {
    return getReturnsElementBiConsumer(
        "blur",
        (driver, element) -> element.blur(driver));
  }

  public static <T> ElementExpectations<T> waitFor(Supplier<T> condition) {
    return new ElementExpectations<>("wait for condition", (driver, element) -> condition
        .get());
  }

  public static ElementExpectations<Element> flick(int xOffset, int yOffset) {
    return getReturnsElementBiConsumer(
        String.format("flick element at X '%d' Y '%d'", xOffset, yOffset),
        (driver, element) -> element.flick(driver, xOffset, yOffset));
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
