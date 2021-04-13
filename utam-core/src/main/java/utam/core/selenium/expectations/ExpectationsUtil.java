/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.selenium.expectations;

import static utam.core.selenium.expectations.SalesforceWebDriverUtils.SCROLL_INTO_VIEW_ALIGN_TO_TOP_JS;

import java.util.List;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Supplier;
import org.openqa.selenium.By;
import org.openqa.selenium.JavascriptException;
import org.openqa.selenium.SearchContext;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.interactions.Actions;
import utam.core.framework.UtamLogger;
import utam.core.selenium.context.WebDriverUtilities;
import utam.core.selenium.element.Selector;
import utam.core.selenium.element.ShadowRootWebElement;

/**
 * helper class for different types of element expectations <br>
 * each method is mapped to enum so that generator could recognize which code to put
 *
 * @author elizaveta.ivanova
 * @since 224
 */
public class ExpectationsUtil {

  public static final String FOCUS_JS = "arguments[0].focus();";
  public static final String SCROLL_TO_CENTER_JS = "arguments[0].scrollIntoView({block:'center'});";
  public static final String BLUR_JS = "arguments[0].blur();";
  static final String ABSENCE_MSG = "wait for absence";
  static final String VISIBILITY_MSG = "wait for visibility";
  static final String IS_VISIBLE_MSG = "check for element visibility";
  static final String INVISIBILITY_MSG = "wait for invisibility";
  static final String CLEAR_AND_TYPE_MSG = "clear and type '%s'";
  static final String GET_ATTRIBUTE_MSG = "get attribute '%s'";
  static final String GET_TEXT_MSG = "get element text";
  static final String SET_TEXT_MSG = "set element text to '%s'";
  static final String FIND_MSG = "find element%s";
  static final String CLEAR_MSG = "clear content of the element";
  static final String JAVASCRIPT_MSG = "execute javascript '%s'";
  static final String CLICK_JS = "arguments[0].click();";
  static final String MOVE_TO_ELEMENT_MSG = "move to element";
  static final String PRESENCE_WAIT_MSG = "wait for element presence";
  static final String PRESENCE_CHECK_MSG = "check for element presence";
  static final String ENABLED_CHECK_MSG = "check if element is enabled";
  static final String HAS_FOCUS_MSG = "check if current element has focus";
  static final String WAIT_FOR_MSG = "wait for condition";
  static final String FIND_ELEMENTS_MSG = "find elements with selector { %s } inside current element%s";

  private static boolean isNothingPresent(List<WebElement> list) {
    return list == null || list.isEmpty();
  }

  public static ElementListExpectations<Boolean> waitForAbsence() {
    return new AbstractListExpectation<>(ABSENCE_MSG, ExpectationsUtil::isNothingPresent, true);
  }

  private static boolean isNothingDisplayed(List<WebElement> list) {
    if (isNothingPresent(list)) {
      return true;
    }
    return list.stream().noneMatch(WebElement::isDisplayed);
  }

  public static ElementListExpectations<Boolean> waitForInvisible() {
    return new AbstractListExpectation<>(
        INVISIBILITY_MSG, ExpectationsUtil::isNothingDisplayed, true);
  }

  public static ElementExpectations<SearchContext> clear() {
    return new AbstractElementExpectation.ConsumesElement(CLEAR_MSG, WebElement::clear);
  }

  public static ElementExpectations<SearchContext> clearAndType(String text) {
    return new AbstractElementExpectation.ConsumesElement(
        String.format(CLEAR_AND_TYPE_MSG, text),
        element -> {
          element.clear();
          element.sendKeys(text);
        });
  }

  public static ElementExpectations<SearchContext> click() {
    return new AbstractElementExpectation.ConsumesUtilsElement("click", (utilities, element) -> {
      try {
        element.click();
      } catch (JavascriptException e) {
        if(e.getMessage().contains("javascript error: Cannot read property 'defaultView' of undefined")) {
          UtamLogger.warning(String.format("Error [%s] was thrown from WebElement.click(), attempting to execute javascript click instead...", e.getMessage()));
          utilities.executeJavaScript(CLICK_JS, element);
          return;
        }
        throw e;
      }
    });
  }

  public static ElementExpectations<String> getAttribute(String attrName) {
    return new AbstractElementExpectation.ReturnsString(
        String.format(GET_ATTRIBUTE_MSG, attrName),
        element -> {
          String res = element.getAttribute(attrName);
          if (res != null) {
            UtamLogger.info(String.format("attribute '%s' has value '%s'", attrName, res));
          }
          return res;
        });
  }

  public static ElementExpectations<String> getText() {
    return new AbstractElementExpectation.ReturnsString(
        GET_TEXT_MSG,
        webElement -> {
          String res = webElement.getText();
          if (res != null) {
            UtamLogger.info(String.format("text is '%s'", res));
          }
          return res;
        });
  }

  private static boolean isSomethingDisplayed(List<WebElement> list) {
    if (list != null) {
      // remove invisible elements
      list.removeIf(we -> !we.isDisplayed());
    }
    return isSomethingPresent(list);
  }

  public static ElementListExpectations<ElementWait.Match> isDisplayed() {
    return new AbstractListExpectation<>(
        IS_VISIBLE_MSG,
        list -> ElementWait.Match.from(isSomethingDisplayed(list)),
        ElementWait.Match.FALSE);
  }

  public static ElementListExpectations<Boolean> waitForVisible() {
    return new AbstractListExpectation<>(VISIBILITY_MSG, ExpectationsUtil::isSomethingDisplayed);
  }

  private static boolean isSomethingPresent(List<WebElement> list) {
    return list != null && list.size() > 0;
  }

  public static ElementListExpectations<ElementWait.Match> isPresent() {
    return new AbstractListExpectation<>(
        PRESENCE_CHECK_MSG,
        list -> ElementWait.Match.from(isSomethingPresent(list)),
        ElementWait.Match.FALSE);
  }

  public static ElementListExpectations<Boolean> presence() {
    return new AbstractListExpectation<>(PRESENCE_WAIT_MSG, ExpectationsUtil::isSomethingPresent);
  }

  public static ElementExpectations<SearchContext> javascriptClick() {
    return new AbstractElementExpectation.ConsumesUtilsElement(
        String.format(JAVASCRIPT_MSG, CLICK_JS),
        (utilities, element) -> utilities.executeJavaScript(CLICK_JS, element));
  }

  public static ElementExpectations<SearchContext> setText(String str) {
    return new AbstractElementExpectation.ConsumesElement(
        String.format(SET_TEXT_MSG, str), element -> element.sendKeys(str));
  }

  public static ElementExpectations<SearchContext> moveTo() {
    return new AbstractElementExpectation.ConsumesUtilsElement(
        MOVE_TO_ELEMENT_MSG,
        (utilities, element) -> {
          Actions actions = new Actions(utilities.getWebDriver());
          actions.moveToElement(element).perform();
        });
  }

  public static ElementExpectations<SearchContext> find(boolean isExpandShadowRoot) {
    String message = String.format(FIND_MSG, isExpandShadowRoot ? " shadow root" : "");
    return new AbstractElementExpectation.ReturnsElement(
        message,
        (utilities, element) -> {
          if (isExpandShadowRoot) {
            return utilities.expandShadowRoot(element);
          } else {
            return element;
          }
        });
  }

  public static ElementListExpectations<ElementWait.Match> isEnabled() {
    return new AbstractListExpectation<>(
        ENABLED_CHECK_MSG,
        list -> {
          boolean res;
          if (isNothingPresent(list)) {
            res = false;
          } else {
            res = list.stream().allMatch(WebElement::isEnabled);
          }
          return ElementWait.Match.from(res);
        },
        ElementWait.Match.FALSE);
  }

  public static ElementExpectations<SearchContext> focus() {
    return new AbstractElementExpectation.ConsumesUtilsElement(
        String.format(JAVASCRIPT_MSG, FOCUS_JS),
        (utilities, element) -> utilities.executeJavaScript(FOCUS_JS, element));
  }

  public static ElementExpectations<ElementWait.Match> hasFocus() {
    return new AbstractElementExpectation.Predicate(
        HAS_FOCUS_MSG,
        (utilities, element) ->
            utilities.getWebDriver().switchTo().activeElement().equals(element));
  }

  public static ElementExpectations<SearchContext> scrollToCenter() {
    return new AbstractElementExpectation.ConsumesUtilsElement(
        String.format(JAVASCRIPT_MSG, SCROLL_TO_CENTER_JS),
        (utilities, element) -> utilities.executeJavaScript(SCROLL_TO_CENTER_JS, element));
  }

  public static ElementExpectations<SearchContext> scrollTo() {
    return new AbstractElementExpectation.ConsumesUtilsElement(
        String.format(JAVASCRIPT_MSG, SCROLL_INTO_VIEW_ALIGN_TO_TOP_JS),
        SalesforceWebDriverUtils::scrollIntoViewJavascript);
  }

  public static ElementExpectations<SearchContext> blur() {
    return new AbstractElementExpectation.ConsumesUtilsElement(
            String.format(JAVASCRIPT_MSG, BLUR_JS),
            (utilities, element) -> utilities.executeJavaScript(BLUR_JS, element));
  }

  public static <T> ElementExpectations<T> waitFor(Supplier<T> condition) {
    BiFunction<WebDriverUtilities, SearchContext, T> apply = (utilities, element) -> condition.get();
    return new AbstractElementExpectation.Returns<>(WAIT_FOR_MSG, apply);
  }

  public static ElementExpectations<Integer> findElements(Selector selector, boolean isExpandShadow) {
    By by = selector.by();
    Function<WebElement, WebElement> transformer = we -> isExpandShadow? new ShadowRootWebElement(we) : we;
    return new AbstractElementExpectation.Returns<>(
        String.format(FIND_ELEMENTS_MSG, by.toString(), isExpandShadow? "'s shadow root" : ""),
        (utilities, context) -> {
          WebElement current = transformer.apply((WebElement) context);
          List<WebElement> found = current.findElements(by);
          if(found == null || found.isEmpty()) {
            return 0;
          }
          return found.size();
        }, 0);
  }
}
