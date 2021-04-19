/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.element;

import static utam.core.element.FindContext.Type.NULLABLE;
import static utam.core.element.FindContext.Type.NULLABLE_IN_SHADOW;

import java.util.function.Supplier;
import org.openqa.selenium.Keys;
import utam.core.driver.Driver;
import utam.core.driver.DriverTimeouts;
import utam.core.driver.Expectations;
import utam.core.element.Actionable;
import utam.core.element.BaseElement;
import utam.core.element.Clickable;
import utam.core.element.Editable;
import utam.core.element.Element;
import utam.core.element.Element.GestureDirection;
import utam.core.element.Element.ScrollOptions;
import utam.core.element.Locator;
import utam.core.element.Touchable;
import utam.core.framework.UtamLogger;
import utam.core.framework.base.PageObjectsFactory;

/**
 * base element that wraps Element implementation with Driver waits, instantiated on the FOUND
 * element
 *
 * @author elizaveta.ivanova
 * @since 234
 */
public class BasePageElement implements BaseElement, Actionable, Clickable, Editable, Touchable {

  private final Driver driver;
  private final DriverTimeouts timeouts;
  private final Element element;

  public BasePageElement(PageObjectsFactory factory, Element element) {
    this.driver = factory.getDriver();
    this.element = element;
    this.timeouts = factory.getDriverContext().getTimeouts();
  }

  private Element getElement() {
    return element;
  }

  private <T> T waitFor(Expectations<T> expectations) {
    return driver.waitFor(timeouts.getWaitForTimeout(), timeouts.getPollingInterval(), expectations,
        getElement());
  }

  private <T> T apply(Expectations<T> expectations) {
    return driver
        .waitFor(timeouts.getFluentWaitTimeout(), timeouts.getPollingInterval(), expectations,
            getElement());
  }

  @Override
  public void waitForAbsence() {
    Expectations<Boolean> expectations = ElementExpectations.absence();
    log(expectations.getLogMessage());
    waitFor(expectations);
  }

  @Override
  public void waitForVisible() {
    Expectations<Boolean> expectations = ElementExpectations.visibility(true);
    log(expectations.getLogMessage());
    waitFor(expectations);
  }

  @Override
  public void waitForInvisible() {
    Expectations<Boolean> expectations = ElementExpectations.visibility(false);
    log(expectations.getLogMessage());
    waitFor(expectations);
  }

  @Override
  public boolean isVisible() {
    return getElement().isDisplayed();
  }

  @Override
  public boolean isEnabled() {
    return getElement().isEnabled();
  }

  @Override
  public String getAttribute(String attribute) {
    Expectations<String> expectations = ElementExpectations.getAttribute(attribute);
    log(expectations.getLogMessage());
    return apply(expectations);
  }

  @Override
  public String getClassAttribute() {
    return getAttribute("class");
  }

  @Override
  public String getText() {
    Expectations<String> expectations = ElementExpectations.getText();
    log(expectations.getLogMessage());
    return apply(expectations);
  }

  @Override
  public void setText(String text) {
    Expectations<Boolean> expectations = ElementExpectations.setText(text);
    log(expectations.getLogMessage());
    apply(expectations);
  }

  @Override
  public String getTitle() {
    return getAttribute("title");
  }

  @Override
  public String getValue() {
    return getAttribute("value");
  }

  @Override
  public void moveTo() {
    Expectations<Boolean> expectations = ElementExpectations.moveTo();
    log(expectations.getLogMessage());
    apply(expectations);
  }

  @Override
  public void scrollToCenter() {
    Expectations<Boolean> expectations = ElementExpectations.scrollTo(ScrollOptions.CENTER);
    log(expectations.getLogMessage());
    apply(expectations);
  }

  @Override
  public void scrollToTop() {
    Expectations<Boolean> expectations = ElementExpectations.scrollTo(ScrollOptions.TOP);
    log(expectations.getLogMessage());
    apply(expectations);
  }

  @Override
  public boolean isFocused() {
    return getElement().hasFocus(driver);
  }

  @Override
  public void focus() {
    Expectations<Boolean> expectations = ElementExpectations.focus();
    log(expectations.getLogMessage());
    apply(expectations);
  }

  @Override
  public void scrollTo() {
    scrollToTop();
  }

  @Override
  public void blur() {
    Expectations<Boolean> expectations = ElementExpectations.blur();
    log(expectations.getLogMessage());
    apply(expectations);
  }

  @Override
  public <T> T waitFor(Supplier<T> condition) {
    Expectations<T> expectations = new ExpectationsImpl<>("wait for condition",
        (driver, element) -> condition.get());
    log(expectations.getLogMessage());
    return waitFor(expectations);
  }

  @Override
  public boolean containsElement(Locator locator, boolean isExpandShadow) {
    return
        getElement().findElements(locator, isExpandShadow ? NULLABLE_IN_SHADOW : NULLABLE).size()
            > 0;
  }

  @Override
  public boolean containsElement(Locator locator) {
    return containsElement(locator, false);
  }

  @Override
  public void clear() {
    Expectations<Boolean> expectations = ElementExpectations.clear();
    log(expectations.getLogMessage());
    apply(expectations);
  }

  @Override
  public void clearAndType(String text) {
    Expectations<Boolean> expectations = ElementExpectations.clearAndType(text);
    log(expectations.getLogMessage());
    apply(expectations);
  }

  @Override
  public void press(CharSequence key) {
    Keys keyToPress = Keys.valueOf(key.toString().toUpperCase());
    log(String.format("press keyboard key '%s'", keyToPress.name()));
    Expectations<Boolean> expectation = ElementExpectations.setText(keyToPress.toString());
    apply(expectation);
  }

  @Override
  public void click() {
    Expectations<Boolean> expectations = ElementExpectations.click();
    log(expectations.getLogMessage());
    apply(expectations);
  }

  @Override
  public void javascriptClick() {
    Expectations<Boolean> expectations = ElementExpectations.javascriptClick();
    log(expectations.getLogMessage());
    apply(expectations);
  }

  @Override
  public void flick(int xOffset, int yOffset) {
    Expectations<Boolean> expectations = new ExpectationsImpl<>(
        String.format("flick element at X '%d' Y '%d'", xOffset, yOffset),
        (driver, element) -> {
          element.flick(driver, timeouts.getWaitForTimeout(), timeouts.getPollingInterval(),  xOffset, yOffset);
          return true;
        });
    log(expectations.getLogMessage());
    apply(expectations);
  }

  private void log(String message) {
    UtamLogger.info(message);
  }

  @Override
  public boolean flickItems(GestureDirection direction) {
    return getElement().flickItems(direction);
  }

  @Override
  public boolean isPresent() {
    return !getElement().isNull();
  }
}
