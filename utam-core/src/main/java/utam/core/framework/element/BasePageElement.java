package utam.core.framework.element;

import static utam.core.element.FindContext.Type.NULLABLE;
import static utam.core.element.FindContext.Type.NULLABLE_IN_SHADOW;

import java.util.function.Supplier;
import org.openqa.selenium.Keys;
import utam.core.driver.Driver;
import utam.core.driver.DriverTimeouts;
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
 * base element that wraps Element implementation with Driver waits, instantiated on the FOUND element
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

  private <T> T waitFor(ElementExpectations<T> expectations) {
    return driver.waitFor(expectations, getElement());
  }

  private <T> T apply(ElementExpectations<T> expectations) {
    return driver
        .waitFor(timeouts.getFluentWaitTimeout(), timeouts.getPollingInterval(), expectations,
            getElement());
  }

  @Override
  public void waitForAbsence() {
    ElementExpectations<Boolean> expectations = ElementExpectations.absence();
    log(expectations.getLogMessage());
    waitFor(expectations);
  }

  @Override
  public void waitForVisible() {
    ElementExpectations<Boolean> expectations = ElementExpectations.visibility(true);
    log(expectations.getLogMessage());
    waitFor(expectations);
  }

  @Override
  public void waitForInvisible() {
    ElementExpectations<Boolean> expectations = ElementExpectations.visibility(false);
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
    ElementExpectations<String> expectations = ElementExpectations.getAttribute(attribute);
    log(expectations.getLogMessage());
    return apply(expectations);
  }

  @Override
  public String getClassAttribute() {
    return getAttribute("class");
  }

  @Override
  public String getText() {
    ElementExpectations<String> expectations = ElementExpectations.getText();
    log(expectations.getLogMessage());
    return apply(expectations);
  }

  @Override
  public void setText(String text) {
    ElementExpectations<Element> expectations = ElementExpectations.setText(text);
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
    ElementExpectations<Element> expectations = ElementExpectations.moveTo();
    log(expectations.getLogMessage());
    apply(expectations);
  }

  @Override
  public void scrollToCenter() {
    ElementExpectations<Element> expectations = ElementExpectations.scrollTo(ScrollOptions.CENTER);
    log(expectations.getLogMessage());
    apply(expectations);
  }

  @Override
  public void scrollToTop() {
    ElementExpectations<Element> expectations = ElementExpectations.scrollTo(ScrollOptions.TOP);
    log(expectations.getLogMessage());
    apply(expectations);
  }

  @Override
  public boolean isFocused() {
    return getElement().hasFocus(driver);
  }

  @Override
  public void focus() {
    ElementExpectations<Element> expectations = ElementExpectations.focus();
    log(expectations.getLogMessage());
    apply(expectations);
  }

  @Override
  public void scrollTo() {
    scrollToTop();
  }

  @Override
  public void blur() {
    ElementExpectations<Element> expectations = ElementExpectations.blur();
    log(expectations.getLogMessage());
    apply(expectations);
  }

  @Override
  public <T> T waitFor(Supplier<T> condition) {
    ElementExpectations<T> expectations = new ElementExpectations<>("wait for condition",
        ((driver, element) -> condition.get()));
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
    ElementExpectations<Element> expectations = ElementExpectations.clear();
    log(expectations.getLogMessage());
    apply(expectations);
  }

  @Override
  public void clearAndType(String text) {
    ElementExpectations<Element> expectations = ElementExpectations.clearAndType(text);
    log(expectations.getLogMessage());
    apply(expectations);
  }

  @Override
  public void press(CharSequence key) {
    Keys keyToPress = Keys.valueOf(key.toString().toUpperCase());
    log(String.format("press keyboard key '%s'", keyToPress.name()));
    ElementExpectations<Element> expectation = ElementExpectations.setText(keyToPress.toString());
    apply(expectation);
  }

  @Override
  public void click() {
    ElementExpectations<Element> expectations = ElementExpectations.click();
    log(expectations.getLogMessage());
    apply(expectations);
  }

  @Override
  public void javascriptClick() {
    ElementExpectations<Element> expectations = ElementExpectations.javascriptClick();
    log(expectations.getLogMessage());
    apply(expectations);
  }

  @Override
  public void flick(int xOffset, int yOffset) {
    ElementExpectations<Element> expectations = ElementExpectations.flick(xOffset, yOffset);
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
