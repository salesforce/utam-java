package utam.core.selenium.element;

import java.util.function.Supplier;
import org.openqa.selenium.Keys;
import org.openqa.selenium.SearchContext;
import org.openqa.selenium.WebDriver;
import utam.core.appium.element.GestureDirection;
import utam.core.appium.expectations.MobileExpectationsUtil;
import utam.core.framework.UtamLogger;
import utam.core.framework.consumer.UtamError;
import utam.core.framework.context.Driver;
import utam.core.selenium.context.SeleniumContext;
import utam.core.selenium.expectations.ElementExpectations;
import utam.core.selenium.expectations.ElementListExpectations;
import utam.core.selenium.expectations.ElementWait;
import utam.core.selenium.expectations.ExpectationsUtil;

class ElementImpl implements BaseElement, Actionable, Clickable, Editable, Touchable {

  private final LocatorImpl elementLocator;
  private final SeleniumContext context;
  private static final String ERR_INVALID_METHOD_CALL = "Method is applicable only for iOS/Android";

  ElementImpl(Locator locator, SeleniumContext context) {
    elementLocator = (LocatorImpl) locator;
    this.context = context;
  }

  private void log(String message) {
    UtamLogger.info(String.format("%s [%s]", message, elementLocator.getSelectorString()));
  }

  LocatorImpl getLocator() {
    return elementLocator;
  }

  // not private because it's used in LocatorUtilities.waitForPresence
  // which is non public method that later will be removed when UI interactions are refactored
  SeleniumContext getSeleniumContext() {
    return context;
  }

  @Override
  public void click() {
    ElementExpectations<SearchContext> expectation = ExpectationsUtil.click();
    log(expectation.getLogMessage());
    new ElementWaitImpl(expectation.getLogMessage(), elementLocator, context).wait(expectation);
  }

  @Override
  public void javascriptClick() {
    ElementExpectations<SearchContext> expectation = ExpectationsUtil.javascriptClick();
    log(expectation.getLogMessage());
    new ElementWaitImpl(expectation.getLogMessage(), elementLocator, context).wait(expectation);
  }

  @Override
  public void clear() {
    ElementExpectations<SearchContext> expectation = ExpectationsUtil.clear();
    log(expectation.getLogMessage());
    new ElementWaitImpl(expectation.getLogMessage(), elementLocator, context).wait(expectation);
  }

  @Override
  public void clearAndType(String text) {
    ElementExpectations<SearchContext> expectation = ExpectationsUtil.clearAndType(text);
    log(expectation.getLogMessage());
    new ElementWaitImpl(expectation.getLogMessage(), elementLocator, context).wait(expectation);
  }

  @Override
  public void waitForAbsence() {
    ElementListExpectations<Boolean> expectation = ExpectationsUtil.waitForAbsence();
    log(expectation.getLogMessage());
    new ElementWaitImpl(expectation.getLogMessage(), elementLocator, context).wait(expectation);
  }

  @Override
  public void waitForVisible() {
    ElementListExpectations<Boolean> expectation = ExpectationsUtil.waitForVisible();
    log(expectation.getLogMessage());
    new ElementWaitImpl(expectation.getLogMessage(), elementLocator, context).wait(expectation);
  }

  @Override
  public void waitForInvisible() {
    ElementListExpectations<Boolean> expectation = ExpectationsUtil.waitForInvisible();
    log(expectation.getLogMessage());
    new ElementWaitImpl(expectation.getLogMessage(), elementLocator, context).wait(expectation);
  }

  @Override
  public boolean isEnabled() {
    ElementListExpectations<ElementWait.Match> assertion = ExpectationsUtil.isEnabled();
    log(assertion.getLogMessage());
    return new ElementWaitImpl(assertion.getLogMessage(), elementLocator, context).match(assertion);
  }

  @Override
  public boolean isVisible() {
    ElementListExpectations<ElementWait.Match> assertion = ExpectationsUtil.isDisplayed();
    log(assertion.getLogMessage());
    return new ElementWaitImpl(assertion.getLogMessage(), elementLocator, context).match(assertion);
  }

  @Override
  public boolean isPresent() {
    ElementListExpectations<ElementWait.Match> assertion = ExpectationsUtil.isPresent();
    log(assertion.getLogMessage());
    return new ElementWaitImpl(assertion.getLogMessage(), elementLocator, context).match(assertion);
  }

  @Override
  public String getAttribute(String attribute) {
    ElementExpectations<String> expectation = ExpectationsUtil.getAttribute(attribute);
    log(expectation.getLogMessage());
    return new ElementWaitImpl(expectation.getLogMessage(), elementLocator, context)
        .wait(expectation);
  }

  @Override
  public String getText() {
    ElementExpectations<String> expectation = ExpectationsUtil.getText();
    log(expectation.getLogMessage());
    return new ElementWaitImpl(expectation.getLogMessage(), elementLocator, context)
        .wait(expectation);
  }

  @Override
  public void setText(String text) {
    ElementExpectations<SearchContext> expectation = ExpectationsUtil.setText(text);
    log(expectation.getLogMessage());
    new ElementWaitImpl(expectation.getLogMessage(), elementLocator, context).wait(expectation);
  }

  @Override
  public String getClassAttribute() {
    ElementExpectations<String> expectation = ExpectationsUtil.getAttribute("class");
    log(expectation.getLogMessage());
    return new ElementWaitImpl(expectation.getLogMessage(), elementLocator, context)
            .wait(expectation);
  }

  @Override
  public String getTitle() {
    ElementExpectations<String> expectation = ExpectationsUtil.getAttribute("title");
    log(expectation.getLogMessage());
    return new ElementWaitImpl(expectation.getLogMessage(), elementLocator, context)
        .wait(expectation);
  }

  @Override
  public String getValue() {
    ElementExpectations<String> expectation = ExpectationsUtil.getAttribute("value");
    log(expectation.getLogMessage());
    return new ElementWaitImpl(expectation.getLogMessage(), elementLocator, context)
        .wait(expectation);
  }

  @Override
  public void moveTo() {
    ElementExpectations<SearchContext> expectation = ExpectationsUtil.moveTo();
    log(expectation.getLogMessage());
    new ElementWaitImpl(expectation.getLogMessage(), elementLocator, context).wait(expectation);
  }

  SearchContext find(boolean isExpandShadowRoot) {
    ElementExpectations<SearchContext> expectation = ExpectationsUtil.find(isExpandShadowRoot);
    log(expectation.getLogMessage());
    return new ElementWaitImpl(expectation.getLogMessage(), elementLocator, context)
        .wait(expectation);
  }

  @Override
  public void scrollToCenter() {
    ElementExpectations<SearchContext> expectation = ExpectationsUtil.scrollToCenter();
    log(expectation.getLogMessage());
    new ElementWaitImpl(expectation.getLogMessage(), elementLocator, context).wait(expectation);
  }

  @Override
  public boolean isFocused() {
    ElementExpectations<ElementWait.Match> assertion = ExpectationsUtil.hasFocus();
    log(assertion.getLogMessage());
    return new ElementWaitImpl(assertion.getLogMessage(), elementLocator, context).match(assertion);
  }

  @Override
  public void focus() {
    ElementExpectations<SearchContext> expectation = ExpectationsUtil.focus();
    log(expectation.getLogMessage());
    new ElementWaitImpl(expectation.getLogMessage(), elementLocator, context).wait(expectation);
  }

  @Override
  @Deprecated // renamed, will cleanup
  public void scrollTo() {
    ElementExpectations<SearchContext> expectation = ExpectationsUtil.scrollTo();
    log(expectation.getLogMessage());
    new ElementWaitImpl(expectation.getLogMessage(), elementLocator, context).wait(expectation);
  }

  @Override
  public void scrollToTop() {
    ElementExpectations<SearchContext> expectation = ExpectationsUtil.scrollTo();
    log(expectation.getLogMessage());
    new ElementWaitImpl(expectation.getLogMessage(), elementLocator, context).wait(expectation);
  }

  @Override
  public void blur() {
    ElementExpectations<SearchContext> expectation = ExpectationsUtil.blur();
    log(expectation.getLogMessage());
    new ElementWaitImpl(expectation.getLogMessage(), elementLocator, context).wait(expectation);
  }

  @Override
  public <T> T waitFor(Supplier<T> condition) {
    ElementExpectations<T> expectation = ExpectationsUtil.waitFor(condition);
    log(expectation.getLogMessage());
    return new ElementWaitImpl(expectation.getLogMessage(), elementLocator, context).wait(expectation);
  }

  @Override
  public boolean containsElement(Selector selector, boolean isExpandShadow) {
    // try to find how many elements are there, if 0 - nothing found
    ElementExpectations<Integer> expectation = ExpectationsUtil.findElements(selector, isExpandShadow);
    log(expectation.getLogMessage());
    return new ElementWaitImpl("check for element containing", elementLocator, context)
            .wait(expectation) > 0;
  }

  @Override
  public boolean containsElement(Selector selector) {
    return containsElement(selector, false);
  }

  @Override
  public void press(CharSequence key) {
    Keys keyToPress = Keys.valueOf(key.toString().toUpperCase());
    ElementExpectations<SearchContext> expectation = ExpectationsUtil.setText(keyToPress.toString());
    log(String.format("press keyboard key '%s'", keyToPress.name()));
    new ElementWaitImpl(expectation.getLogMessage(), elementLocator, context).wait(expectation);
  }

  private void validateDriverForTouchAction() {
    WebDriver driver = context.getWebDriverUtils().getWebDriver();
    if (!Driver.isMobileDriver(driver)) {
      throw new UtamError(ERR_INVALID_METHOD_CALL);
    }
  }

  @Override
  public void flick(int xOffset, int yOffset) {
    validateDriverForTouchAction();
    ElementExpectations<SearchContext> expectation = MobileExpectationsUtil.flick(xOffset, yOffset);
    log(expectation.getLogMessage());
    new ElementWaitImpl(expectation.getLogMessage(), elementLocator, context).wait(expectation);
  }

  @Override
  public boolean flickItems(GestureDirection direction) {
    // TODO Auto-generated method stub
    return false;
  }
}
