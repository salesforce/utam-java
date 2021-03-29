package utam.core.selenium.element;

import java.time.Duration;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.openqa.selenium.InvalidElementStateException;
import org.openqa.selenium.JavascriptException;
import org.openqa.selenium.NoSuchElementException;
import org.openqa.selenium.NotFoundException;
import org.openqa.selenium.StaleElementReferenceException;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.support.ui.FluentWait;
import utam.core.framework.consumer.UtamError;
import utam.core.framework.consumer.UtamTimeouts;
import utam.core.selenium.context.SeleniumContext;
import utam.core.selenium.context.WebDriverUtilities;
import utam.core.selenium.expectations.ElementExpectations;
import utam.core.selenium.expectations.ElementListExpectations;
import utam.core.selenium.expectations.ElementWait;

/**
 * FluentWait implementation for chained locators
 *
 * @author elizaveta.ivanova
 * @since 224
 */
class ElementWaitImpl implements ElementWait {

  private static final List<Class<? extends Throwable>> IGNORE_EXCEPTIONS =
      Stream.of(
          JavascriptException.class,
          NotFoundException.class,
          NoSuchElementException.class,
          StaleElementReferenceException.class,
          InvalidElementStateException.class)
          .collect(Collectors.toList());
  private final LocatorImpl locator;
  private final UtamTimeouts timeouts;
  private final WebDriverUtilities utilities;
  private final String expectationsMessage;

  ElementWaitImpl(String expectationsMessage, Locator locator, SeleniumContext seleniumContext) {
    this.locator = (LocatorImpl) locator;
    this.timeouts = seleniumContext.getTimeouts();
    this.utilities = seleniumContext.getWebDriverUtils();
    this.expectationsMessage = expectationsMessage;
  }

  private List<WebElement> findList(boolean isNullable) {
    return new LocatorWait(
        locator,
        "find elements",
        timeouts.getFindTimeout(),
        timeouts.getPollingInterval())
        .until(
            locator -> locator.find(utilities, LocatorUtilities.Find.getFilteredList(isNullable)));
  }

  private <T> T applyToList(List<WebElement> elements,
      boolean isNullable,
      Duration timeout,
      ElementListExpectations<T> expectations) {
    return new LocatorWait(
        locator,
        expectationsMessage,
        timeout,
        timeouts.getPollingInterval())
        .until(
            locator -> {
              if (elements.isEmpty() && isNullable) {
                return expectations.returnIfNothingFound();
              }
              return expectations.apply(utilities).apply(elements);
            });
  }

  private WebElement findElement(boolean isNullable) {
    return new LocatorWait(
        locator,
        expectationsMessage,
        timeouts.getFindTimeout(),
        timeouts.getPollingInterval())
        .until(
            locator -> {
              List<WebElement> foundElements =
                  locator.find(utilities, LocatorUtilities.Find.getFilteredList(isNullable));
              if (isNullable && foundElements.isEmpty()) {
                return null;
              }
              return foundElements.iterator().next();
            });
  }

  private <T> T applyToElement(
      WebElement element,
      boolean isNullable,
      Duration timeout,
      ElementExpectations<T> expectations) {
    return new LocatorWait(
        locator,
        expectationsMessage,
        timeout,
        timeouts.getPollingInterval())
        .until(
            locator -> {
              if (isNullable && element == null) {
                return expectations.returnIfNothingFound();
              }
              return expectations.apply(utilities).apply(element);
            });
  }

  @Override
  public <T> T wait(ElementListExpectations<T> expectations) {
    boolean isNullable = expectations.returnIfNothingFound() != null;
    List<WebElement> found = findList(isNullable);
    return applyToList(found, isNullable, timeouts.getWaitForTimeout(), expectations);
  }

  @Override
  public <T> T wait(ElementExpectations<T> expectation) {
    boolean isNullable = expectation.returnIfNothingFound() != null;
    WebElement element = findElement(isNullable);
    return applyToElement(element, isNullable, timeouts.getWaitForTimeout(), expectation);
  }

  @Override
  public <T> T apply(ElementListExpectations<T> expectations) {
    boolean isNullable = expectations.returnIfNothingFound() != null;
    List<WebElement> found = findList(isNullable);
    return applyToList(found, isNullable, timeouts.getFluentWaitTimeout(), expectations);
  }

  @Override
  public <T> T apply(ElementExpectations<T> expectation) {
    boolean isNullable = expectation.returnIfNothingFound() != null;
    WebElement element = findElement(isNullable);
    return applyToElement(element, isNullable, timeouts.getFluentWaitTimeout(), expectation);
  }

  @Override
  public boolean match(ElementExpectations<Match> expectation) {
    boolean isNullable = expectation.returnIfNothingFound() != null;
    WebElement element = findElement(isNullable);
    if (isNullable && element == null) {
      return expectation.returnIfNothingFound().is();
    }
    return expectation.apply(utilities).apply(element).is();
  }

  @Override
  public boolean match(ElementListExpectations<Match> expectation) {
    boolean isNullable = expectation.returnIfNothingFound() != null;
    List<WebElement> found = findList(isNullable);
    if (isNullable && (found == null || found.isEmpty())) {
      return expectation.returnIfNothingFound().is();
    }
    return expectation.apply(utilities).apply(found).is();
  }

  /**
   * customized class for fluent wait <br> extends Selenium.FluentWait
   */
  private static class LocatorWait extends FluentWait<LocatorImpl> {

    static final String ERR_MESSAGE =
        "failure during '%s' (tried for %d sec with %d msec interval)";

    private final String error;

    LocatorWait(
        LocatorImpl locator,
        String expectationsMessage,
        Duration timeout,
        Duration pollInterval) {
      super(locator);
      error =
          locator.getErrorPrefix()
              + String.format(
              ERR_MESSAGE,
              expectationsMessage,
              timeout.getSeconds(),
              pollInterval.getSeconds() * 1000);
      this
          // had to use units instead duration for compatibility with SETI
          .withTimeout(timeout)
          .pollingEvery(pollInterval)
          .ignoreAll(IGNORE_EXCEPTIONS);
    }

    @Override // apply custom logic for error handling
    protected final RuntimeException timeoutException(String message, Throwable lastException) {
      // original message is useless, so excluded
      throw new UtamError(error, lastException);
    }
  }
}
