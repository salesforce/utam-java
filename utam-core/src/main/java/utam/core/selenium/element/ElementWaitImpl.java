package utam.core.selenium.element;

import utam.core.framework.consumer.UtamError;
import org.openqa.selenium.*;
import org.openqa.selenium.support.ui.FluentWait;
import utam.core.selenium.context.SeleniumContext;
import utam.core.selenium.context.WebDriverUtilities;
import utam.core.selenium.expectations.ElementExpectations;
import utam.core.selenium.expectations.ElementListExpectations;
import utam.core.selenium.expectations.ElementWait;

import java.time.Duration;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

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
  private final SeleniumContext seleniumContext;
  private final WebDriverUtilities utilities;
  private final String expectationsMessage;

  ElementWaitImpl(String expectationsMessage, Locator locator, SeleniumContext seleniumContext) {
    this.locator = (LocatorImpl) locator;
    this.seleniumContext = seleniumContext;
    this.utilities = seleniumContext.getWebDriverUtils();
    this.expectationsMessage = expectationsMessage;
  }

  @Override
  public <T> T wait(ElementListExpectations<T> expectations) {
    boolean isNullable = expectations.returnIfNothingFound() != null;
    return new LocatorWait(
            locator,
            expectationsMessage,
            seleniumContext.getPollingTimeout(),
            seleniumContext.getPollingInterval())
        .until(
            locator -> {
              List<WebElement> foundElements =
                  locator.find(utilities, LocatorUtilities.Find.getFilteredList(isNullable));
              if (foundElements.isEmpty() && isNullable) {
                return expectations.returnIfNothingFound();
              }
              return expectations.apply(utilities).apply(foundElements);
            });
  }

  @Override
  public <T> T wait(ElementExpectations<T> expectation) {
    boolean isNullable = expectation.returnIfNothingFound() != null;
    return new LocatorWait(
            locator,
            expectationsMessage,
            seleniumContext.getPollingTimeout(),
            seleniumContext.getPollingInterval())
        .until(
            locator -> {
              List<WebElement> foundElements =
                  locator.find(utilities, LocatorUtilities.Find.getFilteredList(isNullable));
              if (isNullable && foundElements.isEmpty()) {
                return expectation.returnIfNothingFound();
              }
              return expectation.apply(utilities).apply(foundElements.iterator().next());
            });
  }

  @Override
  public boolean match(ElementExpectations<Match> expectation) {
    boolean isNullable = expectation.returnIfNothingFound() != null;
    Match res;
    try {
      res =
          new LocatorImmediateWait(locator, expectationsMessage)
              .until(
                  locator -> {
                    List<WebElement> foundElements =
                        locator.find(utilities, LocatorUtilities.Find.getFilteredList(isNullable));
                    if (isNullable && foundElements.isEmpty()) {
                      return expectation.returnIfNothingFound();
                    }
                    return Match.from(
                        expectation.apply(utilities).apply(foundElements.iterator().next()).is());
                  });
    } catch (Exception e) {
      res = expectation.returnIfNothingFound();
    }
    return res.is();
  }

  @Override
  public boolean match(ElementListExpectations<Match> expectation) {
    boolean isNullable = expectation.returnIfNothingFound() != null;
    Match res;
    try {
      res =
          new LocatorImmediateWait(locator, expectationsMessage)
              .until(
                  locator -> {
                    List<WebElement> foundElements =
                        locator.find(utilities, LocatorUtilities.Find.getFilteredList(isNullable));
                    if (foundElements.isEmpty() && isNullable) {
                      return expectation.returnIfNothingFound();
                    }
                    return expectation.apply(utilities).apply(foundElements);
                  });
    } catch (Exception e) {
      res = expectation.returnIfNothingFound();
    }
    return res.is();
  }

    /**
   * some expectations calls do not wait <br>
   * for example checking for presence
   */
  private static class LocatorImmediateWait extends LocatorWait {

    LocatorImmediateWait(LocatorImpl locator, String expectationsMessage) {
      super(locator, expectationsMessage, Duration.ZERO, Duration.ZERO);
    }
  }

  /**
   * customized class for fluent wait <br>
   * extends Selenium.FluentWait
   */
  private static class LocatorWait extends FluentWait<LocatorImpl> {

    static final String ERR_MESSAGE =
        "failure during '%s' (tried for %d sec with %d msec interval)";

    private final String error;

    LocatorWait(
        LocatorImpl locator,
        String expectationsMessage,
        Duration assertionTimeout,
        Duration pollInterval) {
      super(locator);
      error =
          locator.getErrorPrefix()
              + String.format(
                  ERR_MESSAGE,
                  expectationsMessage,
                  assertionTimeout.getSeconds(),
                  pollInterval.getSeconds() * 1000);
      this
          // had to use units instead duration for compatibility with SETI
          .withTimeout(assertionTimeout)
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
