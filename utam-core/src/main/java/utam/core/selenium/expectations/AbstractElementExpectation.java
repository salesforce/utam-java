package utam.core.selenium.expectations;

import org.openqa.selenium.SearchContext;
import org.openqa.selenium.WebElement;
import utam.core.selenium.context.WebDriverUtilities;

import java.util.function.*;

/**
 * @author elizaveta.ivanova
 * @since 226
 */
public abstract class AbstractElementExpectation<T> implements ElementExpectations<T> {

  private final String log;
  private final BiFunction<WebDriverUtilities, SearchContext, T> apply;
  private final T defaultReturns;


  AbstractElementExpectation(String log, BiFunction<WebDriverUtilities, SearchContext, T> apply) {
    this(log, apply, null);
  }

  AbstractElementExpectation(String log, BiFunction<WebDriverUtilities, SearchContext, T> apply, T defaultReturns) {
    this.log = log;
    this.apply = apply;
    this.defaultReturns = defaultReturns;
  }

  @Override
  public Function<SearchContext, T> apply(WebDriverUtilities utilities) {
    return webElement -> this.apply.apply(utilities, webElement);
  }

  @Override
  public String getLogMessage() {
    return log;
  }

  @Override
  public T returnIfNothingFound() {
    return defaultReturns;
  }

  static class ReturnsString extends AbstractElementExpectation<String> {
    ReturnsString(String log, Function<WebElement, String> apply) {
      super(log, (utilities, searchContext) -> apply.apply((WebElement) searchContext));
    }
  }

  public static class ConsumesUtilsElement extends AbstractElementExpectation<SearchContext> {

    public ConsumesUtilsElement(String log, BiConsumer<WebDriverUtilities, WebElement> apply) {
      super(log, (utilities, searchContext) -> {
                apply.accept(utilities, (WebElement) searchContext);
                return searchContext;
              });
    }
  }

  static class ConsumesElement extends AbstractElementExpectation<SearchContext> {

    ConsumesElement(String log, Consumer<WebElement> apply) {
      super(log, (utilities, context) -> {
        apply.accept((WebElement) context);
        return context;
      });
    }
  }

  static class ReturnsElement extends AbstractElementExpectation<SearchContext> {

    ReturnsElement(String log, BiFunction<WebDriverUtilities, SearchContext, SearchContext> apply) {
      super(log, apply);
    }
  }

  static class Predicate extends AbstractElementExpectation<ElementWait.Match> {

    Predicate(String log, BiPredicate<WebDriverUtilities, SearchContext> apply) {
      super(log, ((utilities, searchContext) -> ElementWait.Match.from(apply.test(utilities, searchContext))), ElementWait.Match.FALSE);
    }
  }
}
