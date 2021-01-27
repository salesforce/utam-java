package selenium.element;

import framework.consumer.UtamError;
import org.openqa.selenium.By;
import org.openqa.selenium.NotFoundException;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.interactions.Interactive;
import org.testng.annotations.Test;
import selenium.context.SeleniumContext;
import selenium.context.WebDriverUtilities;
import selenium.expectations.ElementExpectations;
import selenium.expectations.ElementListExpectations;
import selenium.expectations.ElementWait;

import java.time.Duration;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.stream.Collectors;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;
import static org.mockito.Mockito.any;
import static org.mockito.Mockito.*;
import static org.testng.Assert.expectThrows;
import static selenium.element.LocatorImpl.LOCATOR_CHAIN_SEPARATOR;
import static selenium.element.LocatorNodeImpl.ERR_CANT_FIND_FILTERED_ELEMENTS;
import static selenium.element.LocatorNodeImpl.ERR_ELEMENT_SELECTED_BY;
import static selenium.element.LocatorUtilities.EMPTY_FILTER;

/**
 * Provides tests for the ElementWaitImpl class
 *
 * @author james.evans
 */
public class ElementWaitImplTests {

  private static final String SAMPLE_TEXT = "sample text used to test input elements";
  private static final String PARENT_LOCATOR = ".parent";
  private static final String LOG_MESSAGE = "test text is expected";
  private static final String MOCK_ELEMENT_LOCATOR_STRING = "locator";
  private static final String NONEXISTENT_ELEMENT_LOCATOR_STRING = "Does not Exist";

  private static final LocatorImpl EXISTING_ELEMENT_LOCATOR =
      new LocatorImpl(new LocatorNodeImpl.Css(MOCK_ELEMENT_LOCATOR_STRING));
  private static final LocatorImpl NONEXISTENT_ELEMENT_LOCATOR =
      new LocatorImpl(new LocatorNodeImpl.Css(NONEXISTENT_ELEMENT_LOCATOR_STRING));

  @SuppressWarnings("unchecked")
  private static <T> ElementExpectations<T> getExpectation(
      Function<WebElement, T> apply, T returnIfNotFound) {
    ElementExpectations<T> expectation = mock(ElementExpectations.class);
    when(expectation.getLogMessage()).thenReturn(LOG_MESSAGE);
    when(expectation.returnIfNothingFound()).thenReturn(returnIfNotFound);
    when(expectation.apply(any(WebDriverUtilities.class)))
        .thenReturn(searchContext -> apply.apply((WebElement) searchContext));
    return expectation;
  }

  /**
   * The match method should return a valid value when using a single element expectation and a
   * match occurs
   */
  @Test
  public void testMatchWithElementExpectation() {
    MockHelper mockHelper = new MockHelper();
    ElementExpectations<ElementWait.Match> expectation =
        getExpectation(
            element -> ElementWait.Match.from(element.getText().equals(SAMPLE_TEXT)),
            ElementWait.Match.FALSE);
    ElementWaitImpl wait = mockHelper.getWait(EXISTING_ELEMENT_LOCATOR);
    assertThat(wait.match(expectation), is(equalTo(true)));
  }

  /**
   * The match method should return a valid value when using a single element expectation and no
   * match is found
   */
  @Test
  public void testMatchWithElementExpectationWithNoMatch() {
    ElementExpectations<ElementWait.Match> expectation =
        getExpectation(
            element -> ElementWait.Match.from(element.getText().equals("No test")),
            ElementWait.Match.FALSE);
    MockHelper mockHelper = new MockHelper();
    assertThat(
        mockHelper.getWait(NONEXISTENT_ELEMENT_LOCATOR).match(expectation), is(equalTo(false)));
  }

  /**
   * The match method should return a valid value when using a single element expectation that
   * returns null if no match is found, and a match occurs
   */
  @Test
  public void testMatchWithElementExpectationWithNullable() {
    MockHelper mockHelper = new MockHelper();
    ElementExpectations<ElementWait.Match> expectation =
        getExpectation(
            (webelement) -> ElementWait.Match.from(webelement.getText().equals(SAMPLE_TEXT)), null);

    ElementWaitImpl wait = mockHelper.getWait(EXISTING_ELEMENT_LOCATOR);
    assertThat(wait.match(expectation), is(equalTo(true)));
  }

  /**
   * The match method should throw the proper exception when using a single element expectation and
   * the expectation function throws an exception
   */
  @Test
  public void testMatchWithElementExpectationWhenMatchMethodThrows() {
    MockHelper mockHelper = new MockHelper();
    ElementExpectations<ElementWait.Match> expectation =
        getExpectation(
            element -> {
              throw new NotFoundException(
                  EXISTING_ELEMENT_LOCATOR.getErrorPrefix() + "Forced Error");
            },
            ElementWait.Match.FALSE);

    ElementWaitImpl wait = mockHelper.getWait(EXISTING_ELEMENT_LOCATOR);
    assertThat(wait.match(expectation), is(equalTo(false)));
  }

  /**
   * The match method should return a valid value when using an element list expectation and a match
   * occurs
   */
  @Test
  public void testMatchWithElementListExpectation() {
    MockHelper mockHelper = new MockHelper();
    ElementListExpectations<ElementWait.Match> expectation =
        mockHelper.getListExpectation(
            list ->
                ElementWait.Match.from(
                    list.stream().anyMatch((element) -> element.getText().equals(SAMPLE_TEXT))),
            ElementWait.Match.FALSE);

    ElementWaitImpl wait = mockHelper.getWait(EXISTING_ELEMENT_LOCATOR);
    assertThat(wait.match(expectation), is(equalTo(true)));
  }

  /**
   * The match method should return a valid value when using an element list expectation and no
   * match is found
   */
  @Test
  public void testMatchWithElementListExpectationWithNoMatch() {
    MockHelper mockHelper = new MockHelper();
    ElementListExpectations<ElementWait.Match> expectation =
        mockHelper.getListExpectation(
            elements ->
                ElementWait.Match.from(
                    elements.stream().anyMatch((element) -> element.getText().equals("no text"))),
            ElementWait.Match.FALSE);

    ElementWaitImpl wait = mockHelper.getWait(NONEXISTENT_ELEMENT_LOCATOR);
    assertThat(wait.match(expectation), is(equalTo(false)));
  }

  /**
   * The match method should return a valid value when using an element list expectation that
   * returns null if no match is found, and a match occurs
   */
  @Test
  public void testMatchWithElementListExpectationWithNullable() {
    MockHelper mockHelper = new MockHelper();
    ElementListExpectations<ElementWait.Match> expectation =
        mockHelper.getListExpectation(
            elements ->
                ElementWait.Match.from(
                    elements.stream().anyMatch((element) -> element.getText().equals(SAMPLE_TEXT))),
            null);

    ElementWaitImpl wait = mockHelper.getWait(EXISTING_ELEMENT_LOCATOR);
    assertThat(wait.match(expectation), is(equalTo(true)));
  }

  /**
   * The match method should throw the proper exception when using an element list expectation and
   * the expectation function throws an exception
   */
  @Test
  public void testMatchWithElementListExpectationWhenMatchMethodThrows() {
    MockHelper mockHelper = new MockHelper();
    ElementListExpectations<ElementWait.Match> expectation =
        mockHelper.getListExpectation(
            elements ->
                ElementWait.Match.from(
                    elements.stream()
                        .anyMatch(
                            element -> {
                              throw new NotFoundException(
                                  EXISTING_ELEMENT_LOCATOR.getErrorPrefix() + "Forced Error");
                            })),
            ElementWait.Match.FALSE);

    ElementWaitImpl wait = mockHelper.getWait(EXISTING_ELEMENT_LOCATOR);
    assertThat(wait.match(expectation), is(equalTo(false)));
  }

  /**
   * The wait method should return a valid value when using a single element expectation and a match
   * occurs
   */
  @Test
  public void testWaitWithElementExpectation() {
    MockHelper mockHelper = new MockHelper();
    ElementExpectations<String> expectation = getExpectation(WebElement::getText, "");

    ElementWaitImpl wait = mockHelper.getWait(EXISTING_ELEMENT_LOCATOR);
    assertThat(wait.wait(expectation), is(equalTo(SAMPLE_TEXT)));
  }

  /**
   * The wait method should return a valid value when using a single element expectation and no
   * match is found
   */
  @Test
  public void testWaitWithElementExpectationWithNoMatch() {
    MockHelper mockHelper = new MockHelper();
    ElementExpectations<String> expectation = getExpectation(WebElement::getText, "");

    ElementWaitImpl wait = mockHelper.getWait(NONEXISTENT_ELEMENT_LOCATOR);
    assertThat(wait.wait(expectation), is(emptyString()));
  }

  /**
   * The wait method should return a valid value when using a single element expectation that
   * returns null if no match is found, and a match occurs
   */
  @Test
  public void testWaitWithElementExpectationWithNonNullable() {
    MockHelper mockHelper = new MockHelper();
    ElementExpectations<String> expectation = getExpectation(WebElement::getText, null);

    ElementWaitImpl wait = mockHelper.getWait(EXISTING_ELEMENT_LOCATOR);
    assertThat(wait.wait(expectation), is(equalTo(SAMPLE_TEXT)));
  }

  /**
   * The wait method should return a valid value when using an element list expectation and a match
   * occurs
   */
  @Test
  public void testWaitWithElementListExpectation() {
    MockHelper mockHelper = new MockHelper();
    ElementListExpectations<String> expectation =
        mockHelper.getListExpectation(
            elements ->
                elements.stream()
                    .filter((element) -> element.getText().equals(SAMPLE_TEXT))
                    .map(WebElement::getText)
                    .collect(Collectors.joining(",")),
            "");

    ElementWaitImpl wait = mockHelper.getWait(EXISTING_ELEMENT_LOCATOR);
    assertThat(wait.wait(expectation), is(equalTo(SAMPLE_TEXT)));
  }

  /**
   * The wait method should return a valid value when using an element list expectation and no match
   * is found
   */
  @Test
  public void testWaitWithElementListExpectationWithNoMatch() {
    MockHelper mockHelper = new MockHelper();
    ElementListExpectations<String> expectation =
        mockHelper.getListExpectation(
            elements ->
                elements.stream()
                    .filter((element) -> element.getText().equals("not found"))
                    .map(WebElement::getText)
                    .collect(Collectors.joining(",")),
            "");

    ElementWaitImpl wait = mockHelper.getWait(NONEXISTENT_ELEMENT_LOCATOR);
    assertThat(wait.wait(expectation), is(emptyString()));
  }

  /**
   * The wait method should return a valid value when using an element list expectation that returns
   * null if no match is found, and a match occurs
   */
  @Test
  public void testWaitWithElementListExpectationWithNonNullable() {
    MockHelper mockHelper = new MockHelper();
    ElementListExpectations<String> expectation =
        mockHelper.getListExpectation(
            elements ->
                elements.stream()
                    .filter((element) -> element.getText().equals(SAMPLE_TEXT))
                    .map(WebElement::getText)
                    .collect(Collectors.joining(",")),
            null);

    ElementWaitImpl wait = mockHelper.getWait(EXISTING_ELEMENT_LOCATOR);
    assertThat(wait.wait(expectation), is(equalTo(SAMPLE_TEXT)));
  }

  /** The wait method with a scoped locator should return a valid value */
  @Test
  public void testWaitWithElementInScopedLocator() {
    MockHelper mockHelper = new MockHelper();
    WebElement parentElement = mock(WebElement.class);
    when(parentElement.findElements(By.cssSelector(MOCK_ELEMENT_LOCATOR_STRING)))
        .thenReturn(new ArrayList<>(Collections.singletonList(mockHelper.mockElement)));
    when(mockHelper.mockDriver.findElements(By.cssSelector(PARENT_LOCATOR)))
        .thenReturn(new ArrayList<>(Collections.singletonList(parentElement)));
    when(mockHelper.mockElement.getText()).thenReturn(SAMPLE_TEXT);

    ElementExpectations<String> expectation = getExpectation(WebElement::getText, null);
    LocatorImpl locator =
        new LocatorImpl(
            new LocatorNodeImpl.Css(PARENT_LOCATOR),
            new LocatorNodeImpl.Css(
                MOCK_ELEMENT_LOCATOR_STRING, EMPTY_FILTER));

    ElementWaitImpl wait = mockHelper.getWait(locator);
    assertThat(wait.wait(expectation), is(equalTo(SAMPLE_TEXT)));
  }

  /**
   * The wait method with a scoped locator with an expectation that returns null when the
   * expectation is not met should return a valid value
   */
  @Test
  public void testWaitWithNonNullableWithElementInScopedLocator() {
    MockHelper mockHelper = new MockHelper();
    WebElement parentElement = mock(WebElement.class, withSettings().name("parent"));
    when(parentElement.findElements(By.cssSelector(".child")))
        .thenReturn(new ArrayList<>(Collections.singletonList(mockHelper.mockElement)));

    when(mockHelper.mockDriver.findElements(By.cssSelector(".parent")))
        .thenReturn(new ArrayList<>(Collections.singletonList(parentElement)));

    ElementExpectations<String> expectation = getExpectation(WebElement::getText, SAMPLE_TEXT);
    Locator locator =
        new LocatorImpl(
            new LocatorNodeImpl.Css(".parent"),
            new LocatorNodeImpl.Css(".child"));

    ElementWaitImpl wait = mockHelper.getWait(locator);
    assertThat(wait.wait(expectation), is(equalTo(SAMPLE_TEXT)));
  }

  /**
   * The wait method with a scoped locator with an expectation that is not met should return a valid
   * value
   */
  @Test
  public void testWaitWitElementInScopedLocatorNotMatched() {
    MockHelper mockHelper = new MockHelper();
    WebElement parentElement = mock(WebElement.class, withSettings().name("parent"));
    when(parentElement.findElements(By.cssSelector(MOCK_ELEMENT_LOCATOR_STRING)))
        .thenReturn(new ArrayList<>(Collections.singletonList(mockHelper.mockElement)));

    when(mockHelper.mockDriver.findElements(By.cssSelector(PARENT_LOCATOR)))
        .thenReturn(new ArrayList<>(Collections.singletonList(parentElement)));

    ElementExpectations<String> expectation = getExpectation(WebElement::getText, "");

    LocatorNode locator =
        new LocatorNodeImpl.Css(NONEXISTENT_ELEMENT_LOCATOR_STRING, EMPTY_FILTER, ShadowBoundary.NONE);

    LocatorImpl parentLocator = new LocatorImpl(new LocatorNodeImpl.Css(PARENT_LOCATOR));
    LocatorImpl scopedLocator = parentLocator.add(locator);

    ElementWaitImpl wait = mockHelper.getWait(scopedLocator);
    assertThat(wait.wait(expectation), is(emptyString()));
  }

  /**
   * The wait method with a scoped locator with an expectation that returns null when the
   * expectation is not met should throw the appropriate exception when the expectation is not met
   */
  @Test
  public void testWaitWithNonNullableWithElementInScopedLocatorNotMatchedThrows() {
    MockHelper mockHelper = new MockHelper();
    WebElement parentElement = mock(WebElement.class, withSettings().name("parent"));
    when(parentElement.findElements(By.cssSelector(MOCK_ELEMENT_LOCATOR_STRING)))
        .thenReturn(new ArrayList<>(Collections.singletonList(mockHelper.mockElement)));

    when(mockHelper.mockDriver.findElements(By.cssSelector(PARENT_LOCATOR)))
        .thenReturn(new ArrayList<>(Collections.singletonList(parentElement)));

    ElementExpectations<String> expectation = getExpectation(WebElement::getText, null);

    LocatorNode locator =
        new LocatorNodeImpl.Css(NONEXISTENT_ELEMENT_LOCATOR_STRING, EMPTY_FILTER, ShadowBoundary.NONE);

    LocatorImpl parentLocator = new LocatorImpl(new LocatorNodeImpl.Css(PARENT_LOCATOR));
    LocatorImpl scopedLocator = parentLocator.add(locator);

    ElementWaitImpl wait = mockHelper.getWait(scopedLocator);
    UtamError e = expectThrows(UtamError.class, () -> wait.wait(expectation));
    assertThat(
        e.getMessage(),
        containsString(
            String.format(
                "element selected by '%s",
                PARENT_LOCATOR + LOCATOR_CHAIN_SEPARATOR + NONEXISTENT_ELEMENT_LOCATOR_STRING)));
  }

  /**
   * The wait method with a scoped locator should throw the appropriate exception when the element
   * specified by the expectation cannot be found
   */
  @Test
  public void testWaitWithElementInScopedLocatorNotFoundThrows() {
    MockHelper mockHelper = new MockHelper();
    ElementExpectations<String> expectation = getExpectation(WebElement::getText, null);

    LocatorImpl locator =
        new LocatorImpl(new LocatorNodeImpl.Css(".parent"), new LocatorNodeImpl.Css(".child"));

    ElementWaitImpl wait = mockHelper.getWait(locator);
    UtamError e = expectThrows(UtamError.class, () -> wait.wait(expectation));
    assertThat(
        e.getMessage(),
        containsString(
            String.format(
                ERR_ELEMENT_SELECTED_BY, ".parent" + LOCATOR_CHAIN_SEPARATOR + ".child")));
  }

  /**
   * The wait method with an expectation that returns null when the expectation is not met should
   * return a valid value on a locator that contains a filter
   */
  @Test
  public void testWaitWithNonNullableExpectationAndFilter() {
    MockHelper mockHelper = new MockHelper();
    ElementExpectations<String> expectation = getExpectation(WebElement::getText, null);

    LocatorImpl locator =
        new LocatorImpl(
            new LocatorNodeImpl.Css(MOCK_ELEMENT_LOCATOR_STRING, EMPTY_FILTER, ShadowBoundary.NONE));

    ElementWaitImpl wait = mockHelper.getWait(locator);
    assertThat(wait.wait(expectation), is(equalTo(SAMPLE_TEXT)));
  }

  /**
   * The wait method with an expectation that returns null when the expectation is not met should
   * throw the appropriate exception on locator that contains a filter which returns null
   */
  @Test
  public void testWaitWithNonNullableExpectationAndNullFilterThrows() {
    MockHelper mockHelper = new MockHelper();
    ElementExpectations<String> expectation = getExpectation(WebElement::getText, null);
    LocatorNodeFilter filter = mock(LocatorNodeFilter.class);
    final String FILTER_STRING = "Fake filter";
    when(filter.getFilterString()).thenReturn(FILTER_STRING);
    LocatorImpl locator =
        new LocatorImpl(new LocatorNodeImpl.Css(MOCK_ELEMENT_LOCATOR_STRING, filter));
    UtamError e =
        expectThrows(UtamError.class, () -> mockHelper.getWait(locator).wait(expectation));
    assertThat(
        e.getMessage(),
        containsString(
            String.format(ERR_ELEMENT_SELECTED_BY, MOCK_ELEMENT_LOCATOR_STRING + FILTER_STRING)));
    assertThat(
        e.getCause().getMessage(),
        containsString(String.format(ERR_CANT_FIND_FILTERED_ELEMENTS, FILTER_STRING)));
  }

  /**
   * The wait method with an expectation that returns null when the expectation is not met should
   * throw the appropriate exception on locator that contains a filter which returns an empty set of
   * filtered values
   */
  @Test
  public void testWaitWithNonNullableExpectationAndEmptyFilterThrows() {
    MockHelper mockHelper = new MockHelper();
    ElementExpectations<String> expectation = getExpectation(WebElement::getText, null);
    LocatorImpl locator =
        new LocatorImpl(new LocatorNodeImpl.Css(MOCK_ELEMENT_LOCATOR_STRING, new LocatorNodeFilter.Index(75)));

    ElementWaitImpl wait = mockHelper.getWait(locator);
    expectThrows(UtamError.class, () -> wait.wait(expectation));
  }

  static class MockHelper {
    final SeleniumContext context;
    final WebDriverUtilities utilities;
    final WebDriver mockDriver;
    final WebElement mockElement;

    MockHelper() {
      mockDriver = mock(WebDriver.class, withSettings().extraInterfaces(Interactive.class));
      context = mock(SeleniumContext.class);
      utilities = mock(WebDriverUtilities.class);

      mockElement = mock(WebElement.class, withSettings().name("element"));
      when(mockElement.getText()).thenReturn(SAMPLE_TEXT);
      when(context.getWebDriverUtils()).thenReturn(utilities);
      when(context.getPollingTimeout()).thenReturn(Duration.ofMillis(10));
      when(context.getPollingInterval()).thenReturn(Duration.ofMillis(1));
      when(utilities.getWebDriver()).thenReturn(mockDriver);
      when(context
              .getWebDriverUtils()
              .getWebDriver()
              .findElement(By.cssSelector(MOCK_ELEMENT_LOCATOR_STRING)))
          .thenReturn(mockElement);
      when(context
              .getWebDriverUtils()
              .getWebDriver()
              .findElements(By.cssSelector(MOCK_ELEMENT_LOCATOR_STRING)))
          .thenReturn(new ArrayList<>(Collections.singletonList(mockElement)));
    }

    ElementWaitImpl getWait(Locator locator) {
      return new ElementWaitImpl("wait message", locator, context);
    }

    @SuppressWarnings("unchecked")
    private <T> ElementListExpectations<T> getListExpectation(
        Function<List<WebElement>, T> apply, T returnIfNotFound) {
      BiFunction<WebDriverUtilities, List<WebElement>, T> biFunction = (wu, we) -> apply.apply(we);
      ElementListExpectations<T> expectation = mock(ElementListExpectations.class);
      when(expectation.getLogMessage()).thenReturn(LOG_MESSAGE);
      when(expectation.returnIfNothingFound()).thenReturn(returnIfNotFound);
      when(expectation.apply(any(WebDriverUtilities.class)))
          .thenReturn(webElement -> biFunction.apply(utilities, webElement));
      return expectation;
    }
  }
}
