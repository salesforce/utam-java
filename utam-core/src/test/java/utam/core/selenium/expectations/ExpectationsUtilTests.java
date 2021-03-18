package utam.core.selenium.expectations;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static org.hamcrest.Matchers.sameInstance;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.withSettings;
import static org.testng.Assert.assertThrows;
import static org.testng.Assert.expectThrows;
import static utam.core.selenium.element.ShadowRootWebElement.GET_SHADOW_ROOT_QUERY_SELECTOR_ALL;
import static utam.core.selenium.expectations.ExpectationsUtil.ABSENCE_MSG;
import static utam.core.selenium.expectations.ExpectationsUtil.BLUR_JS;
import static utam.core.selenium.expectations.ExpectationsUtil.CLEAR_AND_TYPE_MSG;
import static utam.core.selenium.expectations.ExpectationsUtil.CLEAR_MSG;
import static utam.core.selenium.expectations.ExpectationsUtil.CLICK_JS;
import static utam.core.selenium.expectations.ExpectationsUtil.ENABLED_CHECK_MSG;
import static utam.core.selenium.expectations.ExpectationsUtil.FIND_ELEMENTS_MSG;
import static utam.core.selenium.expectations.ExpectationsUtil.FIND_MSG;
import static utam.core.selenium.expectations.ExpectationsUtil.FOCUS_JS;
import static utam.core.selenium.expectations.ExpectationsUtil.GET_ATTRIBUTE_MSG;
import static utam.core.selenium.expectations.ExpectationsUtil.GET_TEXT_MSG;
import static utam.core.selenium.expectations.ExpectationsUtil.HAS_FOCUS_MSG;
import static utam.core.selenium.expectations.ExpectationsUtil.INVISIBILITY_MSG;
import static utam.core.selenium.expectations.ExpectationsUtil.IS_VISIBLE_MSG;
import static utam.core.selenium.expectations.ExpectationsUtil.JAVASCRIPT_MSG;
import static utam.core.selenium.expectations.ExpectationsUtil.MOVE_TO_ELEMENT_MSG;
import static utam.core.selenium.expectations.ExpectationsUtil.PRESENCE_CHECK_MSG;
import static utam.core.selenium.expectations.ExpectationsUtil.PRESENCE_WAIT_MSG;
import static utam.core.selenium.expectations.ExpectationsUtil.SCROLL_TO_CENTER_JS;
import static utam.core.selenium.expectations.ExpectationsUtil.SET_TEXT_MSG;
import static utam.core.selenium.expectations.ExpectationsUtil.VISIBILITY_MSG;
import static utam.core.selenium.expectations.ExpectationsUtil.WAIT_FOR_MSG;
import static utam.core.selenium.expectations.ExpectationsUtil.blur;
import static utam.core.selenium.expectations.ExpectationsUtil.focus;
import static utam.core.selenium.expectations.ExpectationsUtil.hasFocus;
import static utam.core.selenium.expectations.ExpectationsUtil.scrollTo;
import static utam.core.selenium.expectations.ExpectationsUtil.scrollToCenter;
import static utam.core.selenium.expectations.SalesforceWebDriverUtils.SCROLL_INTO_VIEW_ALIGN_TO_TOP_JS;
import static utam.core.selenium.expectations.SalesforceWebDriverUtils.SCROLL_INTO_VIEW_ERR;
import static utam.core.selenium.expectations.SalesforceWebDriverUtils.SCROLL_INTO_VIEW_MSG;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Supplier;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.openqa.selenium.By;
import org.openqa.selenium.ElementNotVisibleException;
import org.openqa.selenium.InvalidElementStateException;
import org.openqa.selenium.JavascriptException;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.SearchContext;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.WrapsDriver;
import org.testng.annotations.Test;
import utam.core.selenium.context.SeleniumContextProvider;
import utam.core.selenium.context.WebDriverUtilities;
import utam.core.selenium.element.Selector;
import utam.core.selenium.element.Web;
import utam.core.selenium.expectations.ElementWait.Match;

/**
 * Tests the ExpectationsUtil class
 *
 * @author james.evans
 */
public class ExpectationsUtilTests {

  private static final WebDriver mockDriver = getDriverMock();

  private static WebDriver getDriverMock() {
    return mock(
        WebDriver.class,
        withSettings().extraInterfaces(JavascriptExecutor.class, SearchContext.class));
  }

  private static List<WebElement> getEmptyList() {
    return new ArrayList<>();
  }

  private static List<WebElement> getPopulatedList(WebElement element) {
    return Stream.of(element).collect(Collectors.toList());
  }

  /** Tests that the absence method returns true if no element is found */
  @Test
  void testAbsence() {
    SeleniumContextProvider provider = new SeleniumContextProvider(mockDriver);
    ElementListExpectations<Boolean> expectation = ExpectationsUtil.waitForAbsence();

    Boolean result = expectation.apply(provider.getWebDriverUtils()).apply(getEmptyList());
    assertThat(result, is(equalTo(Boolean.TRUE)));

    assertThat(expectation.getLogMessage(), is(equalTo(ABSENCE_MSG)));
    assertThat(expectation.returnIfNothingFound(), is(equalTo(Boolean.TRUE)));
  }

  /** Tests that the absence method returns false if at least one element is found */
  @Test
  void testAbsenceWithNonEmptyList() {
    SeleniumContextProvider provider = new SeleniumContextProvider(mockDriver);
    ElementListExpectations<Boolean> expectation = ExpectationsUtil.waitForAbsence();
    WebElement mockElement = mock(WebElement.class);

    Boolean result =
        expectation.apply(provider.getWebDriverUtils()).apply(getPopulatedList(mockElement));
    assertThat(result, is(equalTo(Boolean.FALSE)));
  }

  /** Tests that the absence method returns true if the list of elements is null */
  @Test
  void testAbsenceWithNullList() {
    SeleniumContextProvider provider = new SeleniumContextProvider(mockDriver);
    ElementListExpectations<Boolean> expectation = ExpectationsUtil.waitForAbsence();

    Boolean result = expectation.apply(provider.getWebDriverUtils()).apply(null);
    assertThat(result, is(equalTo(Boolean.TRUE)));
  }

  /**
   * Tests that the invisibility method returns true if the list of elements contains no elements
   * after removing all visible elements
   */
  @Test
  void testInvisibility() {
    SeleniumContextProvider provider = new SeleniumContextProvider(mockDriver);
    WebElement mockElement = mock(WebElement.class);
    when(mockElement.isDisplayed()).thenReturn(true);
    ElementListExpectations<Boolean> expectation = ExpectationsUtil.waitForInvisible();

    Boolean result =
        expectation.apply(provider.getWebDriverUtils()).apply(getPopulatedList(mockElement));
    assertThat(result, is(equalTo(Boolean.FALSE)));

    assertThat(expectation.getLogMessage(), is(equalTo(INVISIBILITY_MSG)));
    assertThat(expectation.returnIfNothingFound(), is(equalTo(Boolean.TRUE)));
  }

  /**
   * Tests that the invisibility method returns false if the list of elements contains any elements
   * after removing all visible elements
   */
  @Test
  void testInvisibilityWithVisibleElement() {
    SeleniumContextProvider provider = new SeleniumContextProvider(mockDriver);
    WebElement mockElement = mock(WebElement.class);
    when(mockElement.isDisplayed()).thenReturn(false);
    ElementListExpectations<Boolean> expectation = ExpectationsUtil.waitForInvisible();

    Boolean result =
        expectation.apply(provider.getWebDriverUtils()).apply(getPopulatedList(mockElement));
    assertThat(result, is(equalTo(Boolean.TRUE)));
  }

  /**
   * Tests that the invisibility method returns true if the list of elements contains no elements
   */
  @Test
  void testInvisibilityWithEmptyList() {
    SeleniumContextProvider provider = new SeleniumContextProvider(mockDriver);
    ElementListExpectations<Boolean> expectation = ExpectationsUtil.waitForInvisible();

    Boolean result = expectation.apply(provider.getWebDriverUtils()).apply(getEmptyList());
    assertThat(result, is(equalTo(Boolean.TRUE)));
  }

  /** Tests that the invisibility method returns true if the list of elements is null */
  @Test
  void testInvisibilityWithNullList() {
    SeleniumContextProvider provider = new SeleniumContextProvider(mockDriver);
    ElementListExpectations<Boolean> expectation = ExpectationsUtil.waitForInvisible();

    Boolean result = expectation.apply(provider.getWebDriverUtils()).apply(null);
    assertThat(result, is(equalTo(Boolean.TRUE)));
  }

  /** Tests that the clear method can be called */
  @Test
  void testClear() {
    SeleniumContextProvider provider = new SeleniumContextProvider(mockDriver);
    WebElement mockElement = mock(WebElement.class);
    ElementExpectations<SearchContext> expectation = ExpectationsUtil.clear();

    SearchContext result = expectation.apply(provider.getWebDriverUtils()).apply(mockElement);
    assertThat(result, is(equalTo(mockElement)));

    assertThat(expectation.getLogMessage(), is(equalTo(CLEAR_MSG)));
    assertThat(expectation.returnIfNothingFound(), is(nullValue()));
  }

  /** Tests that the clearAndType method can be called */
  @Test
  void testClearAndType() {
    final String testValue = "testValue";
    SeleniumContextProvider provider = new SeleniumContextProvider(mockDriver);
    WebElement mockElement = mock(WebElement.class);
    ElementExpectations<SearchContext> expectation = ExpectationsUtil.clearAndType(testValue);

    SearchContext result = expectation.apply(provider.getWebDriverUtils()).apply(mockElement);
    assertThat(result, is(equalTo(mockElement)));

    assertThat(expectation.getLogMessage(), is(equalTo(String.format(CLEAR_AND_TYPE_MSG, testValue))));
    assertThat(expectation.returnIfNothingFound(), is(nullValue()));
  }

  /** Tests that the click method can be called */
  @Test
  void testClick() {
    SeleniumContextProvider provider = new SeleniumContextProvider(mockDriver);
    WebElement mockElement = mock(WebElement.class);
    ElementExpectations<SearchContext> expectation = ExpectationsUtil.click();

    SearchContext result = expectation.apply(provider.getWebDriverUtils()).apply(mockElement);
    assertThat(result, is(equalTo(mockElement)));

    assertThat(expectation.getLogMessage(), is(equalTo("click")));
    assertThat(expectation.returnIfNothingFound(), is(nullValue()));
  }

  /**
   * Tests that the click method can be called and a JavaScript click will be attempted if an
   * expected exception is thrown
   */
  @Test
  void testClickRetry() {
    SeleniumContextProvider provider = new SeleniumContextProvider(mockDriver);
    WebElement mockElement = mock(WebElement.class);
    doThrow(new JavascriptException("javascript error: Cannot read property 'defaultView' of undefined"))
        .when(mockElement).click();
    ElementExpectations<SearchContext> expectation = ExpectationsUtil.click();

    SearchContext result = expectation.apply(provider.getWebDriverUtils()).apply(mockElement);
    assertThat(result, is(equalTo(mockElement)));

    assertThat(expectation.getLogMessage(), is(equalTo("click")));
    assertThat(expectation.returnIfNothingFound(), is(nullValue()));
  }

  /**
   * Tests that the click method can be called if an unknown JavaScript exception is thrown,
   * it is thrown by the expectation
   */
  @Test
  void testClickUnknownExceptionThrows() {
    SeleniumContextProvider provider = new SeleniumContextProvider(mockDriver);
    WebElement mockElement = mock(WebElement.class);
    doThrow(new JavascriptException("javascript error: unknown JS error")).when(mockElement).click();
    ElementExpectations<SearchContext> expectation = ExpectationsUtil.click();
    JavascriptException e = expectThrows(
        JavascriptException.class,
        () -> expectation.apply(provider.getWebDriverUtils()).apply(mockElement)
    );

    assertThat(e.getMessage(), containsString("javascript error: unknown JS error"));
  }

  /**
   * Tests that the visibility method returns true when the list contains at least one visible
   * element
   */
  @Test
  void testVisibility() {
    SeleniumContextProvider provider = new SeleniumContextProvider(mockDriver);
    WebElement mockElement = mock(WebElement.class);
    when(mockElement.isDisplayed()).thenReturn(true);
    ElementListExpectations<Boolean> expectation = ExpectationsUtil.waitForVisible();

    Boolean result =
        expectation.apply(provider.getWebDriverUtils()).apply(getPopulatedList(mockElement));
    assertThat(result, is(equalTo(Boolean.TRUE)));

    assertThat(expectation.getLogMessage(), is(equalTo(VISIBILITY_MSG)));
    assertThat(expectation.returnIfNothingFound(), is(nullValue()));
  }

  /** Tests that the visibility method returns false when the list contains zero visible elements */
  @Test
  void testVisibilityWithInvisibleElement() {
    SeleniumContextProvider provider = new SeleniumContextProvider(mockDriver);
    WebElement mockElement = mock(WebElement.class);
    when(mockElement.isDisplayed()).thenReturn(false);
    ElementListExpectations<Boolean> expectation = ExpectationsUtil.waitForVisible();

    Boolean result =
        expectation.apply(provider.getWebDriverUtils()).apply(getPopulatedList(mockElement));
    assertThat(result, is(equalTo(Boolean.FALSE)));
  }

  /** Tests that the visibility method returns false when the list is empty */
  @Test
  void testVisibilityWithEmptyList() {
    SeleniumContextProvider provider = new SeleniumContextProvider(mockDriver);
    ElementListExpectations<Boolean> expectation = ExpectationsUtil.waitForVisible();

    Boolean result = expectation.apply(provider.getWebDriverUtils()).apply(getEmptyList());
    assertThat(result, is(equalTo(Boolean.FALSE)));
  }

  /** Tests that the visibility method returns false when the list is null */
  @Test
  void testVisibilityWithNullList() {
    SeleniumContextProvider provider = new SeleniumContextProvider(mockDriver);
    ElementListExpectations<Boolean> expectation = ExpectationsUtil.waitForVisible();

    Boolean result = expectation.apply(provider.getWebDriverUtils()).apply(null);
    assertThat(result, is(equalTo(Boolean.FALSE)));
  }

  /** Tests that the getAttribute method returns the expected value for a valid attribute name */
  @Test
  void testGetAttribute() {
    final String attributeName = "attributeName";
    final String attributeValue = "attributeValue";
    SeleniumContextProvider provider = new SeleniumContextProvider(mockDriver);
    WebElement mockElement = mock(WebElement.class);
    when(mockElement.getAttribute(attributeName)).thenReturn(attributeValue);
    ElementExpectations<String> expectation = ExpectationsUtil.getAttribute(attributeName);

    String result = expectation.apply(provider.getWebDriverUtils()).apply(mockElement);
    assertThat(result, is(equalTo(attributeValue)));

    assertThat(expectation.getLogMessage(), is(equalTo(String.format(GET_ATTRIBUTE_MSG, attributeName))));
    assertThat(expectation.returnIfNothingFound(), is(nullValue()));
  }

  /** Tests that the getAttribute method returns null for an invalid attribute name */
  @Test
  void testGetAttributeWithMissingAttribute() {
    final String attributeName = "attributeName";
    SeleniumContextProvider provider = new SeleniumContextProvider(mockDriver);
    WebElement mockElement = mock(WebElement.class);
    when(mockElement.getAttribute(attributeName)).thenReturn(null);
    ElementExpectations<String> expectation = ExpectationsUtil.getAttribute(attributeName);

    String result = expectation.apply(provider.getWebDriverUtils()).apply(mockElement);
    assertThat(result, is(nullValue()));
  }

  /** Tests that the getText method returns the text of the element */
  @Test
  void testGetText() {
    final String elementText = "textValue";
    SeleniumContextProvider provider = new SeleniumContextProvider(mockDriver);
    WebElement mockElement = mock(WebElement.class);
    when(mockElement.getText()).thenReturn(elementText);
    ElementExpectations<String> expectation = ExpectationsUtil.getText();

    String result = expectation.apply(provider.getWebDriverUtils()).apply(mockElement);
    assertThat(result, is(equalTo(elementText)));

    assertThat(expectation.getLogMessage(), is(equalTo(GET_TEXT_MSG)));
    assertThat(expectation.returnIfNothingFound(), is(nullValue()));
  }

  /** Tests that the getText method returns null when the text of the element is null */
  @Test
  void testGetTextWithNullText() {
    SeleniumContextProvider provider = new SeleniumContextProvider(mockDriver);
    WebElement mockElement = mock(WebElement.class);
    when(mockElement.getText()).thenReturn(null);
    ElementExpectations<String> expectation = ExpectationsUtil.getText();

    String result = expectation.apply(provider.getWebDriverUtils()).apply(mockElement);
    assertThat(result, is(nullValue()));
  }

  /** Tests that the getTitle method returns the value of the title attribute */
  @Test
  void testGetTitle() {
    final String titleAttributeName = "title";
    final String titleValue = "titleValue";
    SeleniumContextProvider provider = new SeleniumContextProvider(mockDriver);
    WebElement mockElement = mock(WebElement.class);
    when(mockElement.getAttribute(titleAttributeName)).thenReturn(titleValue);
    ElementExpectations<String> expectation = ExpectationsUtil.getAttribute(titleAttributeName);

    String result = expectation.apply(provider.getWebDriverUtils()).apply(mockElement);
    assertThat(result, is(equalTo(titleValue)));

    assertThat(expectation.getLogMessage(), is(equalTo(String.format(GET_ATTRIBUTE_MSG, titleAttributeName))));
    assertThat(expectation.returnIfNothingFound(), is(nullValue()));
  }

  /** Tests that the getValue method returns the value of the value attribute */
  @Test
  void testGetValue() {
    final String valueAttributeName = "value";
    final String valueAttributeValue = "attributeValue";
    SeleniumContextProvider provider = new SeleniumContextProvider(mockDriver);
    WebElement mockElement = mock(WebElement.class);
    when(mockElement.getAttribute(valueAttributeName)).thenReturn(valueAttributeValue);
    ElementExpectations<String> expectation = ExpectationsUtil.getAttribute(valueAttributeName);

    String result = expectation.apply(provider.getWebDriverUtils()).apply(mockElement);
    assertThat(result, is(equalTo(valueAttributeValue)));

    assertThat(expectation.getLogMessage(), is(equalTo(String.format(GET_ATTRIBUTE_MSG, valueAttributeName))));
    assertThat(expectation.returnIfNothingFound(), is(nullValue()));
  }

  /**
   * Tests that the isDisplayed method returns true if the list contains at least one visible
   * element
   */
  @Test
  void testIsDisplayed() {
    SeleniumContextProvider provider = new SeleniumContextProvider(mockDriver);
    WebElement mockElement = mock(WebElement.class);
    when(mockElement.isDisplayed()).thenReturn(true);
    ElementListExpectations<Match> expectation = ExpectationsUtil.isDisplayed();

    Match result =
        expectation.apply(provider.getWebDriverUtils()).apply(getPopulatedList(mockElement));
    assertThat(result, is(equalTo(Match.TRUE)));

    assertThat(expectation.getLogMessage(), is(equalTo(IS_VISIBLE_MSG)));
    assertThat(expectation.returnIfNothingFound(), is(equalTo(Match.FALSE)));
  }

  /** Tests that the isDisplayed method returns false if the list contains no visible elements */
  @Test
  void testIsDisplayedWithInvisibleElement() {
    SeleniumContextProvider provider = new SeleniumContextProvider(mockDriver);
    WebElement mockElement = mock(WebElement.class);
    when(mockElement.isDisplayed()).thenReturn(false);
    ElementListExpectations<Match> expectation = ExpectationsUtil.isDisplayed();

    Match result =
        expectation.apply(provider.getWebDriverUtils()).apply(getPopulatedList(mockElement));
    assertThat(result, is(equalTo(Match.FALSE)));
  }

  /** Tests that the isDisplayed method returns false if the list of elements is empty */
  @Test
  void testIsDisplayedWithEmptyList() {
    SeleniumContextProvider provider = new SeleniumContextProvider(mockDriver);
    ElementListExpectations<Match> expectation = ExpectationsUtil.isDisplayed();

    Match result = expectation.apply(provider.getWebDriverUtils()).apply(getEmptyList());
    assertThat(result, is(equalTo(Match.FALSE)));
  }

  /** Tests that the isDisplayed method returns false if the list of elements is null */
  @Test
  void testIsDisplayedWithNullList() {
    SeleniumContextProvider provider = new SeleniumContextProvider(mockDriver);
    ElementListExpectations<Match> expectation = ExpectationsUtil.isDisplayed();

    Match result = expectation.apply(provider.getWebDriverUtils()).apply(null);
    assertThat(result, is(equalTo(Match.FALSE)));
  }

  /** Tests that the isPresent method returns true if there is at least one element in the list */
  @Test
  void testIsPresent() {
    SeleniumContextProvider provider = new SeleniumContextProvider(mockDriver);
    WebElement mockElement = mock(WebElement.class);
    ElementListExpectations<Match> expectation = ExpectationsUtil.isPresent();

    Match result =
        expectation.apply(provider.getWebDriverUtils()).apply(getPopulatedList(mockElement));
    assertThat(result, is(equalTo(Match.TRUE)));

    assertThat(expectation.getLogMessage(), is(equalTo(PRESENCE_CHECK_MSG)));
    assertThat(expectation.returnIfNothingFound(), is(equalTo(Match.FALSE)));
  }

  /** Tests that the isPresent method returns false if element list is empty */
  @Test
  void testIsPresentWithEmptyList() {
    SeleniumContextProvider provider = new SeleniumContextProvider(mockDriver);
    ElementListExpectations<Match> expectation = ExpectationsUtil.isPresent();

    Match result = expectation.apply(provider.getWebDriverUtils()).apply(getEmptyList());
    assertThat(result, is(equalTo(Match.FALSE)));
  }

  /** Tests that the isPresent method returns false if element list is null */
  @Test
  void testIsPresentWithNullList() {
    SeleniumContextProvider provider = new SeleniumContextProvider(mockDriver);
    ElementListExpectations<Match> expectation = ExpectationsUtil.isPresent();

    Match result = expectation.apply(provider.getWebDriverUtils()).apply(null);
    assertThat(result, is(equalTo(Match.FALSE)));
  }

  /** Tests that the isPresent method returns false if element list is null */
  @Test
  void testIsEnabled() {
    WebDriverUtilities utilities = new SeleniumContextProvider(mockDriver).getWebDriverUtils();
    ElementListExpectations<Match> expectation = ExpectationsUtil.isEnabled();
    assertThat(expectation.getLogMessage(), is(equalTo(ENABLED_CHECK_MSG)));
    assertThat(expectation.returnIfNothingFound(), is(equalTo(Match.FALSE)));
    assertThat(expectation.apply(utilities).apply(null), is(equalTo(Match.FALSE)));
    WebElement enabledElement = mock(WebElement.class);
    when(enabledElement.isEnabled()).thenReturn(true);
    WebElement disabledElement = mock(WebElement.class);
    when(disabledElement.isEnabled()).thenReturn(false);
    assertThat(expectation.apply(utilities).apply(getPopulatedList(enabledElement)), is(equalTo(Match.TRUE)));
    assertThat(expectation.apply(utilities).apply(getPopulatedList(disabledElement)), is(equalTo(Match.FALSE)));
    assertThat(expectation.apply(utilities)
            // one of the elements
            .apply(Stream.of(disabledElement, enabledElement)
                    .collect(Collectors.toList())), is(equalTo(Match.FALSE)));
    assertThat(expectation.apply(utilities)
            // one of the elements
            .apply(Stream.of(enabledElement, enabledElement)
                    .collect(Collectors.toList())), is(equalTo(Match.TRUE)));
  }

  /** Tests that the javascriptClick method can be called */
  @Test
  void testJavaScriptClick() {
    SeleniumContextProvider provider = new SeleniumContextProvider(mockDriver);
    WebElement mockElement = mock(WebElement.class);
    ElementExpectations<SearchContext> expectation = ExpectationsUtil.javascriptClick();

    SearchContext result = expectation.apply(provider.getWebDriverUtils()).apply(mockElement);
    assertThat(result, is(equalTo(mockElement)));

    assertThat(expectation.getLogMessage(), is(String.format(JAVASCRIPT_MSG, CLICK_JS)));
    assertThat(expectation.returnIfNothingFound(), is(nullValue()));
  }

  /** Tests that the presence method returns true if there is at least one element in the list */
  @Test
  void testPresence() {
    SeleniumContextProvider provider = new SeleniumContextProvider(mockDriver);
    WebElement mockElement = mock(WebElement.class);
    ElementListExpectations<Boolean> expectation = ExpectationsUtil.presence();

    Boolean result =
            expectation.apply(provider.getWebDriverUtils()).apply(getPopulatedList(mockElement));
    assertThat(result, is(equalTo(Boolean.TRUE)));

    assertThat(expectation.getLogMessage(), is(equalTo(PRESENCE_WAIT_MSG)));
    assertThat(expectation.returnIfNothingFound(), is(nullValue()));
  }

  /** Tests that the presence method returns false if the element list is empty */
  @Test
  void testPresenceWithEmptyList() {
    SeleniumContextProvider provider = new SeleniumContextProvider(mockDriver);
    ElementListExpectations<Boolean> expectation = ExpectationsUtil.presence();

    Boolean result = expectation.apply(provider.getWebDriverUtils()).apply(getEmptyList());
    assertThat(result, is(equalTo(Boolean.FALSE)));
  }

  /** Tests that the presence method returns false if the element list is null */
  @Test
  void testPresenceWithNullList() {
    SeleniumContextProvider provider = new SeleniumContextProvider(mockDriver);
    ElementListExpectations<Boolean> expectation = ExpectationsUtil.presence();

    Boolean result = expectation.apply(provider.getWebDriverUtils()).apply(null);
    assertThat(result, is(equalTo(Boolean.FALSE)));
  }

  @Test
  void testFindElements() {
    SeleniumContextProvider provider = new SeleniumContextProvider(mockDriver);
    WebElement mockElement = mock(WebElement.class);
    when(mockElement.findElements(By.cssSelector(".found"))).thenReturn(getPopulatedList(mockElement));
    Selector selector = Web.byCss(".found");
    ElementExpectations<Integer> expectation = ExpectationsUtil.findElements(selector, false);

    Integer result =
        expectation.apply(provider.getWebDriverUtils()).apply(mockElement);
    assertThat(result, is(equalTo(1)));

    assertThat(expectation.getLogMessage(), is(equalTo(String.format(FIND_ELEMENTS_MSG, "By.cssSelector: .found", ""))));
    assertThat(expectation.returnIfNothingFound(), is(0));
  }

  @Test
  void testFindElementsInShadow() {
    String SELECTOR_STR = ".found";
    SeleniumContextProvider provider = new SeleniumContextProvider(mockDriver);
    WebElement mockElement = mock(WebElement.class, withSettings().extraInterfaces(WrapsDriver.class));
    when(((WrapsDriver)mockElement).getWrappedDriver()).thenReturn(mockDriver);
    JavascriptExecutor mockExecutor = (JavascriptExecutor) mockDriver;
    when(mockExecutor.executeScript(String.format(GET_SHADOW_ROOT_QUERY_SELECTOR_ALL, SELECTOR_STR), mockElement))
        .thenReturn(getPopulatedList(mockElement));
    Selector selector = Web.byCss(SELECTOR_STR);
    ElementExpectations<Integer> expectation = ExpectationsUtil.findElements(selector, true);
    Integer result =
        expectation.apply(provider.getWebDriverUtils()).apply(mockElement);
    assertThat(result, is(equalTo(1)));
  }

  @Test
  void testFindElementsWithEmptyList() {
    SeleniumContextProvider provider = new SeleniumContextProvider(mockDriver);
    WebElement mockElement = mock(WebElement.class);
    when(mockElement.findElements(By.cssSelector(".found"))).thenReturn(getEmptyList());
    Selector selector = Web.byCss(".found");
    ElementExpectations<Integer> expectation = ExpectationsUtil.findElements(selector, false);
    Integer result = expectation.apply(provider.getWebDriverUtils()).apply(mockElement);
    assertThat(result, is(equalTo(0)));
  }

  @Test
  void testFindElementsWithNullList() {
    SeleniumContextProvider provider = new SeleniumContextProvider(mockDriver);
    WebElement mockElement = mock(WebElement.class);
    when(mockElement.findElements(By.cssSelector(".found"))).thenReturn(null);
    Selector selector = Web.byCss(".found");
    ElementExpectations<Integer> expectation = ExpectationsUtil.findElements(selector, false);
    Integer result = expectation.apply(provider.getWebDriverUtils()).apply(mockElement);
    assertThat(result, is(equalTo(0)));
  }

  /** Tests that the setText method can be called */
  @Test
  void testSetText() {
    final String textValue = "textValue";
    SeleniumContextProvider provider = new SeleniumContextProvider(mockDriver);
    WebElement mockElement = mock(WebElement.class);
    ElementExpectations<SearchContext> expectation = ExpectationsUtil.setText(textValue);

    SearchContext result = expectation.apply(provider.getWebDriverUtils()).apply(mockElement);
    assertThat(result, is(equalTo(mockElement)));

    assertThat(expectation.getLogMessage(), is(equalTo(String.format(SET_TEXT_MSG, textValue))));
    assertThat(expectation.returnIfNothingFound(), is(nullValue()));
  }

  /** Tests that the moveTo method can be called */
  @Test
  void testMoveTo() {
    SeleniumContextProvider provider = new SeleniumContextProvider(mockDriver);
    WebElement mockElement = mock(WebElement.class);
    ElementExpectations<SearchContext> expectation = ExpectationsUtil.moveTo();

    SearchContext result = expectation.apply(provider.getWebDriverUtils()).apply(mockElement);
    assertThat(result, is(equalTo(mockElement)));

    assertThat(expectation.getLogMessage(), is(equalTo(MOVE_TO_ELEMENT_MSG)));
    assertThat(expectation.returnIfNothingFound(), is(nullValue()));
  }

  /** Tests that the find method returns the proper values */
  @Test
  public void testFind() {
    SeleniumContextProvider provider = new SeleniumContextProvider(mockDriver);
    WebElement mockElement = mock(WebElement.class);
    ElementExpectations<SearchContext> expectation = ExpectationsUtil.find(false);
    SearchContext result = expectation.apply(provider.getWebDriverUtils()).apply(mockElement);
    assertThat(result, is(sameInstance(mockElement)));
    assertThat(expectation.getLogMessage(), is(equalTo(String.format(FIND_MSG, ""))));
  }

  @Test
  public void testFindShadowRoot() {
    SeleniumContextProvider provider = new SeleniumContextProvider(mockDriver);
    WebElement mockElement = mock(WebElement.class);
    ElementExpectations<SearchContext> expectation = ExpectationsUtil.find(true);
    assertThrows(
        InvalidElementStateException.class,
        () -> expectation.apply(provider.getWebDriverUtils()).apply(mockElement));
  }

  @Test
  void testScrollIntoView() {
    MockHelper mock = new MockHelper();
    ElementExpectations<SearchContext> expectations = SalesforceWebDriverUtils.scrollIntoView();
    assertThat(expectations.getLogMessage(), is(equalTo(SCROLL_INTO_VIEW_MSG)));
    assertThat(expectations.returnIfNothingFound(), is(nullValue()));
    ElementNotVisibleException e =
            expectThrows(
                    ElementNotVisibleException.class,
                    () -> expectations.apply(mock.utilities).apply(mock.element));
    assertThat(
            "error cause element is not visible", e.getMessage(), containsString(SCROLL_INTO_VIEW_ERR));
    when(mock.element.isDisplayed()).thenReturn(true);
    expectations.apply(mock.utilities).apply(mock.element);
  }

  /** Tests that the scrollTo method can be called */
  @Test
  void testScrollTo() {
    MockHelper mock = new MockHelper();
    ElementExpectations<SearchContext> expectation = scrollTo();

    SearchContext result = expectation.apply(mock.utilities).apply(mock.element);
    assertThat(result, is(equalTo(mock.element)));

    assertThat(
            expectation.getLogMessage(),
            is(equalTo(String.format(JAVASCRIPT_MSG, SCROLL_INTO_VIEW_ALIGN_TO_TOP_JS))));
    assertThat(expectation.returnIfNothingFound(), is(nullValue()));
  }

  /** Tests that the scrollToCenter method can be called */
  @Test
  void testScrollToCenter() {
    MockHelper mock = new MockHelper();
    ElementExpectations<SearchContext> expectation = scrollToCenter();

    SearchContext result = expectation.apply(mock.utilities).apply(mock.element);
    assertThat(result, is(equalTo(mock.element)));

    assertThat(
            expectation.getLogMessage(),
            is(equalTo(String.format(JAVASCRIPT_MSG, SCROLL_TO_CENTER_JS))));
    assertThat(expectation.returnIfNothingFound(), is(nullValue()));
  }

  @Test
  public void testHasFocus() {
    MockHelper mock = new MockHelper();
    ElementExpectations<ElementWait.Match> expectations = hasFocus();
    assertThat(expectations.getLogMessage(), is(equalTo(HAS_FOCUS_MSG)));
    WebDriver.TargetLocator targetLocator = mock(WebDriver.TargetLocator.class);
    when(mock.driver.switchTo()).thenReturn(targetLocator);
    when(targetLocator.activeElement()).thenReturn(mock(WebElement.class));
    assertThat(expectations.apply(mock.utilities).apply(mock.element), is(ElementWait.Match.FALSE));
    when(targetLocator.activeElement()).thenReturn(mock.element);
    assertThat(expectations.apply(mock.utilities).apply(mock.element), is(ElementWait.Match.TRUE));
  }

  /** Tests that the focus method can be called */
  @Test
  public void testFocus() {
    MockHelper mock = new MockHelper();
    ElementExpectations<SearchContext> expectation = focus();

    SearchContext result = expectation.apply(mock.utilities).apply(mock.element);
    assertThat(result, is(equalTo(mock.element)));

    assertThat(expectation.getLogMessage(), is(equalTo(String.format(JAVASCRIPT_MSG, FOCUS_JS))));
    assertThat(expectation.returnIfNothingFound(), is(nullValue()));
  }

  /** Tests that the blur method can be called */
  @Test
  public void testBlur() {
    MockHelper mock = new MockHelper();
    ElementExpectations<SearchContext> expectation = blur();

    SearchContext result = expectation.apply(mock.utilities).apply(mock.element);
    assertThat(result, is(equalTo(mock.element)));

    assertThat(expectation.getLogMessage(), is(equalTo(String.format(JAVASCRIPT_MSG, BLUR_JS))));
    assertThat(expectation.returnIfNothingFound(), is(nullValue()));
  }

  @Test
  public void testWaitFor() {
    final String elementText = "textValue";
    SeleniumContextProvider provider = new SeleniumContextProvider(mockDriver);
    WebElement mockElement = mock(WebElement.class);
    when(mockElement.getText()).thenReturn(elementText);
    Supplier<String> condition = mockElement::getText;
    ElementExpectations<String> expectation = ExpectationsUtil.waitFor(condition);
    String result = expectation.apply(provider.getWebDriverUtils()).apply(mockElement);
    assertThat(result, is(equalTo(elementText)));

    assertThat(expectation.getLogMessage(), is(equalTo(WAIT_FOR_MSG)));
    assertThat(expectation.returnIfNothingFound(), is(nullValue()));
  }

  static class MockHelper {

    private final WebDriver driver = getDriverMock();
    final WebDriverUtilities utilities = new SeleniumContextProvider(driver).getWebDriverUtils();
    final WebElement element = mock(WebElement.class);
  }
}
