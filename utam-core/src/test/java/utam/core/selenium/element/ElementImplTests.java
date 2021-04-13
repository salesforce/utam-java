/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.selenium.element;

import java.util.function.Supplier;
import org.hamcrest.Matchers;
import org.openqa.selenium.By;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.Keys;
import org.openqa.selenium.SearchContext;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.WrapsDriver;
import org.openqa.selenium.interactions.Interactive;
import org.openqa.selenium.remote.RemoteWebElement;
import org.testng.annotations.Test;
import utam.core.selenium.context.SeleniumContext;
import utam.core.selenium.context.WebDriverUtilities;
import utam.core.selenium.expectations.ElementExpectations;
import utam.core.selenium.expectations.ElementWait;
import utam.core.selenium.expectations.ExpectationsUtil;

import java.time.Duration;
import java.util.ArrayList;
import java.util.Collections;
import java.util.function.BiFunction;
import java.util.function.Function;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;
import static org.mockito.Mockito.any;
import static org.mockito.Mockito.*;
import static org.testng.Assert.assertThrows;
import static utam.core.selenium.element.LocatorUtilities.waitForPresence;
import static utam.core.selenium.element.ShadowRootWebElement.GET_SHADOW_ROOT_QUERY_SELECTOR_ALL;
import static utam.core.selenium.expectations.ExpectationsUtil.*;
import static utam.core.selenium.expectations.SalesforceWebDriverUtils.SCROLL_INTO_VIEW_ALIGN_TO_TOP_JS;

/**
 * element tests
 *
 * @author jim.evans
 * @author elizaveta.ivanova
 */
public class ElementImplTests {

  private static final String LOCATOR_WITHOUT_PARAM = "locator";
  private static final String NAME_ATTRIBUTE = "mockName";
  private static final String SAMPLE_CLASS_ATTRIBUTE = "mock-class";
  private static final String SAMPLE_TEXT = "sample text used to test input elements";
  private static final String SAMPLE_ELEMENT_TITLE = " element title";
  private static final String SAMPLE_ELEMENT_VALUE = "element value";
  private static final String NONEXISTENT_LOCATOR = "Does not Exist";
  private static final String STATE_CHANGING_LOCATOR = ".stateChanging";

  @Test
  public void testIsWithElementExpectation() {
    ElementImplTests.ActionsMock mock = new ElementImplTests.ActionsMock();
    when(mock.webElement.getText()).thenReturn(SAMPLE_TEXT);
    ElementExpectations<ElementWait.Match> expectation =
        new ElementImplTests.MockExpectations<>(
            webElement -> ElementWait.Match.from(webElement.getText().equals(SAMPLE_TEXT)));
    expectation.getLogMessage(); // otherwise method is not covered
    assertThat(mock.getElementWait("test").match(expectation), Matchers.is(equalTo(true)));
  }

  /** The click() method should click a given WebElement using Selenium click() method */
  @Test
  public void testClick() {
    ActionsMock mock = new ActionsMock();
    ElementImpl element = mock.getElementImpl();
    element.click();
    verify(mock.webElement, times(1)).click();
  }

  /**
   * The javascriptClick() method should click a given WebElement using native JavaScript click
   * method
   */
  @Test
  public void testJavascriptClick() {
    ActionsMock mock = new ActionsMock();
    mock.getElementImpl().javascriptClick();
    verify(mock.utilities, times(1)).executeJavaScript("arguments[0].click();", mock.webElement);
  }

  /** The clear() method should call the Selenium WebElement.clear() method on a given WebElement */
  @Test
  public void testClear() {
    ActionsMock mock = new ActionsMock();
    mock.getElementImpl().clear();
    verify(mock.webElement, times(1)).clear();
  }

  /** The clearAndType() method should call the Selenium WebElement clear and sendKeys methods */
  @Test
  public void testClearAndType() {
    ActionsMock mock = new ActionsMock();
    mock.getElementImpl().clearAndType(SAMPLE_TEXT);
    verify(mock.webElement, times(1)).clear();
    verify(mock.webElement, times(1)).sendKeys(SAMPLE_TEXT);
  }

  /** The absence() method should wait for absence for a given WebElement */
  @Test
  public void testAbsence() {
    ActionsMock mock = new ActionsMock();
    when(mock.context.getPollingTimeout()).thenReturn(Duration.ofSeconds(1));
    when(mock.context.getPollingInterval()).thenReturn(Duration.ofMillis(10));
    when(mock.webDriver.findElements(By.cssSelector(STATE_CHANGING_LOCATOR)))
        .thenReturn(Collections.singletonList(mock.webElement))
        .thenReturn(Collections.emptyList());
    mock.getStateChangingElement().waitForAbsence();
    verify(mock.webDriver, times(2)).findElements(By.cssSelector(STATE_CHANGING_LOCATOR));
  }

  /** The visibility() method should wait for visibility of a given WebElement */
  @Test
  public void testVisibility() {
    WebElement stateElement = mock(WebElement.class);
    when(stateElement.isDisplayed()).thenReturn(false).thenReturn(true);
    ActionsMock mock = new ActionsMock();
    when(mock.context.getPollingTimeout()).thenReturn(Duration.ofSeconds(1));
    when(mock.context.getPollingInterval()).thenReturn(Duration.ofMillis(10));
    when(mock.webDriver.findElements(By.cssSelector(STATE_CHANGING_LOCATOR)))
        .thenReturn(new ArrayList<>(Collections.singletonList(stateElement)))
        .thenReturn(new ArrayList<>(Collections.singletonList(stateElement)));
    mock.getStateChangingElement().waitForVisible();
    verify(stateElement, times(2)).isDisplayed();
  }

  /** The invisibility() method should wait for invisibility for a given WebElement */
  @Test
  public void testInvisibility() {
    WebElement stateElement = mock(WebElement.class);
    when(stateElement.isDisplayed()).thenReturn(true).thenReturn(false);
    ActionsMock mock = new ActionsMock();
    when(mock.context.getPollingTimeout()).thenReturn(Duration.ofSeconds(1));
    when(mock.context.getPollingInterval()).thenReturn(Duration.ofMillis(10));
    when(mock.webDriver.findElements(By.cssSelector(STATE_CHANGING_LOCATOR)))
        .thenReturn(new ArrayList<>(Collections.singletonList(stateElement)))
        .thenReturn(new ArrayList<>(Collections.singletonList(stateElement)));
    mock.getStateChangingElement().waitForInvisible();
    verify(stateElement, times(2)).isDisplayed();
  }

  /** The presence() method should wait for presence for a given webElement */
  @Test
  public void testPresence() {
    ActionsMock mock = new ActionsMock();
    when(mock.context.getPollingTimeout()).thenReturn(Duration.ofSeconds(1));
    when(mock.context.getPollingInterval()).thenReturn(Duration.ofMillis(10));
    when(mock.webDriver.findElements(By.cssSelector(STATE_CHANGING_LOCATOR)))
        .thenReturn(Collections.emptyList())
        .thenReturn(Collections.singletonList(mock.webElement));
    ElementImpl element = mock.getStateChangingElement();
    waitForPresence(element);
    verify(mock.webDriver, times(2)).findElements(By.cssSelector(STATE_CHANGING_LOCATOR));
  }

  /** The isDisplayed() method should return false if WebElement not found in the DOM */
  @Test
  public void testIsDisplayedFalse() {
    ActionsMock mock = new ActionsMock();
    ElementImpl element = mock.getNonExistentElement();
    assertThat(element.isVisible(), is(equalTo(false)));
  }

  /** The isDisplayed() method should return true if WebElement found in the DOM */
  @Test
  public void testIsDisplayedTrue() {
    ActionsMock mock = new ActionsMock();
    when(mock.webElement.isDisplayed()).thenReturn(true);
    ElementImpl element = mock.getElementImpl();
    assertThat(element.isVisible(), is(equalTo(true)));
  }

  /** The isPresent() method should return true if element found in browser */
  @Test
  public void testIsPresentTrue() {
    ActionsMock mock = new ActionsMock();
    ElementImpl element = mock.getElementImpl();
    assertThat(element.isPresent(), is(equalTo(true)));
  }

  /** The isPresent() method should return false if element not found in browser */
  @Test
  public void testIsPresentFalse() {
    ActionsMock mock = new ActionsMock();
    ElementImpl element = mock.getNonExistentElement();
    assertThat(element.isPresent(), is(equalTo(false)));
  }

  /** The getAttribute() method should return WebElement's attribute value */
  @Test
  public void testGetAttribute() {
    ElementImpl element = new ActionsMock().getElementImpl();
    assertThat(element.getAttribute("name"), is(NAME_ATTRIBUTE));
  }

  /** The getText() method should call the Selenium WebElement.getText() method */
  @Test
  public void testGetText() {
    ActionsMock mock = new ActionsMock();
    when(mock.webElement.getText()).thenReturn(SAMPLE_TEXT);
    assertThat(mock.getElementImpl().getText(), is(equalTo(SAMPLE_TEXT)));
  }

  /** The setText() method should call the Selenium WebElement.sendKeys() method */
  @Test
  public void testSetText() {
    ActionsMock mock = new ActionsMock();
    ElementImpl element = mock.getElementImpl();
    element.setText(SAMPLE_TEXT);
    verify(mock.webElement, times(1)).sendKeys(SAMPLE_TEXT);
  }

  @Test
  public void testPress() {
    ActionsMock mock = new ActionsMock();
    ElementImpl element = mock.getElementImpl();
    final String KEY = "Enter";
    element.press(KEY);
    verify(mock.webElement, times(1)).sendKeys(Keys.ENTER.toString());
  }

  /** The getTitle() method should return the value of 'title' attribute for a given WebElement */
  @Test
  public void testGetTitle() {
    ActionsMock mock = new ActionsMock();
    when(mock.webElement.getAttribute("title")).thenReturn(SAMPLE_ELEMENT_TITLE);
    ElementImpl element = mock.getElementImpl();
    assertThat(element.getTitle(), is(SAMPLE_ELEMENT_TITLE));
  }

  /** The getClassAttribute() method should return the value of 'class' attribute for a given WebElement */
  @Test
  public void testGetClassAttribute() {
    ActionsMock mock = new ActionsMock();
    when(mock.webElement.getAttribute("class")).thenReturn(SAMPLE_CLASS_ATTRIBUTE);
    ElementImpl element = mock.getElementImpl();
    assertThat(element.getClassAttribute(), is(SAMPLE_CLASS_ATTRIBUTE));
  }

  /**
   * The getValue() method should return the value of the 'value' attribute for a given WebElement
   */
  @Test
  public void testGetValue() {
    ActionsMock mock = new ActionsMock();
    when(mock.webElement.getAttribute("value")).thenReturn(SAMPLE_ELEMENT_VALUE);
    ElementImpl element = mock.getElementImpl();
    assertThat(element.getValue(), is(SAMPLE_ELEMENT_VALUE));
  }

  /** The moveTo() method should use the Selenium Actions class to move to a given WebElement */
  @Test
  public void testMoveTo() {
    ActionsMock mock = new ActionsMock();
    ElementImpl element = mock.getElementImpl();
    element.moveTo();
    verify(((Interactive) mock.webDriver), times(1)).perform(any());
  }

  @Test
  public void testIsWithListExpectation() {
    ActionsMock mock = new ActionsMock();
    ElementWaitImpl element = mock.getElementWait("?");
    assertThat(element.match(ExpectationsUtil.isPresent()), is(equalTo(true)));
  }

  /** The find() method should return the appropriate WebElement */
  @Test
  public void testFind() {
    ActionsMock mock = new ActionsMock();
    ElementImpl element = mock.getElementImpl();
    assertThat(element.find(false), is(sameInstance(mock.webElement)));
  }

  @Test
  public void testFindShadowRoot() {
    ActionsMock mock = new ActionsMock();
    ElementImpl element = mock.getElementImpl();
    when(mock.utilities.expandShadowRoot(mock.webElement))
        .thenReturn(mock.webElement);
    assertThat(element.find(true), is(sameInstance(mock.webElement)));
  }

  /** The isPresent() method should return true if element found in browser */
  @Test
  public void testIsEnabled() {
    ActionsMock mock = new ActionsMock();
    assertThat(mock.getElementImpl().isEnabled(), is(equalTo(false)));
    assertThat(mock.getNonExistentElement().isEnabled(), is(equalTo(false)));
    when(mock.webElement.isEnabled()).thenReturn(true);
    assertThat(mock.getElementImpl().isEnabled(), is(equalTo(true)));
  }

  @Test
  public void testHasFocus() {
    MockHelper mock = new MockHelper();
    WebDriver.TargetLocator targetLocator = mock(WebDriver.TargetLocator.class);
    when(mock.getWebDriver().switchTo()).thenReturn(targetLocator);
    Actionable element = mock.getCustomElement();
    assertThat("element has no focus", element.isFocused(), is(false));
    when(targetLocator.activeElement()).thenReturn(mock.getWebElement());
    assertThat("element has focus", element.isFocused(), is(true));
  }

  /** The focus() method should focus a given WebElement */
  @Test
  public void testFocus() {
    MockHelper mock = new MockHelper();
    mock.getCustomElement().focus();
    verify(mock.utilities, times(1)).executeJavaScript(FOCUS_JS, mock.webElement);
  }

  /** The scrollTo() method should scroll window into view for a given WebElement */
  @Test
  public void testScrollTo() {
    MockHelper mock = new MockHelper();
    mock.getCustomElement().scrollTo();
    verify(mock.utilities, times(2))
        .executeJavaScript(SCROLL_INTO_VIEW_ALIGN_TO_TOP_JS, mock.webElement);
  }

  /** The scrollToTop() method should scroll window into view for a given WebElement */
  @Test
  public void testScrollToTop() {
    MockHelper mock = new MockHelper();
    mock.getCustomElement().scrollToTop();
    verify(mock.utilities, times(2))
        .executeJavaScript(SCROLL_INTO_VIEW_ALIGN_TO_TOP_JS, mock.webElement);
  }

  @Test
  public void testScrollToCenter() {
    MockHelper mock = new MockHelper();
    mock.getCustomElement().scrollToCenter();
    verify(mock.utilities, times(1)).executeJavaScript(SCROLL_TO_CENTER_JS, mock.webElement);
  }

  /** The blur() method should blur a given WebElement */
  @Test
  public void testBlur() {
    MockHelper mock = new MockHelper();
    mock.getCustomElement().blur();
    verify(mock.utilities, times(1)).executeJavaScript(BLUR_JS, mock.webElement);
  }

  @Test
  public void testContainsElement() {
    String SELECTOR_STR = ".found";
    ActionsMock mock = new ActionsMock();
    ElementImpl element = mock.getElementImpl();
    when(mock.webElement.findElements(By.cssSelector(SELECTOR_STR))).thenReturn(Collections.singletonList(mock.webElement));
    assertThat(element.containsElement(Web.byCss(SELECTOR_STR)), is(true));
    assertThat(element.containsElement(Web.byCss("not found"), false), is(false));
  }

  @Test
  public void testContainsElementInShadow() {
    String SELECTOR_STR = ".found";
    ActionsMock mock = new ActionsMock();
    JavascriptExecutor mockExecutor = (JavascriptExecutor) mock.webDriver;
    when(mockExecutor.executeScript(String.format(GET_SHADOW_ROOT_QUERY_SELECTOR_ALL, SELECTOR_STR), mock.webElement))
        .thenReturn(Collections.singletonList(mock.webElement));
    ElementImpl element = mock.getElementImpl();
    assertThat(element.containsElement(Web.byCss(SELECTOR_STR), true), is(true));
    assertThat(element.containsElement(Web.byCss("not found"), true), is(false));
  }

  @Test
  public void testWaitForObject() {
    ActionsMock mock = new ActionsMock();
    ElementImpl element = mock.getElementImpl();
    Supplier<Object> apply = () -> { element.setText("text"); return element; };
    element.waitFor(apply);
  }

  @Test
  public void testWaitForIsDisplayedTrue() {
    ActionsMock mock = new ActionsMock();
    when(mock.webElement.isDisplayed()).thenReturn(true);
    ElementImpl element = mock.getElementImpl();
    Supplier<Boolean> apply = element::isVisible;
    assertThat(element.waitFor(apply), is(equalTo(true)));
  }

  @Test
  public void testWaitForIsDisplayedFalseThrows() {
    ActionsMock mock = new ActionsMock();
    when(mock.webElement.isDisplayed()).thenReturn(false);
    ElementImpl element = mock.getElementImpl();
    Supplier<Boolean> apply = element::isVisible;
    assertThrows(() -> element.waitFor(apply));
  }

  public static class Mock {

    public final SeleniumContext context;
    final WebElement webElement;
    public final WebDriverUtilities utilities;
    final WebDriver webDriver;

    Mock() {
      webDriver = mock(WebDriver.class, withSettings().extraInterfaces(Interactive.class, JavascriptExecutor.class));
      context = mock(SeleniumContext.class);
      utilities = mock(WebDriverUtilities.class);
      webElement = mock(WebElement.class, withSettings().name("elementName").extraInterfaces(WrapsDriver.class));
      when(((WrapsDriver)webElement).getWrappedDriver()).thenReturn(webDriver);
      when(webElement.getAttribute("name")).thenReturn(NAME_ATTRIBUTE);
      when(context.getWebDriverUtils()).thenReturn(utilities);
      when(utilities.getWebDriver()).thenReturn(webDriver);
      when(context
              .getWebDriverUtils()
              .getWebDriver()
              .findElement(By.cssSelector(LOCATOR_WITHOUT_PARAM)))
          .thenReturn(webElement);
      when(context
              .getWebDriverUtils()
              .getWebDriver()
              .findElements(By.cssSelector(LOCATOR_WITHOUT_PARAM)))
          .thenReturn(new ArrayList<>(Collections.singletonList(webElement)));
      when(context.getPollingTimeout()).thenReturn(Duration.ofSeconds(1));
      when(context.getPollingInterval()).thenReturn(Duration.ofMillis(10));
    }

    ElementImpl getElementImpl() {
      return new ElementImpl(
          new LocatorImpl(new LocatorNodeImpl.Css(LOCATOR_WITHOUT_PARAM)), context);
    }

    ElementWaitImpl getElementWait(String message) {
      return new ElementWaitImpl(
          message, new LocatorImpl(new LocatorNodeImpl.Css(LOCATOR_WITHOUT_PARAM)), context);
    }
  }

  static class MockExpectations<T> implements ElementExpectations<T> {

    private final String logMessage;
    private final BiFunction<WebDriverUtilities, WebElement, T> apply;
    private final T returnIfNotFound;

    @SuppressWarnings("unchecked")
    MockExpectations(Function<WebElement, T> apply) {
      this.logMessage = "test";
      this.apply = (wu, we) -> apply.apply(we);
      this.returnIfNotFound = (T) ElementWait.Match.FALSE;
    }

    @Override
    public T returnIfNothingFound() {
      return returnIfNotFound;
    }

    @Override
    public String getLogMessage() {
      return logMessage;
    }

    @Override
    public Function<SearchContext, T> apply(WebDriverUtilities utilities) {
      return searchContext -> this.apply.apply(utilities, (WebElement) searchContext);
    }
  }

  public static class MockHelper {

    public final SeleniumContext context;
    private final WebElement webElement;
    private final WebDriverUtilities utilities;
    private final WebDriver webDriver;

    MockHelper() {
      webDriver = mock(WebDriver.class, withSettings().extraInterfaces(Interactive.class));
      context = mock(SeleniumContext.class);
      utilities = mock(WebDriverUtilities.class);
      webElement = mock(RemoteWebElement.class, withSettings().name("elementName"));

      when(webElement.getAttribute("name")).thenReturn(NAME_ATTRIBUTE);
      when(context.getWebDriverUtils()).thenReturn(utilities);
      when(utilities.getWebDriver()).thenReturn(webDriver);
      when(context
              .getWebDriverUtils()
              .getWebDriver()
              .findElement(By.cssSelector(LOCATOR_WITHOUT_PARAM)))
          .thenReturn(webElement);
      when(context
              .getWebDriverUtils()
              .getWebDriver()
              .findElements(By.cssSelector(LOCATOR_WITHOUT_PARAM)))
          .thenReturn(Collections.singletonList(webElement));
      when(context.getPollingTimeout()).thenReturn(Duration.ofSeconds(1));
      when(context.getPollingInterval()).thenReturn(Duration.ofMillis(10));
    }

    Actionable getCustomElement() {
      return new ElementImpl(
          new LocatorImpl(new LocatorNodeImpl.Css(LOCATOR_WITHOUT_PARAM)), context);
    }

    WebElement getWebElement() {
      return webElement;
    }

    WebDriver getWebDriver() {
      return webDriver;
    }
  }

  static class ActionsMock extends ElementImplTests.Mock {

    ElementImpl getNonExistentElement() {
      return new ElementImpl(
          new LocatorImpl(new LocatorNodeImpl.Css(NONEXISTENT_LOCATOR)), context);
    }

    ElementImpl getStateChangingElement() {
      return new ElementImpl(
          new LocatorImpl(new LocatorNodeImpl.Css(STATE_CHANGING_LOCATOR)), context);
    }
  }
}
