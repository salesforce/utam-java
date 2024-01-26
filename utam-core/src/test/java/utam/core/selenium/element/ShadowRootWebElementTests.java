/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.selenium.element;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.empty;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.withSettings;
import static org.testng.Assert.expectThrows;
import static utam.core.selenium.element.ElementAdapterTests.ELEMENT_NOT_FOUND_ERROR;
import static utam.core.selenium.element.ElementAdapterTests.NOT_FOUND_SELECTOR;
import static utam.core.selenium.element.ElementAdapterTests.findNotNullable;
import static utam.core.selenium.element.ElementAdapterTests.findNotNullables;
import static utam.core.selenium.element.ElementAdapterTests.findNullable;
import static utam.core.selenium.element.ElementAdapterTests.findNullables;
import static utam.core.selenium.element.LocatorBy.byCss;
import static utam.core.selenium.element.ShadowRootWebElement.GET_SHADOW_ROOT_QUERY_SELECTOR;
import static utam.core.selenium.element.ShadowRootWebElement.GET_SHADOW_ROOT_QUERY_SELECTOR_ALL;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import org.openqa.selenium.By;
import org.openqa.selenium.InvalidArgumentException;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.NoSuchElementException;
import org.openqa.selenium.OutputType;
import org.openqa.selenium.UnsupportedCommandException;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.WrapsDriver;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;
import utam.core.MockUtilities;
import utam.core.element.Element;

public class ShadowRootWebElementTests {

  private static final String GOOD_CSS_SELECTOR = "By.cssSelector locator";
  private static final String BAD_CSS_SELECTOR = "Bad locator";
  private WebDriver mockDriver;
  private WebElement mockWebElement;
  private By mockByNoCss;
  private By mockByWithCss;

  @BeforeMethod
  public void setUp() {
    mockDriver = mock(WebDriver.class, withSettings().extraInterfaces(JavascriptExecutor.class));
    mockWebElement =
        mock(
            WebElement.class,
            withSettings().name("mockWebElement").extraInterfaces(WrapsDriver.class));
    mockByNoCss = mock(By.class, withSettings().name(BAD_CSS_SELECTOR));
    mockByWithCss = mock(By.class, withSettings().name(GOOD_CSS_SELECTOR));
  }

  /** The getScreenshotAs() method should throw {@link UnsupportedCommandException}. */
  @Test
  public void testGetScreenshotAs() {
    ShadowRootWebElement shadowRootWebElement = new ShadowRootWebElement(mockWebElement);
    UnsupportedCommandException uce =
        expectThrows(
            UnsupportedCommandException.class,
            () -> shadowRootWebElement.getScreenshotAs(OutputType.FILE));
    assertThat(
        uce.getMessage(),
        containsString("Method <getScreenshotAs> not supported on shadowRoot element"));
  }

  /** The click() method should throw {@link UnsupportedCommandException}. */
  @Test
  public void testClick() {
    ShadowRootWebElement shadowRootWebElement = new ShadowRootWebElement(mockWebElement);
    UnsupportedCommandException uce =
        expectThrows(UnsupportedCommandException.class, shadowRootWebElement::click);
    assertThat(
        uce.getMessage(), containsString("Method <click> not supported on shadowRoot element"));
  }

  /** The submit() method should throw {@link UnsupportedCommandException}. */
  @Test
  public void testSubmit() {
    ShadowRootWebElement shadowRootWebElement = new ShadowRootWebElement(mockWebElement);
    UnsupportedCommandException uce =
        expectThrows(UnsupportedCommandException.class, shadowRootWebElement::submit);
    assertThat(
        uce.getMessage(), containsString("Method <submit> not supported on shadowRoot element"));
  }

  /** The sendKeys() method should throw {@link UnsupportedCommandException}. */
  @Test
  public void testSendKeys() {
    ShadowRootWebElement shadowRootWebElement = new ShadowRootWebElement(mockWebElement);
    UnsupportedCommandException uce =
        expectThrows(UnsupportedCommandException.class, shadowRootWebElement::sendKeys);
    assertThat(
        uce.getMessage(), containsString("Method <sendKeys> not supported on shadowRoot element"));
  }

  /** The clear() method should throw {@link UnsupportedCommandException}. */
  @Test
  public void testClear() {
    ShadowRootWebElement shadowRootWebElement = new ShadowRootWebElement(mockWebElement);
    UnsupportedCommandException uce =
        expectThrows(UnsupportedCommandException.class, shadowRootWebElement::clear);
    assertThat(
        uce.getMessage(), containsString("Method <clear> not supported on shadowRoot element"));
  }

  /** The getTagName() method should throw {@link UnsupportedCommandException}. */
  @Test
  public void testGetTagName() {
    ShadowRootWebElement shadowRootWebElement = new ShadowRootWebElement(mockWebElement);
    UnsupportedCommandException uce =
        expectThrows(UnsupportedCommandException.class, shadowRootWebElement::getTagName);
    assertThat(
        uce.getMessage(),
        containsString("Method <getTagName> not supported on shadowRoot element"));
  }

  /** The getAttribute() method should throw {@link UnsupportedCommandException}. */
  @Test
  public void testGetAttribute() {
    ShadowRootWebElement shadowRootWebElement = new ShadowRootWebElement(mockWebElement);
    UnsupportedCommandException uce =
        expectThrows(
            UnsupportedCommandException.class,
            () -> shadowRootWebElement.getAttribute("attribute"));
    assertThat(
        uce.getMessage(),
        containsString("Method <getAttribute> not supported on shadowRoot element"));
  }

  /** The isSelected() method should throw {@link UnsupportedCommandException}. */
  @Test
  public void testIsSelected() {
    ShadowRootWebElement shadowRootWebElement = new ShadowRootWebElement(mockWebElement);
    UnsupportedCommandException uce =
        expectThrows(UnsupportedCommandException.class, shadowRootWebElement::isSelected);
    assertThat(
        uce.getMessage(),
        containsString("Method <isSelected> not supported on shadowRoot element"));
  }

  /** The isEnabled() method should throw {@link UnsupportedCommandException}. */
  @Test
  public void testIsEnabled() {
    ShadowRootWebElement shadowRootWebElement = new ShadowRootWebElement(mockWebElement);
    UnsupportedCommandException uce =
        expectThrows(UnsupportedCommandException.class, shadowRootWebElement::isEnabled);
    assertThat(
        uce.getMessage(), containsString("Method <isEnabled> not supported on shadowRoot element"));
  }

  /** The getText() method should throw {@link UnsupportedCommandException}. */
  @Test
  public void testGetText() {
    ShadowRootWebElement shadowRootWebElement = new ShadowRootWebElement(mockWebElement);
    UnsupportedCommandException uce =
        expectThrows(UnsupportedCommandException.class, shadowRootWebElement::getText);
    assertThat(
        uce.getMessage(), containsString("Method <getText> not supported on shadowRoot element"));
  }

  /** The isDisplayed() method should throw {@link UnsupportedCommandException}. */
  @Test
  public void testIsDisplayed() {
    ShadowRootWebElement shadowRootWebElement = new ShadowRootWebElement(mockWebElement);
    UnsupportedCommandException uce =
        expectThrows(UnsupportedCommandException.class, shadowRootWebElement::isDisplayed);
    assertThat(
        uce.getMessage(),
        containsString("Method <isDisplayed> not supported on shadowRoot element"));
  }

  /** The getLocation() method should throw {@link UnsupportedCommandException}. */
  @Test
  public void testGetLocation() {
    ShadowRootWebElement shadowRootWebElement = new ShadowRootWebElement(mockWebElement);
    UnsupportedCommandException uce =
        expectThrows(UnsupportedCommandException.class, shadowRootWebElement::getLocation);
    assertThat(
        uce.getMessage(),
        containsString("Method <getLocation> not supported on shadowRoot element"));
  }

  /** The getSize() method should throw {@link UnsupportedCommandException}. */
  @Test
  public void testGetSize() {
    ShadowRootWebElement shadowRootWebElement = new ShadowRootWebElement(mockWebElement);
    UnsupportedCommandException uce =
        expectThrows(UnsupportedCommandException.class, shadowRootWebElement::getSize);
    assertThat(
        uce.getMessage(), containsString("Method <getSize> not supported on shadowRoot element"));
  }

  /** The getRect() method should throw {@link UnsupportedCommandException}. */
  @Test
  public void testGetRect() {
    ShadowRootWebElement shadowRootWebElement = new ShadowRootWebElement(mockWebElement);
    UnsupportedCommandException uce =
        expectThrows(UnsupportedCommandException.class, shadowRootWebElement::getRect);
    assertThat(
        uce.getMessage(), containsString("Method <getRect> not supported on shadowRoot element"));
  }

  /** The getCssValue() method should throw {@link UnsupportedCommandException}. */
  @Test
  public void testGetCssValue() {
    ShadowRootWebElement shadowRootWebElement = new ShadowRootWebElement(mockWebElement);
    UnsupportedCommandException uce =
        expectThrows(
            UnsupportedCommandException.class, () -> shadowRootWebElement.getCssValue("value"));
    assertThat(
        uce.getMessage(),
        containsString("Method <getCssValue> not supported on shadowRoot element"));
  }

  /** The getWrappedElement() method should return wrapped {@link WebElement}. */
  @Test
  public void testGetWrappedElement() {
    ShadowRootWebElement shadowRootWebElement = new ShadowRootWebElement(mockWebElement);
    assertThat(shadowRootWebElement.getWrappedElement(), is(mockWebElement));
  }

  /**
   * The getSelectorString(By by) method should throw {@link InvalidArgumentException} if element
   * locator String does not start with: By.ByCssSelector.
   */
  @Test
  public void testGetSelectorStringThrowsException() {
    InvalidArgumentException iae =
        expectThrows(
            InvalidArgumentException.class,
            () -> ShadowRootWebElement.getSelectorString((mockByNoCss)));
    assertThat(
        iae.getMessage(),
        containsString(
            "Must search for subelements of a shadowRoot element with By.ByCssSelector. Instead"
                + " got: "));
  }

  /**
   * The getSelectorString(By by) method should return {@link String} if element locator starts
   * with: By.ByCssSelector.
   */
  @Test
  public void testGetSelectorStringReturnsStringLocator() {
    assertThat(ShadowRootWebElement.getSelectorString(mockByWithCss), is(GOOD_CSS_SELECTOR));
  }

  /**
   * The findElements(By by) method should return a list of {@link WebElement} if elements are found
   * on the page.
   */
  @Test
  public void testFindElements() {
    when(((WrapsDriver) mockWebElement).getWrappedDriver()).thenReturn(mockDriver);
    ShadowRootWebElement shadowRootWebElement = new ShadowRootWebElement(mockWebElement);
    when(shadowRootWebElement
            .getExecutor()
            .executeScript(
                String.format(GET_SHADOW_ROOT_QUERY_SELECTOR_ALL, GOOD_CSS_SELECTOR),
                mockWebElement))
        .thenReturn(Collections.singletonList(mockWebElement));

    assertThat(shadowRootWebElement.findElements(mockByWithCss).contains(mockWebElement), is(true));
  }

  /**
   * The findElement(By by) method should return a {@link WebElement} if element is found on the
   * page.
   */
  @Test
  public void testFindElement() {
    when(((WrapsDriver) mockWebElement).getWrappedDriver()).thenReturn(mockDriver);
    ShadowRootWebElement shadowRootWebElement = new ShadowRootWebElement(mockWebElement);
    when(shadowRootWebElement
            .getExecutor()
            .executeScript(
                String.format(GET_SHADOW_ROOT_QUERY_SELECTOR, GOOD_CSS_SELECTOR), mockWebElement))
        .thenReturn(mockWebElement);
    assertThat(shadowRootWebElement.findElement(mockByWithCss), is(mockWebElement));
  }

  /**
   * The findElement(By by) method should throw a {@link NoSuchElementException} if the element is
   * not found on the page.
   */
  @Test
  public void testFindElementThrowsException() {
    when(((WrapsDriver) mockWebElement).getWrappedDriver()).thenReturn(mockDriver);
    ShadowRootWebElement shadowRootWebElement = new ShadowRootWebElement(mockWebElement);
    NoSuchElementException nsee =
        expectThrows(
            NoSuchElementException.class, () -> shadowRootWebElement.findElement((mockByWithCss)));
    assertThat(nsee.getMessage(), containsString("Unable to locate element: " + GOOD_CSS_SELECTOR));
  }

  /** The getWrappedDriver() method should return wrapped {@link WebDriver}. */
  @Test
  public void testGetWrappedDriver() {
    ShadowRootWebElement shadowRootWebElement = new ShadowRootWebElement(mockWebElement);
    assertThat(
        shadowRootWebElement.getWrappedDriver(),
        is(((WrapsDriver) mockWebElement).getWrappedDriver()));
  }

  /**
   * The getElementsWithFirefoxWorkaround method should return an empty list if the a null list is
   * passed as a parameter
   */
  @Test
  public void testFirefoxWorkaroundWithNullList() {
    assertThat(ShadowRootWebElement.getElementsWithFirefoxWorkaround(null), is(empty()));
  }

  /**
   * The getElementsWithFirefoxWorkaround method should return a transformed list if the the
   * expected map is passed as a parameter
   */
  @Test
  public void testFirefoxWorkaroundWithMap() {
    ShadowRootWebElement shadowRootWebElement = new ShadowRootWebElement(mockWebElement);
    Map<String, Object> map = new HashMap<>();
    map.put("0", shadowRootWebElement);
    map.put("1", null);
    assertThat(
        ShadowRootWebElement.getElementsWithFirefoxWorkaround(map),
        is(equalTo(Collections.singletonList(shadowRootWebElement))));
  }

  @Test
  public void testFindElementInShadow() {
    MockUtilities mock = new MockUtilities();
    final String FOUND_SELECTOR = "found";
    mock.setShadowMock(mock.getWebElementMock(), FOUND_SELECTOR);
    Element scope = new ShadowRootElementAdapter(mock.getElementAdapter());
    assertThat(findNotNullable(byCss(FOUND_SELECTOR), scope), is(notNullValue()));
  }

  @Test
  public void testFindNullableElementInShadow() {
    MockUtilities mock = new MockUtilities();
    final String FOUND_SELECTOR = "found";
    mock.setShadowMock(mock.getWebElementMock(), FOUND_SELECTOR);
    Element scope = new ShadowRootElementAdapter(mock.getElementAdapter());
    assertThat(findNullable(byCss(FOUND_SELECTOR), scope), is(notNullValue()));
  }

  @Test
  public void testFindElementInShadowNotFound() {
    Element scope = new ShadowRootElementAdapter(new MockUtilities().getElementAdapter());
    Exception e =
        expectThrows(
            NoSuchElementException.class, () -> findNotNullable(byCss(NOT_FOUND_SELECTOR), scope));
    assertThat(e.getMessage(), containsString(ELEMENT_NOT_FOUND_ERROR));
  }

  @Test
  public void testFindNullableElementInShadowNotFound() {
    Element scope = new ShadowRootElementAdapter(new MockUtilities().getElementAdapter());
    assertThat(findNullable(byCss(NOT_FOUND_SELECTOR), scope), is(nullValue()));
  }

  @Test
  public void testFindElementsInShadow() {
    MockUtilities mock = new MockUtilities();
    final String FOUND_SELECTOR = "found";
    mock.setShadowMock(mock.getWebElementMock(), FOUND_SELECTOR);
    Element scope = new ShadowRootElementAdapter(mock.getElementAdapter());
    assertThat(findNotNullables(byCss(FOUND_SELECTOR), scope), is(not(empty())));
  }

  @Test
  public void testFindNullableElementsInShadow() {
    MockUtilities mock = new MockUtilities();
    final String FOUND_SELECTOR = "found";
    mock.setShadowMock(mock.getWebElementMock(), FOUND_SELECTOR);
    Element scope = new ShadowRootElementAdapter(mock.getElementAdapter());
    assertThat(findNotNullables(byCss(FOUND_SELECTOR), scope), is(not(empty())));
  }

  @Test
  public void testFindElementsInShadowNotFound() {
    Element scope = new ShadowRootElementAdapter(new MockUtilities().getElementAdapter());
    Exception e =
        expectThrows(
            NoSuchElementException.class, () -> findNotNullables(byCss(NOT_FOUND_SELECTOR), scope));
    assertThat(e.getMessage(), containsString(ELEMENT_NOT_FOUND_ERROR));
  }

  @Test
  public void testFindNullableElementsInShadowNotFound() {
    MockUtilities mock = new MockUtilities();
    Element scope = new ShadowRootElementAdapter(mock.getElementAdapter());
    assertThat(findNullables(byCss(NOT_FOUND_SELECTOR), scope), is(nullValue()));
  }
}
