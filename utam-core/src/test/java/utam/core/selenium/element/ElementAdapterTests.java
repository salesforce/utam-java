/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.selenium.element;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.CoreMatchers.sameInstance;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.empty;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.not;
import static org.mockito.ArgumentMatchers.contains;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.refEq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.testng.Assert.assertThrows;
import static org.testng.Assert.expectThrows;
import static utam.core.selenium.element.ElementAdapter.BLUR_VIA_JAVASCRIPT;
import static utam.core.selenium.element.ElementAdapter.CLICK_VIA_JAVASCRIPT;
import static utam.core.selenium.element.ElementAdapter.ERR_NULL_ELEMENT;
import static utam.core.selenium.element.ElementAdapter.FOCUS_VIA_JAVASCRIPT;
import static utam.core.selenium.element.ElementAdapter.IS_PARENT_NODE_SHADOW_ROOT_JS;
import static utam.core.selenium.element.ElementAdapter.ROOT_NODE_GET_ACTIVE_ELEMENT_JS;
import static utam.core.selenium.element.ElementAdapter.SCROLL_CENTER_VIA_JAVASCRIPT;
import static utam.core.selenium.element.ElementAdapter.SCROLL_TOP_VIA_JAVASCRIPT;
import static utam.core.selenium.element.LocatorBy.byCss;
import static utam.core.selenium.element.ShadowRootWebElement.GET_SHADOW_ROOT_QUERY_SELECTOR_ALL;

import java.util.Collections;
import java.util.List;
import org.openqa.selenium.By;
import org.openqa.selenium.NoSuchElementException;
import org.openqa.selenium.Rectangle;
import org.openqa.selenium.StaleElementReferenceException;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.testng.annotations.Test;
import utam.core.MockUtilities;
import utam.core.driver.Driver;
import utam.core.element.DragAndDropOptions;
import utam.core.element.Element;
import utam.core.element.Element.ElementRectangle;
import utam.core.element.Element.ScrollOptions;

/**
 * element tests
 *
 * @author jim.evans, elizaveta.ivanova
 */
public class ElementAdapterTests {

  static final String NOT_FOUND_SELECTOR = "not_found";
  static final String ELEMENT_NOT_FOUND_ERROR =
      "can't find element with locator 'By.cssSelector: not_found'";

  static ElementAdapter findNotNullable(LocatorBy locator, Element scope) {
    return (ElementAdapter) scope.findElement(locator, false);
  }

  static ElementAdapter findNullable(LocatorBy locator, Element scope) {
    Element res = scope.findElement(locator, true);
    return res == null ? null : (ElementAdapter) res;
  }

  static List<Element> findNotNullables(LocatorBy locator, Element scope) {
    return scope.findElements(locator, false);
  }

  static List<Element> findNullables(LocatorBy locator, Element scope) {
    return scope.findElements(locator, true);
  }

  @Test
  public void testFindElement() {
    MockUtilities mock = new MockUtilities();
    WebElement foundMock = mock(WebElement.class);
    Element scope = mock.getElementAdapter();
    final String FOUND_SELECTOR = "found";
    when(mock.getWebElementMock().findElements(By.cssSelector(FOUND_SELECTOR)))
        .thenReturn(Collections.singletonList(foundMock));
    ElementAdapter found = findNotNullable(byCss(FOUND_SELECTOR), scope);
    assertThat(found.getWebElement(), is(sameInstance(foundMock)));
  }

  @Test
  public void testFindElementNotFound() {
    Element scope = new MockUtilities().getElementAdapter();
    Exception e =
        expectThrows(
            NoSuchElementException.class, () -> findNotNullable(byCss(NOT_FOUND_SELECTOR), scope));
    assertThat(e.getMessage(), containsString(ELEMENT_NOT_FOUND_ERROR));
  }

  @Test
  public void testFindElementNullable() {
    MockUtilities mock = new MockUtilities();
    WebElement foundMock = mock(WebElement.class);
    Element scope = mock.getElementAdapter();
    final String FOUND_SELECTOR = "found";
    when(mock.getWebElementMock().findElements(By.cssSelector(FOUND_SELECTOR)))
        .thenReturn(Collections.singletonList(foundMock));
    ElementAdapter found = findNullable(byCss(FOUND_SELECTOR), scope);
    assertThat(found.getWebElement(), is(sameInstance(foundMock)));
  }

  @Test
  public void testFindElementNullableNotFound() {
    Element scope = new MockUtilities().getElementAdapter();
    assertThat(findNullable(byCss(NOT_FOUND_SELECTOR), scope), is(nullValue()));
  }

  @Test
  public void testFindElements() {
    MockUtilities mock = new MockUtilities();
    Element test = mock.getElementAdapter();
    final String FOUND_SELECTOR = "found";
    when(mock.getWebElementMock().findElements(By.cssSelector(FOUND_SELECTOR)))
        .thenReturn(Collections.singletonList(mock(WebElement.class)));
    assertThat(findNotNullables(byCss(FOUND_SELECTOR), test), is(not(empty())));
  }

  @Test
  public void testFindElementsNotFound() {
    Element scope = new MockUtilities().getElementAdapter();
    Exception e =
        expectThrows(
            NoSuchElementException.class, () -> findNotNullables(byCss(NOT_FOUND_SELECTOR), scope));
    assertThat(e.getMessage(), containsString(ELEMENT_NOT_FOUND_ERROR));
  }

  @Test
  public void testFindElementsNullableNotFound() {
    Element scope = new MockUtilities().getElementAdapter();
    assertThat(findNullables(byCss(NOT_FOUND_SELECTOR), scope), is(nullValue()));
  }

  @Test
  public void testClick() {
    MockUtilities mock = new MockUtilities();
    mock.getElementAdapter().click();
    verify(mock.getWebElementMock(), times(1)).click();
  }

  @Test
  public void testDoubleClick() {
    MockUtilities mock = new MockUtilities();
    mock.getElementAdapter().doubleClick();
  }

  @Test
  public void testRightClick() {
    MockUtilities mock = new MockUtilities();
    mock.getElementAdapter().rightClick();
  }

  @Test
  public void testClickAndHold() {
    MockUtilities mock = new MockUtilities();
    mock.getElementAdapter().clickAndHold(1);
  }

  @Test
  public void testJavascriptClick() {
    MockUtilities mock = new MockUtilities.MockDriver();
    mock.getElementAdapter().deprecatedClick();
    verify(mock.getDriverAdapter(), times(1))
        .executeScript(CLICK_VIA_JAVASCRIPT, mock.getWebElementMock());
  }

  @Test
  public void testClear() {
    MockUtilities mock = new MockUtilities();
    mock.getElementAdapter().clear();
    verify(mock.getWebElementMock(), times(1)).clear();
  }

  @Test
  public void testSetText() {
    String text = "text";
    MockUtilities mock = new MockUtilities();
    mock.getElementAdapter().setText(text);
    verify(mock.getWebElementMock(), times(1)).sendKeys(text);
  }

  @Test
  public void testIsExisting() {
    MockUtilities mock = new MockUtilities();
    when(mock.getWebElementMock().isDisplayed()).thenReturn(true);
    assertThat(mock.getElementAdapter().isExisting(), is(true));
    when(mock.getWebElementMock().isDisplayed()).thenThrow(StaleElementReferenceException.class);
    assertThat(mock.getElementAdapter().isExisting(), is(false));
  }

  @Test
  public void testIsExistingNotFound() {
    MockUtilities mock = new MockUtilities();
    when(mock.getWebElementMock().isDisplayed())
        .thenThrow(org.openqa.selenium.NoSuchElementException.class);
    assertThat(mock.getElementAdapter().isExisting(), is(false));
  }

  @Test
  public void testIsDisplayed() {
    MockUtilities mock = new MockUtilities();
    when(mock.getWebElementMock().isDisplayed()).thenReturn(true);
    assertThat(mock.getElementAdapter().isDisplayed(), is(true));
    verify(mock.getWebElementMock(), times(1)).isDisplayed();
  }

  @Test
  public void testGetAttribute() {
    String attr = "nameAndValue";
    MockUtilities mock = new MockUtilities();
    when(mock.getWebElementMock().getAttribute(attr)).thenReturn(attr);
    assertThat(mock.getElementAdapter().getAttribute(attr), is(equalTo(attr)));
    verify(mock.getWebElementMock(), times(1)).getAttribute(attr);
  }

  @Test
  public void testGetCssPropertyValue() {
    String attr = "nameAndValue";
    MockUtilities mock = new MockUtilities();
    when(mock.getWebElementMock().getCssValue(attr)).thenReturn(attr);
    assertThat(mock.getElementAdapter().getCssPropertyValue(attr), is(equalTo(attr)));
    verify(mock.getWebElementMock(), times(1)).getCssValue(attr);
  }

  @Test
  public void testGetText() {
    String text = "text";
    MockUtilities mock = new MockUtilities();
    when(mock.getWebElementMock().getText()).thenReturn(text);
    assertThat(mock.getElementAdapter().getText(), is(equalTo(text)));
  }

  @Test
  public void testMoveTo() {
    MockUtilities mock = new MockUtilities.MockDriver();
    mock.getElementAdapter().moveTo();
  }

  @Test
  public void testIsEnabled() {
    MockUtilities mock = new MockUtilities();
    when(mock.getWebElementMock().isEnabled()).thenReturn(true);
    assertThat(mock.getElementAdapter().isEnabled(), is(true));
    verify(mock.getWebElementMock(), times(1)).isEnabled();
  }

  @Test
  public void testHasFocus() {
    MockUtilities mock = new MockUtilities();
    Element element = mock.getElementAdapter();
    WebDriver.TargetLocator targetLocator = mock(WebDriver.TargetLocator.class);
    when(mock.getWebDriverMock().switchTo()).thenReturn(targetLocator);
    when(targetLocator.activeElement()).thenReturn(mock(WebElement.class));
    assertThat("element has no focus", element.hasFocus(), is(false));
    when(targetLocator.activeElement()).thenReturn(mock.getWebElementMock());
    assertThat("element has focus", element.hasFocus(), is(true));
  }

  @Test
  public void testHasFocusInsideShadowRoot() {
    MockUtilities mockUtils = new MockUtilities();
    WebElement elementMock = mockUtils.getWebElementMock();
    Driver driverAdapterMock = mockUtils.getDriverAdapter();
    Element element = mockUtils.getElementAdapter();
    when(driverAdapterMock.executeScript(
            contains(IS_PARENT_NODE_SHADOW_ROOT_JS), refEq(elementMock)))
        .thenReturn(true);
    assertThat("element has no focus", element.hasFocus(), is(false));
    when(driverAdapterMock.executeScript(
            contains(ROOT_NODE_GET_ACTIVE_ELEMENT_JS), refEq(elementMock)))
        .thenReturn(elementMock);
    assertThat("element has focus", element.hasFocus(), is(true));
  }

  @Test
  public void testFocus() {
    MockUtilities mock = new MockUtilities.MockDriver();
    mock.getElementAdapter().focus();
    verify(mock.getDriverAdapter(), times(1))
        .executeScript(FOCUS_VIA_JAVASCRIPT, mock.getWebElementMock());
  }

  @Test
  public void testScrollToTop() {
    MockUtilities mock = new MockUtilities.MockDriver();
    when(mock.getElementAdapter().isDisplayed())
        .thenReturn(false)
        .thenReturn(false)
        .thenReturn(false)
        .thenReturn(true);
    mock.getElementAdapter().scrollIntoView(ScrollOptions.TOP);
    verify(mock.getDriverAdapter(), times(2))
        .executeScript(SCROLL_TOP_VIA_JAVASCRIPT, mock.getWebElementMock());
  }

  @Test
  public void testScrollToCenter() {
    MockUtilities mock = new MockUtilities.MockDriver();
    mock.getElementAdapter().scrollIntoView(ScrollOptions.CENTER);
    verify(mock.getDriverAdapter(), times(1))
        .executeScript(SCROLL_CENTER_VIA_JAVASCRIPT, mock.getWebElementMock());
  }

  @Test
  public void testBlur() {
    MockUtilities mock = new MockUtilities();
    mock.getElementAdapter().blur();
    verify(mock.getExecutorMock(), times(1))
        .executeScript(BLUR_VIA_JAVASCRIPT, mock.getWebElementMock());
  }

  @Test
  public void testContainsElement() {
    MockUtilities mock = new MockUtilities();
    when(mock.getWebElementMock().findElements(By.cssSelector("css")))
        .thenReturn(Collections.singletonList(mock.getWebElementMock()));
    assertThat(mock.getElementAdapter().containsElements(byCss("css")), is(equalTo(1)));
    assertThat(mock.getElementAdapter().containsElements(byCss("css1")), is(equalTo(0)));
  }

  @Test
  public void testContainsElementInShadow() {
    MockUtilities mock = new MockUtilities();
    when(mock.getExecutorMock()
            .executeScript(
                contains(String.format(GET_SHADOW_ROOT_QUERY_SELECTOR_ALL, "css")),
                refEq(mock.getWebElementMock())))
        .thenReturn(Collections.singletonList(mock.getWebElementMock()));
    assertThat(
        new ShadowRootElementAdapter(mock.getElementAdapter()).containsElements(byCss("css")),
        is(equalTo(1)));
  }

  @Test
  public void testMobileActionsThrow() {
    MockUtilities mock = new MockUtilities();
    assertThrows(() -> mock.getElementAdapter().flick(0, 0));
  }

  @Test
  public void testDragAndDropWithTargetElement() {
    MockUtilities mock = new MockUtilities.MockDriver();
    mock.getElementAdapter()
        .dragAndDrop(new DragAndDropOptions.ByElement(mock.getElementAdapter()));
    mock.getElementAdapter()
        .dragAndDrop(new DragAndDropOptions.ByElement(mock.getElementAdapter(), 1));
  }

  @Test
  public void testDragAndDropWithOffset() {
    MockUtilities mock = new MockUtilities.MockDriver();
    mock.getElementAdapter().dragAndDrop(new DragAndDropOptions.ByOffset(1, 1));
    mock.getElementAdapter().dragAndDrop(new DragAndDropOptions.ByOffset(1, 1, 1));
  }

  @Test
  public void testGetWebElement() {
    NullPointerException e =
        expectThrows(
            NullPointerException.class,
            () -> new ElementAdapter(null, new MockUtilities().getDriverAdapter()).getWebElement());
    assertThat(e.getMessage(), containsString(ERR_NULL_ELEMENT));
  }

  @Test
  public void testGetRect() {
    MockUtilities mock = new MockUtilities();
    when(mock.getWebElementMock().getRect()).thenReturn(new Rectangle(0, 1, 2, 3));
    ElementRectangle rectangle = mock.getElementAdapter().getRect();
    assertThat(rectangle.getX(), equalTo(0));
    assertThat(rectangle.getY(), equalTo(1));
    assertThat(rectangle.getHeight(), equalTo(2));
    assertThat(rectangle.getWidth(), equalTo(3));
  }
}
