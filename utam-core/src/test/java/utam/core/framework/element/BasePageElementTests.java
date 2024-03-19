/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.element;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.ArgumentMatchers.contains;
import static org.mockito.ArgumentMatchers.refEq;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.testng.Assert.assertThrows;
import static utam.core.selenium.element.ElementAdapter.SCROLL_INTO_VIEW_JS;
import static utam.core.selenium.element.ElementAdapter.SCROLL_TOP_VIA_JAVASCRIPT;

import org.openqa.selenium.JavascriptException;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.Keys;
import org.openqa.selenium.Rectangle;
import org.openqa.selenium.StaleElementReferenceException;
import org.openqa.selenium.TimeoutException;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.testng.annotations.Test;
import utam.core.MockUtilities;
import utam.core.element.Element.ElementRectangle;
import utam.core.element.Locator;
import utam.core.framework.consumer.UtamError;
import utam.core.selenium.element.LocatorBy;

/**
 * @author elizaveta.ivanova
 * @since 234
 */
public class BasePageElementTests {

  @Test
  public void testWaitForAbsence() {
    MockUtilities mock = new MockUtilities();
    when(mock.getWebElementMock().isDisplayed())
        .thenThrow(StaleElementReferenceException.class)
        .thenReturn(true);
    mock.getUtamElement().waitForAbsence();
    assertThrows(TimeoutException.class, () -> mock.getUtamElement().waitForAbsence());
  }

  @Test
  public void testWaitForVisible() {
    MockUtilities mock = new MockUtilities();
    when(mock.getElementAdapter().isDisplayed()).thenReturn(false).thenReturn(true);
    mock.getUtamElement().waitForVisible();
    verify(mock.getWebElementMock(), times(2)).isDisplayed();
  }

  @Test
  public void testWaitForInvisible() {
    MockUtilities mock = new MockUtilities();
    when(mock.getElementAdapter().isDisplayed()).thenReturn(true).thenReturn(false);
    mock.getUtamElement().waitForInvisible();
    verify(mock.getWebElementMock(), times(2)).isDisplayed();
  }

  @Test
  public void testIsVisible() {
    MockUtilities mock = new MockUtilities();
    when(mock.getElementAdapter().isDisplayed()).thenReturn(false);
    assertThat(mock.getUtamElement().isVisible(), is(false));
    when(mock.getElementAdapter().isDisplayed()).thenReturn(true);
    assertThat(mock.getUtamElement().isVisible(), is(true));
    verify(mock.getWebElementMock(), times(2)).isDisplayed();
  }

  @Test
  public void testIsEnabled() {
    MockUtilities mock = new MockUtilities();
    when(mock.getElementAdapter().isEnabled()).thenReturn(false);
    assertThat(mock.getUtamElement().isEnabled(), is(false));
    verify(mock.getWebElementMock(), times(1)).isEnabled();
  }

  @Test
  public void testGetAttribute() {
    String attrName = "name";
    String attrValue = "value";
    MockUtilities mock = new MockUtilities();
    when(mock.getElementAdapter().getAttribute(attrName)).thenReturn(attrValue);
    assertThat(mock.getUtamElement().getAttribute(attrName), is(equalTo(attrValue)));
    verify(mock.getWebElementMock(), times(1)).getAttribute(attrName);
  }

  @Test
  public void testGetCssPropertyValue() {
    String attrName = "name";
    String attrValue = "value";
    MockUtilities mock = new MockUtilities();
    when(mock.getElementAdapter().getCssPropertyValue(attrName)).thenReturn(attrValue);
    assertThat(mock.getUtamElement().getCssPropertyValue(attrName), is(equalTo(attrValue)));
    verify(mock.getWebElementMock(), times(1)).getCssValue(attrName);
  }

  @Test
  public void testGetClassAttribute() {
    String attr = "class";
    MockUtilities mock = new MockUtilities();
    when(mock.getElementAdapter().getAttribute(attr)).thenReturn(attr);
    assertThat(mock.getUtamElement().getClassAttribute(), is(equalTo(attr)));
    verify(mock.getWebElementMock(), times(1)).getAttribute(attr);
  }

  @Test
  public void testGetText() {
    MockUtilities mock = new MockUtilities();
    when(mock.getWebElementMock().getText()).thenReturn("text");
    assertThat(mock.getUtamElement().getText(), is(equalTo("text")));
    verify(mock.getWebElementMock(), times(1)).getText();
  }

  @Test
  public void testSetText() {
    MockUtilities mock = new MockUtilities();
    mock.getUtamElement().setText("str");
    verify(mock.getWebElementMock(), times(1)).sendKeys("str");
  }

  @Test
  public void testGetTitle() {
    String attr = "title";
    MockUtilities mock = new MockUtilities();
    when(mock.getElementAdapter().getAttribute(attr)).thenReturn(attr);
    assertThat(mock.getUtamElement().getTitle(), is(equalTo(attr)));
  }

  @Test
  public void testGetValue() {
    String attr = "value";
    MockUtilities mock = new MockUtilities();
    when(mock.getElementAdapter().getAttribute(attr)).thenReturn(attr);
    assertThat(mock.getUtamElement().getValue(), is(equalTo(attr)));
  }

  @Test
  public void testMoveTo() {
    MockUtilities mock = new MockUtilities();
    mock.getUtamElement().moveTo();
  }

  @Test
  public void testScrollToCenter() {
    MockUtilities mock = new MockUtilities();
    mock.getUtamElement().scrollToCenter();
  }

  @Test
  public void testScrollToTopWithElementAlreadyInView() {
    MockUtilities mock = new MockUtilities();
    when(mock.getElementAdapter().isDisplayed()).thenReturn(true);
    mock.getUtamElement().scrollToTop();
  }

  @Test
  public void testScrollToTopWithElementAlignedToBottom() {
    MockUtilities mock = new MockUtilities();
    when(((JavascriptExecutor) mock.getWebDriverMock())
            .executeScript(contains(SCROLL_INTO_VIEW_JS), refEq(mock.getWebElementMock())))
        .then(
            (invocation) ->
                when(((WebElement) invocation.getArgument(1)).isDisplayed()).thenReturn(true));
    mock.getUtamElement().scrollToTop();
  }

  @Test
  public void testScrollToTopWithElementAlignedToTop() {
    MockUtilities mock = new MockUtilities();
    when(((JavascriptExecutor) mock.getWebDriverMock())
            .executeScript(contains(SCROLL_TOP_VIA_JAVASCRIPT), refEq(mock.getWebElementMock())))
        .then(
            (invocation) ->
                when(((WebElement) invocation.getArgument(1)).isDisplayed()).thenReturn(true));
    mock.getUtamElement().scrollToTop();
  }

  @Test
  public void testScrollIntoViewIfElementNotVisibleThrows() {
    MockUtilities mock = new MockUtilities();
    assertThrows(UtamError.class, () -> mock.getUtamElement().scrollToTop());
  }

  @Test
  public void testIsFocused() {
    MockUtilities mock = new MockUtilities();
    WebDriver.TargetLocator targetLocator = mock(WebDriver.TargetLocator.class);
    when(mock.getWebDriverMock().switchTo()).thenReturn(targetLocator);
    when(targetLocator.activeElement()).thenReturn(mock.getWebElementMock());
    assertThat(mock.getUtamElement().isFocused(), is(true));
  }

  @Test
  public void testFocus() {
    MockUtilities mock = new MockUtilities();
    mock.getUtamElement().focus();
  }

  @Test
  public void testBlur() {
    MockUtilities mock = new MockUtilities();
    mock.getUtamElement().blur();
  }

  @Test
  public void testContainsElement() {
    MockUtilities mock = new MockUtilities.MockAdapter();
    Locator locator = LocatorBy.byCss("css");
    assertThat(mock.getUtamElement().containsElement(locator, false), is(false));
    assertThat(mock.getUtamElement().containsElement(locator, true), is(false));
    when(mock.getElementAdapter().containsElements(locator)).thenReturn(1);
    assertThat(mock.getUtamElement().containsElement(locator), is(true));
  }

  @Test
  public void testClear() {
    MockUtilities mock = new MockUtilities.MockAdapter();
    mock.getUtamElement().clear();
    verify(mock.getElementAdapter(), times(1)).clear();
  }

  @Test
  public void testClearAndType() {
    String text = "text";
    MockUtilities mock = new MockUtilities.MockAdapter();
    mock.getUtamElement().clearAndType(text);
    verify(mock.getElementAdapter(), times(1)).clear();
    verify(mock.getElementAdapter(), times(1)).setText(text);
  }

  @Test
  public void testPress() {
    MockUtilities mock = new MockUtilities();
    final String KEY = "Enter";
    mock.getUtamElement().press(KEY);
    verify(mock.getWebElementMock(), times(1)).sendKeys(Keys.ENTER.toString());
  }

  @Test
  public void testClick() {
    MockUtilities mock = new MockUtilities();
    mock.getUtamElement().click();
    verify(mock.getWebElementMock(), times(1)).click();
  }

  @Test
  public void testClickWithException() {
    MockUtilities mock = new MockUtilities();
    doThrow(
            new JavascriptException(
                "javascript error: Cannot read property 'defaultView' of undefined"))
        .when(mock.getWebElementMock())
        .click();
    // fall back on javascript click
    mock.getUtamElement().click();
    // fallback regardless of the message, aligns with JavaScript
    mock.getUtamElement().click();
  }

  @Test
  public void testDoubleClick() {
    MockUtilities mock = new MockUtilities();
    mock.getUtamElement().doubleClick();
  }

  @Test
  public void testClickAndHold() {
    MockUtilities mock = new MockUtilities();
    mock.getUtamElement().clickAndHold(1);
  }

  @Test
  public void testRightClick() {
    MockUtilities mock = new MockUtilities();
    mock.getUtamElement().rightClick();
  }

  @Test
  public void testFlick() {
    MockUtilities mock = new MockUtilities();
    assertThrows(() -> mock.getUtamElement().flick(0, 0));
  }

  @Test
  public void testDragAndDrop() {
    MockUtilities mock = new MockUtilities();
    mock.getUtamElement().dragAndDrop(mock.getUtamElement());
    mock.getUtamElement().dragAndDrop(mock.getUtamElement(), 1);
    mock.getUtamElement().dragAndDropByOffset(1, 2);
    mock.getUtamElement().dragAndDropByOffset(1, 2, 1);
  }

  @Test
  public void testGetRectangle() {
    MockUtilities mock = new MockUtilities();
    when(mock.getWebElementMock().getRect()).thenReturn(new Rectangle(1, 2, 3, 4));
    ElementRectangle test = mock.getUtamElement().getRect();
    assertThat(test.getX(), equalTo(1));
    assertThat(test.getY(), equalTo(2));
    assertThat(test.getWidth(), equalTo(4));
    assertThat(test.getHeight(), equalTo(3));
  }
}
