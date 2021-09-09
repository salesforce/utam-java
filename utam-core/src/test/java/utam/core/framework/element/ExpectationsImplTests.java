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
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;
import static org.mockito.ArgumentMatchers.contains;
import static org.mockito.ArgumentMatchers.refEq;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.testng.Assert.assertThrows;
import static org.testng.Assert.expectThrows;
import static utam.core.framework.element.ElementExpectations.absence;
import static utam.core.framework.element.ElementExpectations.blur;
import static utam.core.framework.element.ElementExpectations.clear;
import static utam.core.framework.element.ElementExpectations.clearAndType;
import static utam.core.framework.element.ElementExpectations.click;
import static utam.core.framework.element.ElementExpectations.flick;
import static utam.core.framework.element.ElementExpectations.focus;
import static utam.core.framework.element.ElementExpectations.getAttribute;
import static utam.core.framework.element.ElementExpectations.getText;
import static utam.core.framework.element.ElementExpectations.isPresent;
import static utam.core.framework.element.ElementExpectations.javascriptClick;
import static utam.core.framework.element.ElementExpectations.moveTo;
import static utam.core.framework.element.ElementExpectations.setText;
import static utam.core.framework.element.ElementExpectations.visibility;
import static utam.core.framework.element.ElementExpectations.waitFor;
import static utam.core.selenium.element.ElementAdapter.NULL_ELEMENT;
import static utam.core.selenium.element.ElementAdapter.SCROLL_INTO_VIEW_JS;
import static utam.core.selenium.element.ElementAdapter.SCROLL_TOP_VIA_JAVASCRIPT;

import io.appium.java_client.android.AndroidDriver;
import org.hamcrest.Matchers;
import org.hamcrest.core.IsEqual;
import org.openqa.selenium.Dimension;
import org.openqa.selenium.ElementNotVisibleException;
import org.openqa.selenium.JavascriptException;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.Point;
import org.openqa.selenium.Rectangle;
import org.openqa.selenium.StaleElementReferenceException;
import org.openqa.selenium.WebElement;
import org.testng.annotations.Test;
import utam.core.MockUtilities;
import utam.core.driver.Driver;
import utam.core.driver.Expectations;
import utam.core.element.Element;
import utam.core.element.Element.ScrollOptions;
import utam.core.framework.element.ElementExpectations.Match;

/**
 * @author elizaveta.ivanova
 * @since 234
 */
public class ExpectationsImplTests {

  @Test
  public void testGetLogMessage() {
    Expectations<Object> expectations = new ExpectationsImpl("message",
        (driver, object) -> object);
    assertThat(expectations.getLogMessage(), is(equalTo("message")));
  }

  @Test
  public void testApply() {
    Expectations<Object> expectations = new ExpectationsImpl("message",
        (driver, object) -> object);
    Driver driverMock = mock(Driver.class);
    assertThat(expectations.apply(driverMock, null), is(nullValue()));
    expectThrows(NullPointerException.class, () -> expectations.apply(driverMock, NULL_ELEMENT));
    Element element = mock(Element.class);
    assertThat(expectations.apply(driverMock, element), is(equalTo(element)));
  }

  /**
   * Tests that Match values can be translated to boolean primitives via the enum's is() static
   * method
   */
  @Test
  public void testTranslateToBooleanPrimitive() {
    assertThat(ElementExpectations.Match.TRUE.is(), Matchers.is(Matchers.equalTo(true)));
    assertThat(ElementExpectations.Match.FALSE.is(), Matchers.is(Matchers.equalTo(false)));
  }

  /**
   * Tests that Match values can be generated from boolean primitives
   */
  @Test
  public void testTranslateFromBooleanPrimitive() {
    assertThat(ElementExpectations.Match.from(true),
        Matchers.is(Matchers.equalTo(ElementExpectations.Match.TRUE)));
    assertThat(ElementExpectations.Match.from(false),
        Matchers.is(Matchers.equalTo(ElementExpectations.Match.FALSE)));
  }

  @Test
  public void testWaitForAbsence() {
    MockUtilities mock = new MockUtilities();
    Expectations<Boolean> expectations = absence();
    assertThat(expectations.apply(mock.getDriverAdapter(), mock.getElementAdapter()),
        Matchers.is(false));
    when(mock.getWebElementMock().isDisplayed()).thenThrow(StaleElementReferenceException.class);
    assertThat(
        expectations.apply(mock.getDriverAdapter(), mock.getElementAdapter()),
        Matchers.is(true));
  }

  @Test
  public void testVisible() {
    MockUtilities mock = new MockUtilities();
    when(mock.getWebElementMock().isDisplayed()).thenReturn(false);
    Expectations<Boolean> visible = visibility(true);
    Expectations<Boolean> invisible = visibility(false);
    assertThat(visible.apply(mock.getDriverAdapter(), mock.getElementAdapter()),
        Matchers.is(false));
    assertThat(invisible.apply(mock.getDriverAdapter(), mock.getElementAdapter()),
        Matchers.is(true));
    when(mock.getWebElementMock().isDisplayed()).thenReturn(true);
    assertThat(visible.apply(mock.getDriverAdapter(), mock.getElementAdapter()), Matchers.is(true));
    assertThat(invisible.apply(mock.getDriverAdapter(), mock.getElementAdapter()),
        Matchers.is(false));
  }

  @Test
  public void testClear() {
    MockUtilities mock = new MockUtilities();
    Expectations<Boolean> expectations = clear();
    assertThat(expectations.apply(mock.getDriverAdapter(), mock.getElementAdapter()),
        Matchers.is(notNullValue()));
  }

  @Test
  public void testClearAndType() {
    final String testValue = "testValue";
    MockUtilities mock = new MockUtilities();
    Expectations<Boolean> expectations = clearAndType(testValue);
    assertThat(expectations.apply(mock.getDriverAdapter(), mock.getElementAdapter()),
        Matchers.is(notNullValue()));
  }

  @Test
  public void testGetAttribute() {
    final String attributeName = "attributeName";
    final String attributeValue = "attributeValue";
    MockUtilities mock = new MockUtilities();
    when(mock.getWebElementMock().getAttribute(attributeName)).thenReturn(attributeValue);
    Expectations<String> expectations = getAttribute(attributeName);
    assertThat(expectations.apply(mock.getDriverAdapter(), mock.getElementAdapter()),
        Matchers.is(IsEqual.equalTo(attributeValue)));
    when(mock.getWebElementMock().getAttribute(attributeName)).thenReturn(null);
    assertThat(expectations.apply(mock.getDriverAdapter(), mock.getElementAdapter()),
        Matchers.is(Matchers.nullValue()));
  }

  @Test
  public void testGetText() {
    final String elementText = "textValue";
    MockUtilities mock = new MockUtilities();
    when(mock.getWebElementMock().getText()).thenReturn(elementText);
    Expectations<String> expectations = getText();
    assertThat(expectations.apply(mock.getDriverAdapter(), mock.getElementAdapter()),
        Matchers.is(IsEqual.equalTo(elementText)));
  }

  @Test
  public void testSetText() {
    final String textValue = "textValue";
    MockUtilities mock = new MockUtilities();
    Expectations<Boolean> expectations = setText(textValue);
    assertThat(expectations.apply(mock.getDriverAdapter(), mock.getElementAdapter()),
        Matchers.is(notNullValue()));
  }

  @Test
  public void testIsPresent() {
    MockUtilities mock = new MockUtilities();
    Expectations<Match> expectations = isPresent();
    assertThat(expectations.apply(mock.getDriverAdapter(), mock.getElementAdapter()),
        Matchers.is(IsEqual.equalTo(Match.TRUE)));
  }

  @Test
  public void testJavascriptClick() {
    MockUtilities mock = new MockUtilities();
    Expectations<Boolean> expectations = javascriptClick();
    assertThat(expectations.apply(mock.getDriverAdapter(), mock.getElementAdapter()),
        Matchers.is(notNullValue()));
  }

  @Test
  public void testClick() {
    MockUtilities mock = new MockUtilities();
    Expectations<Boolean> expectations = click();
    assertThat(expectations.apply(mock.getDriverAdapter(), mock.getElementAdapter()),
        Matchers.is(notNullValue()));
    doThrow(new JavascriptException(
        "javascript error: Cannot read property 'defaultView' of undefined"))
        .when(mock.getWebElementMock()).click();
    assertThat(expectations.apply(mock.getDriverAdapter(), mock.getElementAdapter()),
        Matchers.is(notNullValue()));
    doThrow(new JavascriptException("javascript error: unknown JS error"))
        .when(mock.getWebElementMock()).click();
    assertThrows(() -> expectations.apply(mock.getDriverAdapter(), mock.getElementAdapter()));
  }

  @Test
  public void testMoveTo() {
    MockUtilities mock = new MockUtilities();
    Expectations<Boolean> expectations = moveTo();
    assertThat(expectations.apply(mock.getDriverAdapter(), mock.getElementAdapter()),
        Matchers.is(notNullValue()));
  }

  @Test
  public void testFocus() {
    MockUtilities mock = new MockUtilities();
    Expectations<Boolean> expectations = focus();
    assertThat(expectations.apply(mock.getDriverAdapter(), mock.getElementAdapter()),
        Matchers.is(notNullValue()));
  }

  @Test
  public void testScrollToTopWithElementAlreadyInView() {
    MockUtilities mock = new MockUtilities();
    when(mock.getWebElementMock().isDisplayed()).thenReturn(true);
    Expectations<Boolean> scrollTo = ElementExpectations.scrollTo(ScrollOptions.TOP);
    assertThat(scrollTo.apply(mock.getDriverAdapter(), mock.getElementAdapter()),
        Matchers.is(notNullValue()));
  }

  @Test
  public void testScrollToTopWithElementAlignedToBottom() {
    MockUtilities mock = new MockUtilities();
    when(((JavascriptExecutor) mock.getWebDriverMock()).executeScript(
        contains(SCROLL_INTO_VIEW_JS),
        refEq(mock.getWebElementMock()))).then((invocation) -> when(
        ((WebElement) invocation.getArgument(1)).isDisplayed()).thenReturn(true));
    Expectations<Boolean> scrollTo = ElementExpectations.scrollTo(ScrollOptions.TOP);
    assertThat(scrollTo.apply(mock.getDriverAdapter(), mock.getElementAdapter()),
        Matchers.is(notNullValue()));
  }

  @Test
  public void testScrollToTopWithElementAlignedToTop() {
    MockUtilities mock = new MockUtilities();
    when(((JavascriptExecutor) mock.getWebDriverMock()).executeScript(
        contains(SCROLL_TOP_VIA_JAVASCRIPT),
        refEq(mock.getWebElementMock()))).then((invocation) -> when(
        ((WebElement) invocation.getArgument(1)).isDisplayed()).thenReturn(true));
    Expectations<Boolean> scrollTo = ElementExpectations.scrollTo(ScrollOptions.TOP);
    assertThat(scrollTo.apply(mock.getDriverAdapter(), mock.getElementAdapter()),
        Matchers.is(notNullValue()));
  }

  @Test
  public void testScrollIntoViewIfElementNotVisibleThrows() {
    MockUtilities mock = new MockUtilities();
    assertThrows(
        ElementNotVisibleException.class,
        () -> ElementExpectations.scrollTo(ScrollOptions.TOP)
            .apply(mock.getDriverAdapter(), mock.getElementAdapter()));
  }

  @Test
  public void testBlur() {
    MockUtilities mock = new MockUtilities();
    Expectations<Boolean> expectations = blur();
    assertThat(expectations.apply(mock.getDriverAdapter(), mock.getElementAdapter()),
        Matchers.is(notNullValue()));
  }

  @Test
  public void testWaitFor() {
    MockUtilities mock = new MockUtilities();
    Expectations<Element> expectations = waitFor(mock::getElementAdapter);
    assertThat(expectations.apply(mock.getDriverAdapter(), mock.getElementAdapter()),
        Matchers.is(notNullValue()));
  }

  @Test
  public void testFlick() {
    MockUtilities mock = new MockUtilities(AndroidDriver.class);
    when(mock.getMobileDriverAdapter().getWebViewElement()).thenReturn(mock.getWebElementMock());
    when(mock.getWebElementMock().getLocation()).thenReturn(new Point(125,125));
    when(mock.getWebElementMock().getRect()).thenReturn(new Rectangle(125,125, 5, 5));
    when(mock.getWebElementMock().getSize()).thenReturn(new Dimension(5,5));
    Expectations expectations = flick(1,1);
    assertThat(expectations.apply(mock.getDriverAdapter(), mock.getElementAdapter()),
        Matchers.is(notNullValue()));
  }
}
