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
import static org.hamcrest.Matchers.greaterThanOrEqualTo;
import static org.hamcrest.Matchers.instanceOf;
import static org.hamcrest.Matchers.lessThanOrEqualTo;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.notNullValue;
import static org.mockito.Mockito.any;
import static org.mockito.Mockito.contains;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.testng.Assert.assertThrows;
import static org.testng.Assert.expectThrows;
import static utam.core.selenium.element.DriverAdapter.ERR_CANT_ENTER_NULL_FRAME;
import static utam.core.selenium.element.DriverAdapter.ERR_SUPPORTED_FOR_MOBILE;

import java.time.Duration;
import java.time.Instant;
import java.util.Collections;
import org.openqa.selenium.By;
import org.openqa.selenium.NoSuchElementException;
import org.openqa.selenium.TimeoutException;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebDriver.Navigation;
import org.openqa.selenium.WebElement;
import org.testng.annotations.Test;
import utam.core.MockUtilities;
import utam.core.driver.Document;
import utam.core.driver.Driver;
import utam.core.element.Element;
import utam.core.framework.consumer.UtamError;
import utam.core.framework.context.PlatformType;

public class DriverAdapterTests {

  @Test
  public void testCreation() {
    MockUtilities mock = new MockUtilities();
    DriverAdapter adapter = (DriverAdapter) mock.getDriverAdapter();
    assertThat(adapter.getSeleniumDriver(), is(instanceOf(WebDriver.class)));
    assertThat(adapter.isNativeContext(), is(false));
    assertThat(adapter.getSeleniumDriver(), is(notNullValue()));
  }

  /** Tests that the executeJavaScript method will execute JavaScript */
  @Test
  public void testExecuteScript() {
    MockUtilities mock = new MockUtilities();
    when(mock.getExecutorMock().executeScript(contains("return arguments[0]"), any()))
        .then((invocation) -> invocation.getArgument(1));
    final String JAVASCRIPT_RETURN_VALUE = "testValue";
    Object scriptReturnValue =
        mock.getDriverAdapter().executeScript("return arguments[0]", JAVASCRIPT_RETURN_VALUE);
    assertThat(scriptReturnValue, is(instanceOf(Object.class)));
    assertThat(scriptReturnValue.toString(), is(equalTo(JAVASCRIPT_RETURN_VALUE)));
  }

  @Test
  public void testSetPageContextToNative() {
    IllegalStateException e =
        expectThrows(
            IllegalStateException.class,
            () -> new MockUtilities().getDriverAdapter().setPageContextToNative());
    assertThat(e.getMessage(), containsString(ERR_SUPPORTED_FOR_MOBILE));
  }

  @Test
  public void testSetPageContextToWebView() {
    IllegalStateException e =
        expectThrows(
            IllegalStateException.class,
            () -> new MockUtilities().getDriverAdapter().setPageContextToWebView("title"));
    assertThat(e.getMessage(), containsString(ERR_SUPPORTED_FOR_MOBILE));
  }

  @Test
  public void testGetContext() {
    IllegalStateException e =
        expectThrows(
            IllegalStateException.class,
            () -> new MockUtilities().getDriverAdapter().getPageContext());
    assertThat(e.getMessage(), containsString(ERR_SUPPORTED_FOR_MOBILE));
  }

  @Test
  public void testFindElement() {
    MockUtilities mock = new MockUtilities();
    assertThrows(() -> mock.getDriverAdapter().findElement(LocatorBy.byCss("not-existing")));
    when(mock.getWebDriverMock().findElement(By.cssSelector("test")))
        .thenReturn(mock(WebElement.class));
    assertThat(mock.getDriverAdapter().findElement(LocatorBy.byCss("test")), is(notNullValue()));
  }

  @Test
  public void testFindElements() {
    MockUtilities mock = new MockUtilities();
    Driver driver = mock.getDriverAdapter();
    assertThrows(() -> driver.findElements(LocatorBy.byCss("not-existing")));
    when(mock.getWebDriverMock().findElements(By.cssSelector("test")))
        .thenReturn(Collections.singletonList(mock(WebElement.class)));
    assertThat(driver.findElements(LocatorBy.byCss("test")), is(not(empty())));
  }

  @Test
  public void testWaitFor() {
    Driver driver = new MockUtilities().getDriverAdapter();
    assertThat(driver.waitFor(() -> true, "test", Duration.ofSeconds(10)), is(true));
  }

  @Test
  public void testWaitForThrowsTimeout() {
    Driver driver = new MockUtilities().getDriverAdapter();
    Instant start = Instant.now();
    TimeoutException e =
        expectThrows(TimeoutException.class, () -> driver.waitFor(() -> null, "test", null));
    Instant stop = Instant.now();
    assertThat(Duration.between(start, stop).toSecondsPart(), is(lessThanOrEqualTo(1)));
    assertThat(e.getMessage(), containsString("Expected condition failed: test"));
    start = Instant.now();
    e = expectThrows(TimeoutException.class, () -> driver.waitFor(() -> false, null, null));
    stop = Instant.now();
    assertThat(e.getMessage(), containsString("Expected condition failed: wait for condition"));
    assertThat(Duration.between(start, stop).toSecondsPart(), is(lessThanOrEqualTo(1)));
  }

  @Test
  public void testWaitForThrowsNullPointer() {
    Driver driver = new MockUtilities().getDriverAdapter();
    Instant start = Instant.now();
    NullPointerException e =
        expectThrows(
            NullPointerException.class,
            () ->
                driver.waitFor(
                    () -> {
                      throw new NullPointerException("my error");
                    },
                    "test",
                    Duration.ofSeconds(3)));
    Instant stop = Instant.now();
    assertThat(e.getMessage(), containsString("my error"));
    assertThat(Duration.between(start, stop).toSecondsPart(), is(greaterThanOrEqualTo(3)));
  }

  @Test
  public void testWaitForThrowsSeleniumException() {
    Driver driver = new MockUtilities().getDriverAdapter();
    Instant start = Instant.now();
    NoSuchElementException e =
        expectThrows(
            NoSuchElementException.class,
            () ->
                driver.waitFor(
                    () -> driver.findElement(LocatorBy.byCss("css")),
                    "test",
                    Duration.ofSeconds(3)));
    Instant stop = Instant.now();
    assertThat(
        e.getMessage(), containsString("can't find element with locator 'By.cssSelector: css'"));
    assertThat(Duration.between(start, stop).toSecondsPart(), is(greaterThanOrEqualTo(3)));
  }

  @Test
  public void testGetUrl() {
    String url = "url";
    MockUtilities mock = new MockUtilities();
    when(mock.getWebDriverMock().getCurrentUrl()).thenReturn(url);
    assertThat(mock.getDriverAdapter().getUrl(), is(equalTo(url)));
  }

  @Test
  public void testEnterFrame() {
    MockUtilities mock = new MockUtilities();
    Element element = mock.getElementAdapter();
    mock.getDriverAdapter().enterFrame(element);
  }

  @Test
  public void testEnterNullFrameThrows() {
    MockUtilities mock = new MockUtilities();
    Document document = mock.getDocument();
    Exception e = expectThrows(UtamError.class, () -> document.enterFrame(null));
    assertThat(e.getMessage(), is(equalTo(ERR_CANT_ENTER_NULL_FRAME)));
  }

  @Test
  public void testExitToParentFrame() {
    MockUtilities mock = new MockUtilities();
    mock.getDriverAdapter().exitToParentFrame();
  }

  @Test
  public void exitFrame() {
    MockUtilities mock = new MockUtilities();
    mock.getDriverAdapter().exitFrame();
  }

  @Test
  public void testSetPageContextThrows() {
    MockUtilities mock = new MockUtilities();
    DriverAdapter adapter = (DriverAdapter) mock.getDriverAdapter();
    // nothing happens
    adapter.setPageContext(PlatformType.WEB);
  }

  @Test
  public void testBackNavigation() {
    MockUtilities mock = new MockUtilities();
    Navigation navigationMock = mock(Navigation.class);
    WebDriver driver = mock.getWebDriverMock();
    when(driver.navigate()).thenReturn(navigationMock);
    mock.getDriverAdapter().back();
    verify(navigationMock, times(1)).back();
  }

  @Test
  public void testForwardNavigation() {
    MockUtilities mock = new MockUtilities();
    Navigation navigationMock = mock(Navigation.class);
    WebDriver driver = mock.getWebDriverMock();
    when(driver.navigate()).thenReturn(navigationMock);
    mock.getDriverAdapter().forward();
    verify(navigationMock, times(1)).forward();
  }
}
