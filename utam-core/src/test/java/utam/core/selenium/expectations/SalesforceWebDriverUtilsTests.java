/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.selenium.expectations;

import static org.mockito.ArgumentMatchers.contains;
import static org.mockito.Mockito.*;
import static org.testng.Assert.expectThrows;
import static utam.core.selenium.expectations.SalesforceWebDriverUtils.SCROLL_INTO_VIEW_ERR;
import static utam.core.selenium.expectations.SalesforceWebDriverUtils.SCROLL_INTO_VIEW_MSG;
import static org.hamcrest.Matchers.*;
import static org.hamcrest.MatcherAssert.assertThat;
import org.openqa.selenium.ElementNotVisibleException;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.SearchContext;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.testng.annotations.Test;
import utam.core.selenium.context.SeleniumContextProvider;
import utam.core.selenium.context.WebDriverUtilities;
import utam.core.selenium.expectations.ElementExpectations;
import utam.core.selenium.expectations.SalesforceWebDriverUtils;

public class SalesforceWebDriverUtilsTests {

  private static WebDriver getDriverMock() {
    return mock(
        WebDriver.class,
        withSettings().extraInterfaces(
            JavascriptExecutor.class,
            SearchContext.class));
  }
  
  @Test
  public void testScrollIntoViewWithElementAlreadyInView() {
    MockHelper mock = new MockHelper();
    ElementExpectations<SearchContext> expectations =
        SalesforceWebDriverUtils.scrollIntoView();
    when(mock.element.isDisplayed()).thenReturn(true);
    expectations.apply(mock.utilities).apply(mock.element);
  }
  
  @Test
  public void testScrollIntoViewWithElementAlignedToBottom() {
    MockHelper mock = new MockHelper();
    when(((JavascriptExecutor) mock.driver).executeScript(
        contains(SalesforceWebDriverUtils.SCROLL_INTO_VIEW_JS),
        refEq(mock.element))).then((invocation) -> when(
            ((WebElement)invocation.getArgument(1)).isDisplayed()).thenReturn(true));    
    ElementExpectations<SearchContext> expectations = 
        SalesforceWebDriverUtils.scrollIntoView();
    expectations.apply(mock.utilities).apply(mock.element);
  }
  
  @Test
  public void testScrollIntoViewWithElementAlignedToTop() {
    MockHelper mock = new MockHelper();
    when(((JavascriptExecutor) mock.driver).executeScript(
        contains(SalesforceWebDriverUtils.SCROLL_INTO_VIEW_ALIGN_TO_TOP_JS),
        refEq(mock.element))).then((invocation) -> when(
            ((WebElement)invocation.getArgument(1)).isDisplayed()).thenReturn(true));    
    ElementExpectations<SearchContext> expectations =
        SalesforceWebDriverUtils.scrollIntoView();
    expectations.apply(mock.utilities).apply(mock.element);
  }
  
  @Test
  public void testScrollIntoViewIfElementNotVisibleThrows() {
    MockHelper mock = new MockHelper();
    ElementExpectations<SearchContext> expectations = 
        SalesforceWebDriverUtils.scrollIntoView();
    assertThat(expectations.getLogMessage(), is(equalTo(SCROLL_INTO_VIEW_MSG)));
    assertThat(expectations.returnIfNothingFound(), is(nullValue()));
    ElementNotVisibleException e =
            expectThrows(
                    ElementNotVisibleException.class,
                    () -> expectations.apply(mock.utilities).apply(mock.element));
    assertThat(
            "error cause element is not visible",
            e.getMessage(),
            containsString(SCROLL_INTO_VIEW_ERR));
  }

  static class MockHelper {

    private final WebDriver driver = getDriverMock();
    final WebDriverUtilities utilities = new SeleniumContextProvider(driver).getWebDriverUtils();
    final WebElement element = mock(WebElement.class);
  }
}
