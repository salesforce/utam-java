/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.base;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static utam.core.selenium.factory.WebDriverFactory.getAdapterMock;

import org.openqa.selenium.WebDriver;
import org.testng.annotations.Test;
import utam.core.driver.Driver;
import utam.core.element.Element;
import utam.core.element.Locator;
import utam.core.framework.consumer.PageObjectContext;
import utam.core.selenium.element.LocatorBy;

/**
 * @author elizaveta.ivanova
 * @since 234
 */
public class UtamBaseTests {

  @Test
  public void testIsPresent() {
    UtamBase utamBase = new UtamBaseImplTest();
    assertThat(utamBase.isPresent(), is(equalTo(false)));
  }

  @Test
  public void testIsVisible() {
    UtamBase utamBase = new UtamBaseImplTest();
    assertThat(utamBase.isVisible(), is(equalTo(false)));
  }

  @Test
  public void testContainsElement() {
    UtamBase utamBase = new UtamBaseImplTest();
    Locator locator = LocatorBy.byCss("css");
    assertThat(utamBase.containsElement(locator), is(equalTo(false)));
    assertThat(utamBase.containsElement(locator, false), is(equalTo(false)));
  }

  @Test
  public void testWaitFor() {
    UtamBase utamBase = new UtamBaseImplTest();
    assertThat(utamBase.waitFor(() -> true), is(equalTo(true)));
  }

  @Test
  public void testWaitForAbsence() {
    UtamBase utamBase = new UtamBaseImplTest();
    utamBase.waitForAbsence();
  }

  @Test
  public void testWaitForPresence() {
    UtamBase utamBase = new UtamBaseImplTest();
    utamBase.waitForAbsence();
  }

  @Test
  public void testWaitForVisible() {
    UtamBaseImplTest utamBase = new UtamBaseImplTest();
    when(utamBase.getElement().isDisplayed()).thenReturn(true);
    utamBase.waitForVisible();
  }

  @Test
  public void testWaitForInvisible() {
    UtamBaseImplTest utamBase = new UtamBaseImplTest();
    when(utamBase.getElement().isDisplayed()).thenReturn(false);
    utamBase.waitForInvisible();
  }

  static class UtamBaseImplTest extends UtamBaseImpl {

    private final Element element = mock(Element.class);
    private final PageObjectsFactory factory =
        new PageObjectsFactoryImpl(
            mock(PageObjectContext.class), getAdapterMock(mock(WebDriver.class)));

    @Override
    protected Element getElement() {
      return element;
    }

    @Override
    protected Driver getDriver() {
      return factory.getDriver();
    }
  }
}
