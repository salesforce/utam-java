/*
 * Copyright (c) 2026, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.selenium.wrapped;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.instanceOf;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;
import utam.core.MockUtilities;
import utam.core.driver.DriverConfig;
import utam.core.element.Element;

public class WrappedDriverAdapterTests {

  private WrappedDriverDecorator mockWrappedDriver;
  private WebDriver mockWebDriver;
  private DriverConfig driverConfig;
  private WebElement mockElement;

  @BeforeMethod
  public void setup() {
    mockWebDriver = mock(WebDriver.class);
    mockWrappedDriver = mock(WrappedDriverDecorator.class);
    mockElement = mock(WebElement.class);
    MockUtilities mockUtilities = new MockUtilities();
    driverConfig = mockUtilities.getDriverAdapter().getDriverConfig();
    when(mockWrappedDriver.unwrap()).thenReturn(mockWebDriver);
  }

  @Test
  public void testConstructor() {
    WrappedDriverAdapter adapter = new WrappedDriverAdapter(mockWrappedDriver, driverConfig);
    assertThat(adapter, is(notNullValue()));
  }

  @Test
  public void testUnwrap() {
    WrappedDriverAdapter adapter = new WrappedDriverAdapter(mockWrappedDriver, driverConfig);
    WebDriver unwrapped = adapter.unwrap();
    assertThat(unwrapped, is(mockWebDriver));
  }

  @Test
  public void testWrapElement() {
    WrappedDriverAdapter adapter = new WrappedDriverAdapter(mockWrappedDriver, driverConfig);
    Element element = adapter.wrapElement(mockElement);
    assertThat(element, is(notNullValue()));
  }

  @Test
  public void testWrapElementWithDecorator() {
    WrappedElementDecorator decoratedElement = mock(WrappedElementDecorator.class);

    WrappedDriverAdapter adapter = new WrappedDriverAdapter(mockWrappedDriver, driverConfig);
    Element element = adapter.wrapElement(decoratedElement);
    assertThat(element, is(instanceOf(WrappedElementAdapter.class)));
  }
}
