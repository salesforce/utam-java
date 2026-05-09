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

public class WrappedElementAdapterTests {

  private WrappedDriverAdapter mockDriverAdapter;
  private WrappedElementDecorator mockElement;
  private WebElement mockWebElement;

  @BeforeMethod
  public void setup() {
    MockUtilities mockUtilities = new MockUtilities();
    DriverConfig driverConfig = mockUtilities.getDriverAdapter().getDriverConfig();
    WrappedDriverDecorator wrappedDriver = mock(WrappedDriverDecorator.class);
    WebDriver webDriver = mock(WebDriver.class);
    mockWebElement = mock(WebElement.class);

    when(wrappedDriver.unwrap()).thenReturn(webDriver);

    mockDriverAdapter = new WrappedDriverAdapter(wrappedDriver, driverConfig);
    mockElement = mock(WrappedElementDecorator.class);
    when(mockElement.unwrap()).thenReturn(mockWebElement);
  }

  @Test
  public void testConstructor() {
    WrappedElementAdapter adapter = new WrappedElementAdapter(mockElement, mockDriverAdapter);
    assertThat(adapter, is(notNullValue()));
  }

  @Test
  public void testUnwrap() {
    WrappedElementAdapter adapter = new WrappedElementAdapter(mockElement, mockDriverAdapter);
    WebElement unwrapped = adapter.unwrap();
    assertThat(unwrapped, is(mockWebElement));
  }

  @Test
  public void testWrapElement() {
    WrappedElementAdapter adapter = new WrappedElementAdapter(mockElement, mockDriverAdapter);
    WrappedElementDecorator childElement = mock(WrappedElementDecorator.class);

    Element wrapped = adapter.wrapElement(childElement);
    assertThat(wrapped, is(instanceOf(WrappedElementAdapter.class)));
  }
}
