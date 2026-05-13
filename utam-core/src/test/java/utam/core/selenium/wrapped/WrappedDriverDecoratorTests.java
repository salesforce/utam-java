/*
 * Copyright (c) 2026, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.selenium.wrapped;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.openqa.selenium.WebDriver;
import org.testng.annotations.Test;

public class WrappedDriverDecoratorTests {

  @Test
  public void testUnwrap() {
    WebDriver mockWebDriver = mock(WebDriver.class);
    WrappedDriverDecorator decorator = mock(WrappedDriverDecorator.class);

    when(decorator.unwrap()).thenReturn(mockWebDriver);

    WebDriver result = decorator.unwrap();
    assertThat(result, is(mockWebDriver));
  }
}
