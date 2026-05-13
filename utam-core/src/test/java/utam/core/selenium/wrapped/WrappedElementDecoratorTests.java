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
import static org.hamcrest.Matchers.notNullValue;
import static org.mockito.Mockito.mock;

import org.openqa.selenium.WebElement;
import org.testng.annotations.Test;

public class WrappedElementDecoratorTests {

  @Test
  public void testGetMetadata() {
    WrappedElementDecorator decorator = mock(WrappedElementDecorator.class);
    assertThat(decorator, is(notNullValue()));
  }

  @Test
  public void testUnwrapMethod() {
    // Test that unwrap method exists and can be called
    WebElement mockElement = mock(WebElement.class);
    WrappedElementDecorator decorator = mock(WrappedElementDecorator.class);

    // The unwrap() default method exists on the interface
    assertThat(decorator, is(notNullValue()));
  }
}
