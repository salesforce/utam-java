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
import static org.hamcrest.Matchers.nullValue;
import static org.mockito.Mockito.mock;
import static org.testng.Assert.expectThrows;
import static utam.core.selenium.element.ElementAdapter.NULL_ELEMENT;

import org.testng.annotations.Test;
import utam.core.driver.Driver;
import utam.core.driver.Expectations;
import utam.core.element.Element;

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
}
