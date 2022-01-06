/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.base;

import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static org.hamcrest.Matchers.sameInstance;
import static org.mockito.Mockito.when;
import static utam.core.element.FindContext.Type.EXISTING;
import static utam.core.element.FindContext.Type.NULLABLE;
import static utam.core.framework.base.BasicElementBuilder.getUnwrappedElement;
import static utam.core.framework.base.BasicElementBuilderTests.getBasicBuilder;

import org.openqa.selenium.By;
import org.testng.annotations.Test;
import utam.core.MockUtilities;
import utam.core.element.BasicElement;
import utam.core.element.Element;
import utam.core.element.FrameElement;
import utam.core.selenium.element.LocatorBy;

public class FrameElementTests {

  @Test
  public void testGetElement() {
    MockUtilities mock = new MockUtilities();
    Element mockElement = mock.getElementAdapter();
    FrameElement frame = mock.getFrameElement();
    assertThat(getUnwrappedElement(frame), is(sameInstance(mockElement)));
  }

  @Test
  public void testBuildFrame() {
    MockUtilities mock = new MockUtilities();
    when(mock.getWebElementMock().findElement(By.cssSelector("css")))
        .thenReturn(mock.getWebElementMock());
    ElementLocation location = new ElementLocation(LocatorBy.byCss("css"), NULLABLE);
    BasicElement test = getBasicBuilder(mock, location).buildFrame();
    assertThat(test, is(nullValue()));
  }

  @Test
  public void testBuildFrameWithParametrizedLocator() {
    MockUtilities mock = new MockUtilities();
    when(mock.getWebElementMock().findElement(By.cssSelector("css[string][1]")))
        .thenReturn(mock.getWebElementMock());
    ElementLocation location = new ElementLocation(LocatorBy.byCss("css[%s][%d]"), EXISTING);
    FrameElement test = getBasicBuilder(mock, location.setParameters("string", 1)).buildFrame();
    assertThat(test, is(notNullValue()));
  }
}
