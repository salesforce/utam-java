/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.base;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static org.hamcrest.Matchers.sameInstance;
import static org.mockito.Mockito.when;
import static utam.core.element.FindContext.Type.EXISTING;
import static utam.core.element.FindContext.Type.NULLABLE;
import static utam.core.framework.base.BasicElementBuilder.getUnwrappedElement;
import static utam.core.framework.element.BasePageElement.createInstance;

import java.util.Collections;
import org.openqa.selenium.By;
import org.openqa.selenium.WebElement;
import org.testng.annotations.Test;
import utam.core.MockUtilities;
import utam.core.driver.Driver;
import utam.core.element.BasicElement;
import utam.core.element.Element;
import utam.core.element.FrameElement;
import utam.core.framework.element.BasePageElement;
import utam.core.selenium.element.ElementAdapter;
import utam.core.selenium.element.LocatorBy;

public class FrameElementTests {

  private static FrameElement buildFrameElement(MockUtilities mock, ElementLocation location) {
    Driver driver = mock.getFactory().getDriver();
    BasePageElement element = createInstance(mock.getElementAdapter(), driver);
    return new BasicElementBuilder(mock.getFactory(), element, location).buildFrame();
  }

  private static WebElement getUnwrappedWebElement(FrameElement frame) {
    ElementAdapter adapter = (ElementAdapter) getUnwrappedElement(frame);
    return adapter.getWebElement();
  }

  @Test
  public void testGetElement() {
    MockUtilities mock = new MockUtilities();
    Element mockElement = mock.getElementAdapter();
    FrameElement frame = mock.getFrameElement();
    assertThat(getUnwrappedElement(frame), is(sameInstance(mockElement)));
  }

  @Test
  public void testBuildFrameForExistingElement() {
    MockUtilities mock = new MockUtilities();
    WebElement foundElementMock = mock.getWebElementMock();
    when(mock.getWebElementMock().findElements(By.cssSelector("css")))
        .thenReturn(Collections.singletonList(foundElementMock));
    ElementLocation location = new ElementLocation(LocatorBy.byCss("css"), NULLABLE);
    FrameElement frame = buildFrameElement(mock, location);
    assertThat(getUnwrappedWebElement(frame), is(sameInstance(foundElementMock)));
  }

  @Test
  public void testBuildNullableFrameForNonExistingElement() {
    MockUtilities mock = new MockUtilities();
    ElementLocation location = new ElementLocation(LocatorBy.byCss("css"), NULLABLE);
    BasicElement test = buildFrameElement(mock, location);
    assertThat(test, is(nullValue()));
  }

  @Test
  public void testBuildFrameWithParametrizedLocator() {
    MockUtilities mock = new MockUtilities();
    WebElement foundElementMock = mock.getWebElementMock();
    when(mock.getWebElementMock().findElements(By.cssSelector("css[string][1]")))
        .thenReturn(Collections.singletonList(foundElementMock));
    ElementLocation location = new ElementLocation(LocatorBy.byCss("css[%s][%d]"), EXISTING);
    FrameElement frame = buildFrameElement(mock, location.setParameters("string", 1));
    assertThat(getUnwrappedWebElement(frame), is(sameInstance(foundElementMock)));
  }
}
