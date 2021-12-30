/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.base;

import org.testng.annotations.Test;
import utam.core.MockUtilities;
import utam.core.element.Element;
import utam.core.element.FrameElement;
import utam.core.selenium.element.ElementAdapter;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static org.hamcrest.Matchers.sameInstance;
import static utam.core.framework.base.BasicElementBuilder.getUnwrappedElement;
import static utam.core.framework.element.BasePageElement.createInstance;
import static utam.core.selenium.element.ElementAdapter.getNullElement;

public class FrameElementTests {

  @Test
  public void testGetElement() {
    MockUtilities mock = new MockUtilities();
    Element mockElement = mock.getElementAdapter();
    FrameElement frame = mock.getFrameElement();
    assertThat(getUnwrappedElement(frame), is(sameInstance(mockElement)));
  }

  @Test
  public void testGetElementNull() {
    MockUtilities mock = new MockUtilities();
    ElementAdapter nullElement = getNullElement(mock.getDriverAdapter());
    FrameElement element = createInstance(FrameElementImpl.class, nullElement, mock.getDriverAdapter());
    assertThat(element, is(nullValue()));
  }
}
