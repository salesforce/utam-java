/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.base;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static utam.core.framework.base.ElementMarker.getLocator;

import org.openqa.selenium.By;
import org.testng.annotations.Test;
import utam.core.element.FindContext.Type;
import utam.core.selenium.element.LocatorBy;

/**
 * @author elizaveta.ivanova
 * @since 234
 */
public class ElementMarkerTests {

  @Test
  public void testGetFinderContext() {
    ElementMarker.Find annotation = mock(ElementMarker.Find.class);
    when(annotation.expand()).thenReturn(true);
    when(annotation.nullable()).thenReturn(true);
    assertThat(ElementMarker.getFinderContext(annotation), is(equalTo(Type.NULLABLE_IN_SHADOW)));
    when(annotation.expand()).thenReturn(false);
    when(annotation.nullable()).thenReturn(true);
    assertThat(ElementMarker.getFinderContext(annotation), is(equalTo(Type.NULLABLE)));
    when(annotation.expand()).thenReturn(false);
    when(annotation.nullable()).thenReturn(false);
    assertThat(ElementMarker.getFinderContext(annotation), is(equalTo(Type.EXISTING)));
    when(annotation.expand()).thenReturn(true);
    when(annotation.nullable()).thenReturn(false);
    assertThat(ElementMarker.getFinderContext(annotation), is(equalTo(Type.EXISTING_IN_SHADOW)));
  }

  @Test
  public void testGetLocator() {
    ElementMarker.Find annotation = mock(ElementMarker.Find.class);
    when(annotation.accessid()).thenReturn("test");
    assertThat(getLocator(annotation), is(equalTo(LocatorBy.byAccessibilityId("test"))));

    when(annotation.accessid()).thenReturn("");
    when(annotation.classchain()).thenReturn("test");
    assertThat(getLocator(annotation), is(equalTo(LocatorBy.byClassChain("test"))));

    when(annotation.classchain()).thenReturn("");
    when(annotation.uiautomator()).thenReturn("checkable()");
    assertThat(getLocator(annotation), is(equalTo(LocatorBy.byUiAutomator("checkable()"))));
  }

  @Test
  public void testGetElementSelectorFromAnnotationNull() {
    ElementMarker.Find annotation = mock(ElementMarker.Find.class);
    when(annotation.accessid()).thenReturn("");
    when(annotation.classchain()).thenReturn("");
    when(annotation.uiautomator()).thenReturn("");
    when(annotation.css()).thenReturn("css");
    assertThat(getLocator(annotation).getValue(), is(equalTo(By.cssSelector(annotation.css()))));
  }
}
