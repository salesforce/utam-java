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

import org.testng.annotations.Test;
import utam.core.selenium.element.LocatorBy;

/**
 * @author elizaveta.ivanova
 * @since 234
 */
public class PageMarkerTests {

  @Test
  public void testGetRootLocator() {
    PageMarker.Find annotation = mock(PageMarker.Find.class);
    when(annotation.accessid()).thenReturn("test");
    assertThat(
        PageMarker.getRootLocatorFromAnnotation(annotation),
        is(equalTo(LocatorBy.byAccessibilityId("test"))));

    when(annotation.accessid()).thenReturn("");
    when(annotation.classchain()).thenReturn("test");
    assertThat(
        PageMarker.getRootLocatorFromAnnotation(annotation),
        is(equalTo(LocatorBy.byClassChain("test"))));

    when(annotation.classchain()).thenReturn("");
    when(annotation.uiautomator()).thenReturn("enabled(true)");
    assertThat(
        PageMarker.getRootLocatorFromAnnotation(annotation),
        is(equalTo(LocatorBy.byUiAutomator("enabled(true)"))));
  }
}
