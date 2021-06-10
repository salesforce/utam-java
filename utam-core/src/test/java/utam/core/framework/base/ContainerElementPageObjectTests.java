/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.base;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.sameInstance;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.Mockito.mock;
import static org.testng.Assert.assertThrows;

import org.testng.annotations.Test;
import utam.core.framework.consumer.ContainerElement;
import utam.core.selenium.element.LocatorBy;

/**
 * @author elizaveta.ivanova
 * @since 234
 */
public class ContainerElementPageObjectTests {

  @Test
  public void testGetContainer() {
    ContainerElement containerElement = mock(ContainerElement.class);
    ContainerElementPageObject pageObject = new ContainerElementPageObject(containerElement);
    assertThat(pageObject.getContainerElement(), is(sameInstance(containerElement)));
  }

  @Test
  public void testUnsupportedMethods() {
    ContainerElementPageObject pageObject = new ContainerElementPageObject(mock(ContainerElement.class));
    assertThrows(pageObject::load);
    assertThrows(()-> pageObject.containsElement(mock(LocatorBy.class)));
    assertThrows(()-> pageObject.containsElement(mock(LocatorBy.class), false));
    assertThrows(pageObject::isPresent);
    assertThrows(pageObject::isVisible);
    assertThrows(pageObject::waitForAbsence);
    assertThrows(pageObject::waitForInvisible);
    assertThrows(pageObject::waitForVisible);
    assertThrows(()-> pageObject.waitFor(() -> true));
  }
}
