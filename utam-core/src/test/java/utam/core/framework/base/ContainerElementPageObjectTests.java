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
import static org.hamcrest.Matchers.equalTo;
import static org.mockito.Mockito.mock;
import static org.testng.Assert.expectThrows;
import static utam.core.framework.base.ContainerElementPageObject.ERR_UNSUPPORTED_METHOD;

import org.testng.annotations.Test;
import utam.core.framework.UtamCoreError;
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
  public void testSupportedCompatibilityMethods() {
    ContainerElement containerElement = mock(ContainerElement.class);
    ContainerElementPageObject pageObject = new ContainerElementPageObject(containerElement);
    assertThat(pageObject.isPresent(), is(equalTo(false)));
    pageObject.load();
  }

  @Test
  public void testUnsupportedMethods() {
    ContainerElementPageObject pageObject = new ContainerElementPageObject(
        mock(ContainerElement.class));

    UtamCoreError e = expectThrows(UtamCoreError.class, () -> pageObject.containsElement(mock(LocatorBy.class)));
    assertThat(e.getMessage(), is(equalTo(ERR_UNSUPPORTED_METHOD)));

    e = expectThrows(UtamCoreError.class,
        () -> pageObject.containsElement(mock(LocatorBy.class), false));
    assertThat(e.getMessage(), is(equalTo(ERR_UNSUPPORTED_METHOD)));

    e = expectThrows(UtamCoreError.class, pageObject::isVisible);
    assertThat(e.getMessage(), is(equalTo(ERR_UNSUPPORTED_METHOD)));

    e = expectThrows(UtamCoreError.class, pageObject::waitForAbsence);
    assertThat(e.getMessage(), is(equalTo(ERR_UNSUPPORTED_METHOD)));

    e = expectThrows(UtamCoreError.class, pageObject::waitForInvisible);
    assertThat(e.getMessage(), is(equalTo(ERR_UNSUPPORTED_METHOD)));

    e = expectThrows(UtamCoreError.class, pageObject::waitForVisible);
    assertThat(e.getMessage(), is(equalTo(ERR_UNSUPPORTED_METHOD)));

    e = expectThrows(UtamCoreError.class, () -> pageObject.waitFor(() -> true));
    assertThat(e.getMessage(), is(equalTo(ERR_UNSUPPORTED_METHOD)));
  }
}
