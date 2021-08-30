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
import static utam.core.element.FindContext.Type.EXISTING;
import static utam.core.framework.base.ContainerElementPageObject.ERR_UNSUPPORTED_METHOD;

import org.testng.annotations.Test;
import utam.core.MockUtilities.MockDriver;
import utam.core.element.ElementLocation;
import utam.core.framework.UtamCoreError;
import utam.core.framework.consumer.ContainerElement;
import utam.core.framework.element.ElementLocationChain;
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

    UtamCoreError e = expectThrows(UtamCoreError.class,
        () -> pageObject.containsElement(mock(LocatorBy.class)));
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

  @Test
  public void testCorrectContainer() {
    PageObjectsFactory factory = new MockDriver().getFactory();
    TestContainersPage page = new TestContainersPage(factory);
    ContainerElementPageObject testMe = page
        .getContainerContent(ContainerElementPageObject.class, false);
    assertThat(testMe.getRootLocationChain().getLocatorChainString(),
        is(equalTo("driver > By.cssSelector: root > By.cssSelector: :scope > *:first-child")));

    LoadMe test = testMe.test(LoadMe.class, "inject");
    assertThat(test.getRootLocation().getLocatorChainString(), is(equalTo(
        "driver > By.cssSelector: root > By.cssSelector: :scope > *:first-child > By.cssSelector: inject")));
  }

  @Test
  public void testCorrectContainerInsideShadow() {
    PageObjectsFactory factory = new MockDriver().getFactory();
    TestContainersPage page = new TestContainersPage(factory);
    ContainerElementPageObject testMe = page
        .getContainerContent(ContainerElementPageObject.class, true);
    assertThat(testMe.getRootLocationChain().getLocatorChainString(),
        is(equalTo("driver > By.cssSelector: root >> By.cssSelector: :scope > *:first-child")));

    LoadMe test = testMe.test(LoadMe.class, "inject");
    assertThat(test.getRootLocation().getLocatorChainString(), is(equalTo(
        "driver > By.cssSelector: root >> By.cssSelector: :scope > *:first-child >> By.cssSelector: inject")));
  }

  public static class LoadMe extends BasePageObject {

    ElementLocation getRootLocation() {
      return super.root;
    }
  }

  static class TestContainersPage extends BasePageObject {

    TestContainersPage(PageObjectsFactory factory) {
      factory.bootstrap(this, new ElementLocationChain(LocatorBy.byCss("root"), EXISTING));
    }

    final <T extends PageObject> T getContainerContent(Class<T> pageObjectType,
        boolean isExpandRoot) {
      return this.inContainer(this.root, isExpandRoot)
          .load(pageObjectType, LocatorBy.byCss(":scope > *:first-child"));
    }
  }
}
