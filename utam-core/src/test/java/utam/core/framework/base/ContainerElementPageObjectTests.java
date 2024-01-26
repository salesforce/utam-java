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
import static org.mockito.Mockito.when;
import static org.testng.Assert.expectThrows;
import static utam.core.framework.base.ContainerElementPageObject.ERR_UNSUPPORTED_METHOD;

import java.util.Collections;
import org.openqa.selenium.By;
import org.openqa.selenium.WebElement;
import org.testng.annotations.Test;
import utam.core.MockUtilities;
import utam.core.element.Locator;
import utam.core.framework.UtamCoreError;
import utam.core.selenium.element.ElementAdapter;
import utam.core.selenium.element.LocatorBy;

/**
 * @author elizaveta.ivanova
 * @since 234
 */
public class ContainerElementPageObjectTests {

  private static ContainerElementPageObject getMockPageObject() {
    MockUtilities mock = new MockUtilities();
    ContainerElementImpl containerElement =
        new ContainerElementImpl(mock.getFactory(), mock.getElementAdapter(), false);
    return new ContainerElementPageObject(containerElement);
  }

  @Test
  public void testGetContainer() {
    ContainerElementPageObject pageObject = getMockPageObject();
    assertThat(pageObject.getContainerElement(), is(sameInstance(pageObject)));
  }

  @Test
  public void testSupportedCompatibilityMethods() {
    ContainerElementPageObject pageObject = getMockPageObject();
    assertThat(pageObject.isPresent(), is(equalTo(false)));
    pageObject.load();
  }

  @Test
  public void testUnsupportedMethods() {
    ContainerElementPageObject pageObject = getMockPageObject();

    UtamCoreError e =
        expectThrows(UtamCoreError.class, () -> pageObject.containsElement(mock(LocatorBy.class)));
    assertThat(e.getMessage(), is(equalTo(ERR_UNSUPPORTED_METHOD)));

    e =
        expectThrows(
            UtamCoreError.class, () -> pageObject.containsElement(mock(LocatorBy.class), false));
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
    MockUtilities mock = new MockUtilities();
    TestContainersPage page = new TestContainersPage(mock);
    WebElement expectedContent = mock(WebElement.class);
    when(mock.getWebElementMock().findElements(By.cssSelector(TestContainersPage.CONTAINER_CSS)))
        .thenReturn(Collections.singletonList(expectedContent));
    ContainerElementPageObject testMe =
        page.getContainerContent(ContainerElementPageObject.class, false);
    WebElement actualContent =
        ((ElementAdapter) (testMe.getContainerElement()).containerScope).getWebElement();
    // locator: "driver > By.cssSelector: root >> By.cssSelector: :scope > *:first-child"
    assertThat(actualContent, is(equalTo(expectedContent)));

    WebElement expectedContentInjected = mock(WebElement.class);
    when(expectedContent.findElements(By.cssSelector("inject")))
        .thenReturn(Collections.singletonList(expectedContentInjected));
    LoadMe test = testMe.load(LoadMe.class, LocatorBy.byCss("inject"));
    WebElement actualContentInjected = ((ElementAdapter) test.getElement()).getWebElement();
    // "driver > By.cssSelector: root >> By.cssSelector: :scope > *:first-child > By.cssSelector:
    // inject"
    assertThat(actualContentInjected, is(equalTo(expectedContentInjected)));
    assertThat(test.getRootLocation().getStringValue(), is(equalTo("inject")));

    when(expectedContent.findElements(By.cssSelector("inject")))
        .thenReturn(Collections.singletonList(expectedContentInjected));
    test = testMe.loadList(LoadMe.class, LocatorBy.byCss("inject")).get(0);
    actualContentInjected = ((ElementAdapter) test.getElement()).getWebElement();
    assertThat(actualContentInjected, is(equalTo(expectedContentInjected)));
  }

  @Test
  public void testCorrectContainerInsideShadow() {
    MockUtilities mock = new MockUtilities();
    TestContainersPage page = new TestContainersPage(mock);
    WebElement expectedContent = mock.getWebElementMock();
    mock.setShadowMock(expectedContent, TestContainersPage.CONTAINER_CSS);
    ContainerElementPageObject testMe =
        page.getContainerContent(ContainerElementPageObject.class, true);
    WebElement actualContent =
        ((ElementAdapter) (testMe.getContainerElement()).containerScope).getWebElement();
    // locator: "driver > By.cssSelector: root >> By.cssSelector: :scope > *:first-child"
    assertThat(actualContent, is(equalTo(expectedContent)));

    WebElement expectedContentInjected = mock.getWebElementMock();
    mock.setShadowMock(expectedContentInjected, "inject");
    LoadMe test = testMe.load(LoadMe.class, LocatorBy.byCss("inject"));
    WebElement actualContentInjected = ((ElementAdapter) test.getElement()).getWebElement();
    // "driver > By.cssSelector: root >> By.cssSelector: :scope > *:first-child >> By.cssSelector:
    // inject"
    assertThat(actualContentInjected, is(equalTo(expectedContentInjected)));

    test = testMe.loadList(LoadMe.class, LocatorBy.byCss("inject")).get(0);
    actualContentInjected = ((ElementAdapter) test.getElement()).getWebElement();
    assertThat(actualContentInjected, is(equalTo(expectedContentInjected)));
  }

  public static class LoadMe extends BasePageObject {

    Locator getRootLocation() {
      return getRootLocator();
    }
  }

  static class TestContainersPage extends BasePageObject {

    static final String CONTAINER_CSS = ":scope > *:first-child";

    TestContainersPage(MockUtilities mockUtilities) {
      mockUtilities
          .getFactory()
          .bootstrap(this, mockUtilities.getElementAdapter(), LocatorBy.byCss("root"));
    }

    final <T extends PageObject> T getContainerContent(
        Class<T> pageObjectType, boolean isExpandShadowRoot) {
      return this.container(this.getRootElement(), isExpandShadowRoot)
          .load(pageObjectType, LocatorBy.byCss(CONTAINER_CSS));
    }
  }
}
