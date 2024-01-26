/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.base;

import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.Mockito.when;
import static org.testng.Assert.assertThrows;
import static utam.core.element.FindContext.Type.EXISTING;

import java.util.function.Supplier;
import org.testng.annotations.Test;
import utam.core.MockUtilities;
import utam.core.driver.Document;
import utam.core.driver.Navigation;
import utam.core.framework.element.BasePageElement;
import utam.core.selenium.element.LocatorBy;

/**
 * Tests the default base PageObject class
 *
 * @author james.evans
 */
public class PageObjectTests {

  @Test
  public void testWaitFor() {
    TestPageImpl testPage = new TestPageImpl();
    MockUtilities mockUtilities = testPage.mock;
    Supplier<Object> apply =
        () -> {
          mockUtilities.getElementAdapter().setText("text");
          return mockUtilities.getElementAdapter();
        };

    BasePageElement element = mockUtilities.getUtamElement();
    assertThat(testPage.waitFor(apply), is(notNullValue()));

    // successfully waits for visibility
    when(mockUtilities.getElementAdapter().isDisplayed()).thenReturn(true);
    assertThat(testPage.waitFor(element::isVisible), is(true));

    // throws exception when visibility times out
    when(mockUtilities.getElementAdapter().isDisplayed()).thenReturn(false);
    assertThrows(() -> testPage.waitFor(() -> mockUtilities.getUtamElement().isVisible()));
  }

  @Test
  public void testGetDocument() {
    TestPageImpl testPage = new TestPageImpl();
    assertThat(testPage.getDocument(), is(instanceOf(Document.class)));
  }

  @Test
  public void testGetNavigation() {
    TestPageImpl testPage = new TestPageImpl();
    assertThat(testPage.getNavigation(), is(instanceOf(Navigation.class)));
  }

  @Test
  public void testInScopeMethod() {
    TestPageImpl testPage = new TestPageImpl();
    assertThat(
        testPage.custom(testPage.getRootElement(), testPage.element),
        is(instanceOf(CustomElementBuilder.class)));
  }

  @Test
  public void testElementMethod() {
    TestPageImpl testPage = new TestPageImpl();
    testPage.basic(testPage.getRootElement(), testPage.element);
  }

  @Test
  public void testInContainerMethod() {
    TestPageImpl testPage = new TestPageImpl();
    assertThat(
        testPage.container(testPage.getRootElement(), false),
        is(instanceOf(ContainerElementImpl.class)));
  }

  static class TestPageImpl extends BasePageObject {

    final MockUtilities mock = new MockUtilities();

    final ElementLocation element = new ElementLocation(LocatorBy.byCss("css"), EXISTING);

    TestPageImpl() {
      initialize(
          mock.getFactory(),
          mock.getElementAdapter(),
          LocatorBy.byCss("root"),
          mock.getDocument(),
          mock.getNavigation());
    }
  }
}
