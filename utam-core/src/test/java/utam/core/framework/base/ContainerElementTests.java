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
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.core.IsEqual.equalTo;
import static org.mockito.Mockito.when;
import static org.testng.Assert.expectThrows;
import static utam.core.framework.base.ContainerElementImpl.NULL_SCOPE_ERR;

import java.util.Collections;
import java.util.List;
import java.util.function.Supplier;
import org.openqa.selenium.By;
import org.openqa.selenium.NoSuchElementException;
import org.openqa.selenium.SearchContext;
import org.openqa.selenium.WebElement;
import org.testng.annotations.Test;
import utam.core.MockUtilities;
import utam.core.element.Element;
import utam.core.element.Locator;
import utam.core.framework.consumer.Contained;
import utam.core.framework.consumer.ContainerElement;
import utam.core.selenium.element.LocatorBy;

/**
 * container element tests
 *
 * @author elizaveta.ivanova
 * @since 228
 */
public class ContainerElementTests {

  @Test
  public void testWithNonExistingElement() {
    MockUtilities mock = new MockUtilities();
    Element elementMock = mock.getElementAdapter();
    Locator locator = LocatorBy.byCss("css");
    // nothing found, throw if not nullable
    expectThrows(
        NoSuchElementException.class,
        () ->
            new ContainerElementImpl(mock.getFactory(), elementMock, false)
                .load(TestLoad.class, locator));
    expectThrows(
        NoSuchElementException.class,
        () ->
            new ContainerElementImpl(mock.getFactory(), elementMock, true)
                .loadList(TestLoad.class, locator));
  }

  @Test
  public void testCreateWithElementFound() {
    MockUtilities mock = new MockUtilities();
    Element elementMock = mock.getElementAdapter();
    ContainerElement element = new ContainerElementImpl(mock.getFactory(), elementMock, false);
    when(mock.getWebElementMock().findElements(By.cssSelector("css")))
        .thenReturn(Collections.singletonList(mock.getWebElementMock()));
    TestLoad testLoad = element.load(TestLoad.class, "css");
    assertThat(testLoad.getRootLocator().getStringValue(), is(equalTo("css")));
    testLoad = element.load(TestLoad.class, LocatorBy.byCss("css"));
    assertThat(testLoad.getRootLocator().getStringValue(), is(equalTo("css")));

    when(mock.getWebElementMock().findElements(By.cssSelector("css")))
        .thenReturn(Collections.singletonList(mock.getWebElementMock()));
    List<TestLoad> list = element.loadList(TestLoad.class, LocatorBy.byCss("css"));
    assertThat(list.get(0).getRootLocator().getStringValue(), is(equalTo("css")));
  }

  @Test
  public void testCreateWithElementFoundInShadow() {
    MockUtilities mock = new MockUtilities();
    Element elementMock = mock.getElementAdapter();
    ContainerElement element = new ContainerElementImpl(mock.getFactory(), elementMock, true);
    mock.setShadowMock(mock.getWebElementMock(), "css");
    TestLoad testLoad = element.load(TestLoad.class, LocatorBy.byCss("css"));
    // driver > element >> By.cssSelector: css
    assertThat(testLoad.getRootLocator().getStringValue(), is(equalTo("css")));
  }

  @Test
  public void testSetScope() {
    MockUtilities mock = new MockUtilities();
    Element elementMock = mock.getElementAdapter();
    ContainerElement element = new ContainerElementImpl(mock.getFactory(), elementMock, false);
    CompatiblePageObject compatiblePageObjectInsideContainer = new CompatiblePageObject();
    element.setScope(compatiblePageObjectInsideContainer);
    assertThat(
        compatiblePageObjectInsideContainer.getRoot(), is(sameInstance(mock.getWebElementMock())));
  }

  @Test
  public void testNullScopeThrows() {
    MockUtilities mock = new MockUtilities();
    NoSuchElementException e =
        expectThrows(
            NoSuchElementException.class,
            () -> new ContainerElementImpl(mock.getFactory(), (Element) null, false));
    assertThat(e.getMessage(), containsString(NULL_SCOPE_ERR));
  }

  static final class TestLoad extends BasePageObject {

    // should be public for java reflection to work
    public TestLoad() {}
  }

  static final class CompatiblePageObject implements Contained {

    private Supplier<SearchContext> scopeSupplier;

    WebElement getRoot() {
      return (WebElement) scopeSupplier.get();
    }

    @Override
    public void setScope(Supplier<SearchContext> scopeSupplier) {
      this.scopeSupplier = scopeSupplier;
    }
  }
}
