/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.base;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.empty;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.nullValue;
import static org.mockito.Mockito.when;
import static org.testng.Assert.assertThrows;
import static utam.core.element.FindContext.Type.EXISTING;
import static utam.core.element.FindContext.Type.NULLABLE;

import java.util.Collections;
import java.util.List;
import org.hamcrest.Matchers;
import org.openqa.selenium.By;
import org.testng.annotations.Test;
import utam.core.MockUtilities;
import utam.core.element.Actionable;
import utam.core.element.ElementLocation;
import utam.core.framework.element.ElementLocationChain;
import utam.core.selenium.element.LocatorBy;

/**
 * element builder tests
 *
 * @author elizaveta.ivanova
 * @since 232
 */
public class ElementBuilderTests {

  @Test
  public void testBuild() {
    MockUtilities mock = new MockUtilities();
    // with WebElement
    new ElementBuilder(mock.getFactory(), new ElementLocationChain(mock.getElementAdapter()))
        .build(Actionable.class);
    // with locator, expected exist
    assertThrows(() -> new ElementBuilder(mock.getFactory(),
        new ElementLocationChain(LocatorBy.byCss("css1"), EXISTING))
        .build(Actionable.class));
    when(mock.getWebDriverMock().findElement(By.cssSelector("css1"))).thenReturn(mock.getWebElementMock());
    new ElementBuilder(mock.getFactory(),
        new ElementLocationChain(LocatorBy.byCss("css1"), EXISTING))
        .build(Actionable.class);
    // with locator, expected does not exist
    new ElementBuilder(mock.getFactory(),
        new ElementLocationChain(LocatorBy.byCss("css2"), NULLABLE))
        .build(Actionable.class);
    // with parametrizes locator
    when(mock.getWebDriverMock().findElement(By.cssSelector("css[string]")))
        .thenReturn(mock.getWebElementMock());
    ElementLocation location = new ElementLocationChain(LocatorBy.byCss("css[%s]"), EXISTING);
    new ElementBuilder(mock.getFactory(), location).build(Actionable.class, "string");
  }

  @Test
  public void testBuildWithFilter() {
    // todo
  }

  @Test
  void testBuildList() {
    MockUtilities mock = new MockUtilities();
    // with WebElement
    List<Actionable> list = new ElementBuilder(mock.getFactory(),
        new ElementLocationChain(mock.getElementAdapter()))
        .buildList(Actionable.class);
    assertThat(list.size(), Matchers.is(equalTo(1)));
    // with locator, nullable
    assertThat(new ElementBuilder(mock.getFactory(),
        new ElementLocationChain(LocatorBy.byCss("css"), NULLABLE))
        .buildList(Actionable.class), is(nullValue()));
    // with locator, not nullable
    assertThrows(() -> new ElementBuilder(mock.getFactory(),
        new ElementLocationChain(LocatorBy.byCss("css"), EXISTING))
        .buildList(Actionable.class));
    // with parameter
    when(mock.getWebDriverMock().findElements(By.cssSelector("css[string]")))
        .thenReturn(Collections.singletonList(mock.getWebElementMock()));
    assertThat(new ElementBuilder(mock.getFactory(),
        new ElementLocationChain(LocatorBy.byCss("css[%s]"), EXISTING))
        .buildList(Actionable.class, "string"), is(not(empty())));
  }

  @Test
  public void testBuildListWithFilter() {
    // todo
  }
}
