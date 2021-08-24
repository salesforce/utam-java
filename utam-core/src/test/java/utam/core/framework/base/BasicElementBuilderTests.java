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
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.startsWith;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.empty;
import static org.hamcrest.Matchers.emptyIterable;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.nullValue;
import static org.mockito.Mockito.when;
import static org.testng.Assert.expectThrows;
import static utam.core.element.FindContext.Type.EXISTING;
import static utam.core.element.FindContext.Type.NULLABLE;
import static utam.core.framework.base.CustomElementBuilder.getFilteredElementNotFoundErr;
import static utam.core.selenium.element.DriverAdapter.ERR_ELEMENT_NOT_FOUND_PREFIX;

import java.util.Collections;
import java.util.List;
import org.openqa.selenium.By;
import org.openqa.selenium.NoSuchElementException;
import org.openqa.selenium.WebElement;
import org.testng.annotations.Test;
import utam.core.MockUtilities;
import utam.core.element.Actionable;
import utam.core.element.BasicElement;
import utam.core.element.ElementLocation;
import utam.core.element.FrameElement;
import utam.core.framework.element.BasePageElement;
import utam.core.framework.element.ElementLocationChain;
import utam.core.selenium.element.LocatorBy;

/**
 * element builder tests
 *
 * @author elizaveta.ivanova
 * @since 232
 */
public class BasicElementBuilderTests {

  private static final String ERR_FOR_FILTER = getFilteredElementNotFoundErr(Actionable.class);
  private static final String NULLABLE_CSS = "nullable";
  private static final String NOT_NULLABLE_CSS = "existing";

  private static BasicElementBuilder getBuilder(MockUtilities mock) {
    return getBuilder(mock, new ElementLocationChain(mock.getElementAdapter()));
  }

  private static BasicElementBuilder getBuilder(MockUtilities mock,
      ElementLocationChain locationChain) {
    return new BasicElementBuilder(mock.getFactory(), locationChain);
  }

  private static ElementLocationChain getNullableLocation() {
    return new ElementLocationChain(LocatorBy.byCss(NULLABLE_CSS), NULLABLE);
  }

  private static ElementLocationChain getNotNullableLocation() {
    return new ElementLocationChain(LocatorBy.byCss(NOT_NULLABLE_CSS), EXISTING);
  }

  @Test
  public void testBuildSingleElement() {
    MockUtilities mock = new MockUtilities();
    BasicElement test;

    // with WebElement
    test = getBuilder(mock).build(Actionable.class, BasePageElement.class);
    assertThat(test, is(notNullValue()));

    // with locator
    Exception e = expectThrows(NullPointerException.class,
        () -> getBuilder(mock, getNotNullableLocation())
            .build(Actionable.class, BasePageElement.class));
    assertThat(e.getMessage(), startsWith(ERR_ELEMENT_NOT_FOUND_PREFIX));

    when(mock.getWebDriverMock().findElement(By.cssSelector(NOT_NULLABLE_CSS)))
        .thenReturn(mock.getWebElementMock());
    test = getBuilder(mock, getNotNullableLocation())
        .build(Actionable.class, BasePageElement.class);
    assertThat(test, is(notNullValue()));
  }

  @Test
  public void testBuilderWithParametrizedLocator() {
    MockUtilities mock = new MockUtilities();
    BasicElement test;
    when(mock.getWebDriverMock().findElement(By.cssSelector("css[string]")))
        .thenReturn(mock.getWebElementMock());
    ElementLocation location = new ElementLocationChain(LocatorBy.byCss("css[%s]"), EXISTING);
    test = new BasicElementBuilder(mock.getFactory(), location)
        .build(Actionable.class, BasePageElement.class, "string");
    assertThat(test, is(notNullValue()));
  }

  @Test
  public void testBuildSingleElementNullable() {
    MockUtilities mock = new MockUtilities();
    BasicElement test;

    test = new BasicElementBuilder(mock.getFactory(), getNullableLocation())
        .build(Actionable.class, BasePageElement.class);
    assertThat(test, is(nullValue()));

    when(mock.getWebDriverMock().findElement(By.cssSelector(NULLABLE_CSS)))
        .thenReturn(mock.getWebElementMock());
    test = getBuilder(mock, getNullableLocation()).build(Actionable.class, BasePageElement.class);
    assertThat(test, is(notNullValue()));
  }

  @Test
  public void testBuildSingleElementWithFilterNotNullable() {
    MockUtilities mock = new MockUtilities();
    ElementLocationChain location = getNotNullableLocation();
    BasicElement test;

    // not found
    Exception e = expectThrows(NoSuchElementException.class, () -> getBuilder(mock, location)
        .build(Actionable.class, BasePageElement.class, UtamBase::isPresent));
    assertThat(e.getMessage(), startsWith(ERR_ELEMENT_NOT_FOUND_PREFIX));

    // found but filter does not match
    WebElement mockElement = mock.getWebElementMock();
    when(mock.getWebDriverMock().findElements(By.cssSelector(NOT_NULLABLE_CSS)))
        .thenReturn(Collections.singletonList(mockElement));
    e = expectThrows(NullPointerException.class,
        () -> getBuilder(mock, location)
            .build(Actionable.class, BasePageElement.class, UtamBase::isVisible));
    assertThat(e.getMessage(), is(equalTo(ERR_FOR_FILTER)));

    // found filter match
    when(mockElement.isDisplayed()).thenReturn(true);
    test = getBuilder(mock, location)
        .build(Actionable.class, BasePageElement.class, UtamBase::isVisible);
    assertThat(test, is(notNullValue()));
  }

  @Test
  public void testBuildSingleElementWithFilterNullable() {
    MockUtilities mock = new MockUtilities();
    ElementLocationChain location = getNullableLocation();
    BasicElement test;

    // not found
    test = getBuilder(mock, location)
        .build(Actionable.class, BasePageElement.class, UtamBase::isPresent);
    assertThat(test, is(nullValue()));

    // found but filter does not match
    WebElement mockElement = mock.getWebElementMock();
    when(mock.getWebDriverMock().findElements(By.cssSelector(NULLABLE_CSS)))
        .thenReturn(Collections.singletonList(mockElement));
    Exception e = expectThrows(NullPointerException.class,
        () -> getBuilder(mock, location)
            .build(Actionable.class, BasePageElement.class, UtamBase::isVisible));
    assertThat(e.getMessage(), is(equalTo(ERR_FOR_FILTER)));

    // found filter match
    when(mockElement.isDisplayed()).thenReturn(true);
    test = getBuilder(mock, location)
        .build(Actionable.class, BasePageElement.class, UtamBase::isVisible);
    assertThat(test, is(notNullValue()));
  }

  @Test
  void testBuildListNotNullableElement() {
    MockUtilities mock = new MockUtilities();
    ElementLocationChain location = getNotNullableLocation();

    // with WebElement
    List<Actionable> list = getBuilder(mock).buildList(Actionable.class, BasePageElement.class);
    assertThat(list, is(not(empty())));

    // not found
    Exception e = expectThrows(NoSuchElementException.class, () -> getBuilder(mock, location)
        .buildList(Actionable.class, BasePageElement.class));
    assertThat(e.getMessage(), startsWith(ERR_ELEMENT_NOT_FOUND_PREFIX));

    // found, with parameter
    when(mock.getWebDriverMock().findElements(By.cssSelector("css[string]")))
        .thenReturn(Collections.singletonList(mock.getWebElementMock()));
    assertThat(getBuilder(mock,
        new ElementLocationChain(LocatorBy.byCss("css[%s]"), EXISTING))
        .buildList(Actionable.class, BasePageElement.class, "string"), is(not(empty())));
  }

  @Test
  void testBuildListNullableElement() {
    MockUtilities mock = new MockUtilities();
    ElementLocationChain location = getNullableLocation();

    // not found
    List<Actionable> list = getBuilder(mock, location)
        .buildList(Actionable.class, BasePageElement.class);
    assertThat(list, is(nullValue()));

    // found
    WebElement mockElement = mock.getWebElementMock();
    when(mock.getWebDriverMock().findElements(By.cssSelector(NULLABLE_CSS)))
        .thenReturn(Collections.singletonList(mockElement));
    list = getBuilder(mock, location).buildList(Actionable.class, BasePageElement.class);
    assertThat(list, is(not(emptyIterable())));
  }

  @Test
  void testBuildListWithFilterNotNullableElement() {
    MockUtilities mock = new MockUtilities();
    ElementLocationChain location = getNotNullableLocation();

    // not found
    Exception e = expectThrows(NoSuchElementException.class, () -> getBuilder(mock, location)
        .buildList(Actionable.class, BasePageElement.class, UtamBase::isVisible));
    assertThat(e.getMessage(), startsWith(ERR_ELEMENT_NOT_FOUND_PREFIX));

    // found but filter does not match
    WebElement mockElement = mock.getWebElementMock();
    when(mock.getWebDriverMock().findElements(By.cssSelector(NOT_NULLABLE_CSS)))
        .thenReturn(Collections.singletonList(mockElement));
    List<Actionable> list = getBuilder(mock, location)
        .buildList(Actionable.class, BasePageElement.class, UtamBase::isVisible);
    assertThat(list, is(emptyIterable()));

    // found, filter matches
    when(mockElement.isDisplayed()).thenReturn(true);
    list = getBuilder(mock, location)
        .buildList(Actionable.class, BasePageElement.class, UtamBase::isVisible);
    assertThat(list, is(not(emptyIterable())));
  }

  @Test
  void testBuildListWithFilterNullableElement() {
    MockUtilities mock = new MockUtilities();
    ElementLocationChain location = getNullableLocation();

    // not found
    List<Actionable> list = getBuilder(mock, location)
        .buildList(Actionable.class, BasePageElement.class, UtamBase::isVisible);
    assertThat(list, is(nullValue()));

    // found but filter does not match
    WebElement mockElement = mock.getWebElementMock();
    when(mock.getWebDriverMock().findElements(By.cssSelector(NULLABLE_CSS)))
        .thenReturn(Collections.singletonList(mockElement));
    list = getBuilder(mock, location)
        .buildList(Actionable.class, BasePageElement.class, UtamBase::isVisible);
    assertThat(list, is(emptyIterable()));

    // found, filter matches
    when(mockElement.isDisplayed()).thenReturn(true);
    list = getBuilder(mock, location)
        .buildList(Actionable.class, BasePageElement.class, UtamBase::isVisible);
    assertThat(list, is(not(emptyIterable())));
  }

  @Test
  public void testBuildFrameElement() {
    MockUtilities mock = new MockUtilities();
    FrameElement test = getBuilder(mock).build(FrameElement.class, FrameElementImpl.class);
    assertThat(test, is(notNullValue()));
  }
}
