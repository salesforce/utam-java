/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.base;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.instanceOf;
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
import utam.core.element.FrameElement;
import utam.core.framework.element.BasePageElement;
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

  private static BasicElementBuilder getBuilder(MockUtilities mock, ElementLocation location) {
    return new BasicElementBuilder(mock.getFactory().getDriver(), mock.getElementAdapter(), location);
  }

  private static ElementLocation getNullableLocation() {
    return new ElementLocation(LocatorBy.byCss(NULLABLE_CSS), NULLABLE);
  }

  private static ElementLocation getNotNullableLocation() {
    return new ElementLocation(LocatorBy.byCss(NOT_NULLABLE_CSS), EXISTING);
  }

  @Test
  public void testBuildSingleElement() {
    MockUtilities mock = new MockUtilities();

    Exception e = expectThrows(NullPointerException.class,
        () -> getBuilder(mock, getNotNullableLocation())
            .build(Actionable.class, BasePageElement.class));
    assertThat(e.getMessage(), startsWith(ERR_ELEMENT_NOT_FOUND_PREFIX));

    when(mock.getWebElementMock().findElement(By.cssSelector(NOT_NULLABLE_CSS)))
        .thenReturn(mock.getWebElementMock());
    BasicElement test = getBuilder(mock, getNotNullableLocation())
        .build(Actionable.class, BasePageElement.class);
    assertThat(test, is(notNullValue()));
  }

  @Test
  public void testBuildFrame() {
    MockUtilities mock = new MockUtilities();
    when(mock.getWebElementMock().findElement(By.cssSelector(NOT_NULLABLE_CSS)))
        .thenReturn(mock.getWebElementMock());
    BasicElement test = getBuilder(mock, getNotNullableLocation()).buildFrame();
    assertThat(test, is(instanceOf(FrameElementImpl.class)));
  }

  @Test
  public void testBuildFrameWithParametrizedLocator() {
    MockUtilities mock = new MockUtilities();
    when(mock.getWebElementMock().findElement(By.cssSelector("css[string][1]")))
        .thenReturn(mock.getWebElementMock());
    ElementLocation location = new ElementLocation(LocatorBy.byCss("css[%s][%d]"), EXISTING);
    FrameElement test = new BasicElementBuilder(mock.getDriverAdapter(), mock.getElementAdapter(), location.setParameters("string", 1))
        .buildFrame();
    assertThat(test, is(notNullValue()));
  }

  @Test
  public void testBuilderWithParametrizedLocator() {
    MockUtilities mock = new MockUtilities();
    when(mock.getWebElementMock().findElement(By.cssSelector("css[string]")))
        .thenReturn(mock.getWebElementMock());
    ElementLocation location = new ElementLocation(LocatorBy.byCss("css[%s]"), EXISTING);
    BasicElement test = new BasicElementBuilder(mock.getDriverAdapter(), mock.getElementAdapter(), location.setParameters("string"))
        .build(Actionable.class, BasePageElement.class);
    assertThat(test, is(notNullValue()));
  }

  @Test
  public void testBuildSingleElementNullable() {
    MockUtilities mock = new MockUtilities();
    BasicElement test = new BasicElementBuilder(mock.getDriverAdapter(), mock.getElementAdapter(), getNullableLocation())
        .build(Actionable.class, BasePageElement.class);
    assertThat(test, is(nullValue()));

    when(mock.getWebElementMock().findElements(By.cssSelector(NULLABLE_CSS)))
        .thenReturn(Collections.singletonList(mock.getWebElementMock()));
    when(mock.getWebElementMock().findElement(By.cssSelector(NULLABLE_CSS)))
        .thenReturn(mock.getWebElementMock());
    test = getBuilder(mock, getNullableLocation()).build(Actionable.class, BasePageElement.class);
    assertThat(test, is(notNullValue()));
  }

  @Test
  public void testBuildSingleElementWithFilterNotNullable() {
    MockUtilities mock = new MockUtilities();
    ElementLocation location = getNotNullableLocation();

    // not found
    Exception e = expectThrows(NoSuchElementException.class, () -> getBuilder(mock, location)
        .build(Actionable.class, BasePageElement.class, UtamBase::isPresent));
    assertThat(e.getMessage(), startsWith(ERR_ELEMENT_NOT_FOUND_PREFIX));

    // found but filter does not match
    WebElement mockElement = mock.getWebElementMock();
    when(mock.getWebElementMock().findElements(By.cssSelector(NOT_NULLABLE_CSS)))
        .thenReturn(Collections.singletonList(mockElement));
    when(mock.getWebElementMock().findElement(By.cssSelector(NOT_NULLABLE_CSS)))
        .thenReturn(mockElement);
    e = expectThrows(NullPointerException.class,
        () -> getBuilder(mock, location)
            .build(Actionable.class, BasePageElement.class, UtamBase::isVisible));
    assertThat(e.getMessage(), is(equalTo(ERR_FOR_FILTER)));

    // found filter match
    when(mockElement.isDisplayed()).thenReturn(true);
    BasicElement test = getBuilder(mock, location)
        .build(Actionable.class, BasePageElement.class, UtamBase::isVisible);
    assertThat(test, is(notNullValue()));
  }

  @Test
  public void testBuildSingleElementWithFilterNullable() {
    MockUtilities mock = new MockUtilities();
    ElementLocation location = getNullableLocation();

    // not found
    BasicElement test = getBuilder(mock, location)
        .build(Actionable.class, BasePageElement.class, UtamBase::isPresent);
    assertThat(test, is(nullValue()));

    // found but filter does not match
    WebElement mockElement = mock.getWebElementMock();
    when(mockElement.findElements(By.cssSelector(NULLABLE_CSS)))
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
    ElementLocation location = getNotNullableLocation();

    // not found
    Exception e = expectThrows(NoSuchElementException.class, () -> getBuilder(mock, location)
        .buildList(Actionable.class, BasePageElement.class));
    assertThat(e.getMessage(), startsWith(ERR_ELEMENT_NOT_FOUND_PREFIX));

    // found
    when(mock.getWebElementMock().findElements(By.cssSelector(NOT_NULLABLE_CSS)))
        .thenReturn(Collections.singletonList(mock.getWebElementMock()));
    List<Actionable> list = getBuilder(mock, location).buildList(Actionable.class, BasePageElement.class);
    assertThat(list, is(not(empty())));

    // found, with parameter
    when(mock.getWebElementMock().findElements(By.cssSelector("css[string]")))
        .thenReturn(Collections.singletonList(mock.getWebElementMock()));
    assertThat(getBuilder(mock,
        new ElementLocation(LocatorBy.byCss("css[%s]"), EXISTING))
        .buildList(Actionable.class, BasePageElement.class, "string"), is(not(empty())));
  }

  @Test
  void testBuildListNullableElement() {
    MockUtilities mock = new MockUtilities();
    ElementLocation location = getNullableLocation();

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
    ElementLocation location = getNotNullableLocation();

    // not found
    Exception e = expectThrows(NoSuchElementException.class, () -> getBuilder(mock, location)
        .buildList(Actionable.class, BasePageElement.class, UtamBase::isVisible));
    assertThat(e.getMessage(), startsWith(ERR_ELEMENT_NOT_FOUND_PREFIX));

    // found but filter does not match
    WebElement mockElement = mock.getWebElementMock();
    when(mock.getWebElementMock().findElements(By.cssSelector(NOT_NULLABLE_CSS)))
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
    ElementLocation location = getNullableLocation();

    // not found
    List<Actionable> list = getBuilder(mock, location)
        .buildList(Actionable.class, BasePageElement.class, UtamBase::isVisible);
    assertThat(list, is(nullValue()));

    // found but filter does not match
    WebElement mockElement = mock.getWebElementMock();
    when(mockElement.findElements(By.cssSelector(NULLABLE_CSS)))
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
    when(mock.getWebElementMock().findElement(By.cssSelector(NOT_NULLABLE_CSS)))
        .thenReturn(mock.getWebElementMock());
    FrameElement test = getBuilder(mock, getNotNullableLocation()).build(FrameElement.class, FrameElementImpl.class);
    assertThat(test, is(notNullValue()));
  }
}
