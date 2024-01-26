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
import static org.hamcrest.CoreMatchers.startsWith;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.emptyIterable;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;
import static org.mockito.Mockito.when;
import static org.testng.Assert.expectThrows;
import static utam.core.element.FindContext.Type.EXISTING;
import static utam.core.element.FindContext.Type.NULLABLE;
import static utam.core.framework.base.BasicElementBuilder.NULL_SCOPE_ERR;

import java.util.Collections;
import java.util.List;
import java.util.Objects;
import org.openqa.selenium.By;
import org.openqa.selenium.NoSuchElementException;
import org.openqa.selenium.NotFoundException;
import org.testng.annotations.Test;
import utam.core.MockUtilities;
import utam.core.element.Element;
import utam.core.element.FindContext;
import utam.core.selenium.element.LocatorBy;

/**
 * element builder tests
 *
 * @author elizaveta.ivanova
 * @since 234
 */
public class CustomElementBuilderTests {

  private static final String NOTHING_FOUND_ERR = "can't find element";

  private static CustomElementBuilder getNullBuilder(MockUtilities mock) {
    return new CustomElementBuilder(
        mock.getFactory(),
        mock.getElementAdapter(),
        new ElementLocation(LocatorBy.byCss("notfound"), EXISTING));
  }

  private static CustomElementBuilder getNullableBuilder(MockUtilities mock) {
    return new CustomElementBuilder(
        mock.getFactory(),
        mock.getElementAdapter(),
        new ElementLocation(LocatorBy.byCss("notfound"), NULLABLE));
  }

  private static CustomElementBuilder getBuilder(MockUtilities mock, boolean isNullable) {
    when(mock.getWebElementMock().findElement(By.cssSelector("found")))
        .thenReturn(mock.getWebElementMock());
    when(mock.getWebElementMock().findElements(By.cssSelector("found")))
        .thenReturn(Collections.singletonList(mock.getWebElementMock()));
    FindContext.Type findContext = FindContext.Type.build(isNullable, false);
    return new CustomElementBuilder(
        mock.getFactory(),
        mock.getElementAdapter(),
        new ElementLocation(LocatorBy.byCss("found"), findContext));
  }

  private static CustomElementBuilder getBuilder(MockUtilities mock) {
    return getBuilder(mock, false);
  }

  @Test
  public void testBuildSingleElement() {
    MockUtilities mock = new MockUtilities();

    // not null
    CustomElementBuilder builder = getBuilder(mock);
    TestPageObject instance = builder.build(TestPageObject.class);
    assertThat(instance, is(notNullValue()));

    // nothing found
    Exception e =
        expectThrows(
            NoSuchElementException.class, () -> getNullBuilder(mock).build(TestPageObject.class));
    assertThat(e.getMessage(), startsWith(NOTHING_FOUND_ERR));

    // nullable
    instance = getNullableBuilder(mock).build(TestPageObject.class);
    assertThat(instance, is(nullValue()));
  }

  @Test
  public void testBuildList() {
    MockUtilities mock = new MockUtilities();

    // not null
    CustomElementBuilder builder = getBuilder(mock);
    List<TestPageObject> instances = builder.buildList(TestPageObject.class);
    assertThat(instances, is(not(emptyIterable())));

    // nothing found
    Exception e =
        expectThrows(
            NotFoundException.class, () -> getNullBuilder(mock).buildList(TestPageObject.class));
    assertThat(e.getMessage(), startsWith(NOTHING_FOUND_ERR));

    // nullable
    instances = getNullableBuilder(mock).buildList(TestPageObject.class);
    assertThat(instances, is(nullValue()));
  }

  @Test
  public void testBuildSingleElementWithFilter() {
    MockUtilities mock = new MockUtilities();

    // not null, filter returns true
    CustomElementBuilder builder = getBuilder(mock);
    TestPageObject instance = builder.build(TestPageObject.class, Objects::nonNull);
    assertThat(instance, is(notNullValue()));

    // not null, filter returns false
    Exception e =
        expectThrows(
            NullPointerException.class,
            () -> builder.build(TestPageObject.class, TestPageObject::isFalse));
    assertThat(e.getMessage(), startsWith(NOTHING_FOUND_ERR));

    // nothing found
    e =
        expectThrows(
            NotFoundException.class,
            () -> getNullBuilder(mock).build(TestPageObject.class, Objects::nonNull));
    assertThat(e.getMessage(), startsWith(NOTHING_FOUND_ERR));
  }

  @Test
  public void testFindFirstWithFilterNullableFound() {
    MockUtilities mock = new MockUtilities();
    TestPageObject instance =
        getBuilder(mock, true).build(TestPageObject.class, TestPageObject::isFalse);
    assertThat(instance, is(nullValue()));
  }

  @Test
  public void testFindFirstWithFilterNullableNothingFound() {
    MockUtilities mock = new MockUtilities();
    TestPageObject instance =
        getNullableBuilder(mock).build(TestPageObject.class, TestPageObject::isFalse);
    assertThat(instance, is(nullValue()));
  }

  @Test
  public void testBuildListWithFilter() {
    MockUtilities mock = new MockUtilities();

    // not null, filter returns true
    CustomElementBuilder builder = getBuilder(mock);
    List<TestPageObject> instances = builder.buildList(TestPageObject.class, Objects::nonNull);
    assertThat(instances, is(not(emptyIterable())));

    // not null, filter returns false
    instances = builder.buildList(TestPageObject.class, TestPageObject::isFalse);
    assertThat(instances, is(emptyIterable()));

    // not nullable, nothing found
    NotFoundException e =
        expectThrows(
            NotFoundException.class,
            () -> getNullBuilder(mock).buildList(TestPageObject.class, Objects::nonNull));
    assertThat(e.getMessage(), startsWith("can't find element"));

    // nullable, nothing found
    instances = getNullableBuilder(mock).buildList(TestPageObject.class, TestPageObject::isFalse);
    assertThat(instances, is(nullValue()));
  }

  @Test
  public void testNullScopeWithNullableBuilder() {
    MockUtilities mock = new MockUtilities();
    Object test =
        new CustomElementBuilder(
                mock.getFactory(),
                (Element) null,
                new ElementLocation(LocatorBy.byCss("found"), NULLABLE))
            .build(TestPageObject.class);
    assertThat(test, is(nullValue()));
  }

  @Test
  public void testNullScopeWithNotNullableBuilderThrows() {
    MockUtilities mock = new MockUtilities();
    NoSuchElementException e =
        expectThrows(
            NoSuchElementException.class,
            () ->
                new CustomElementBuilder(
                    mock.getFactory(),
                    (Element) null,
                    new ElementLocation(LocatorBy.byCss("existing"), EXISTING)));
    assertThat(e.getMessage(), containsString(String.format(NULL_SCOPE_ERR, "existing")));
  }

  /**
   * when calling getter for custom element, make sure element is injected by finding locator inside
   * root and not root locator inside the driver
   */
  @Test
  public void testElementForRootPageObjectInjected() {
    MockUtilities mock = new MockUtilities();
    CustomElementBuilder builder = getBuilder(mock);
    TestRootPageObject instance = builder.build(TestRootPageObject.class);
    assertThat(instance, is(notNullValue()));
    assertThat(
        "locator is injected from parent",
        instance.getRootLocator().getStringValue(),
        is(equalTo("found")));
    assertThat("element is injected from parent", instance.getElement(), is(notNullValue()));
  }

  // has to be public to construct with reflections
  public static class TestPageObject extends BasePageObject {

    private boolean isFalse() {
      return false;
    }
  }

  @PageMarker.Find(css = "any")
  public static class TestRootPageObject extends BaseRootPageObject {}
}
