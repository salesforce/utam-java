/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.base;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.startsWith;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.emptyIterable;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;
import static org.testng.Assert.expectThrows;
import static utam.core.element.FindContext.Type.EXISTING;
import static utam.core.element.FindContext.Type.NULLABLE;

import java.util.List;
import org.openqa.selenium.NotFoundException;
import org.testng.annotations.Test;
import utam.core.MockUtilities.MockAdapter;
import utam.core.element.Element;
import utam.core.element.ElementLocation;
import utam.core.framework.element.ElementLocationChain;
import utam.core.selenium.element.LocatorBy;

/**
 * element builder tests
 *
 * @author elizaveta.ivanova
 * @since 234
 */
public class CustomElementBuilderTests {

  private static final String NOTHING_FOUND_ERR = "can't find element";

  private static CustomElementBuilder getNullBuilder(PageObjectsFactory factory) {
    return new CustomElementBuilder(factory, null, LocatorBy.byCss("notfound"),
        EXISTING);
  }

  private static CustomElementBuilder getNullableBuilder(PageObjectsFactory factory) {
    return new CustomElementBuilder(factory, null, LocatorBy.byCss("notfound"),
        NULLABLE);
  }

  @Test
  public void testBuild() {
    MockAdapter mock = new MockAdapter();
    Element rootElement = mock.getElementAdapter();
    PageObjectsFactory factory = mock.getFactory();
    ElementLocation root = new ElementLocationChain(rootElement);

    // not null
    CustomElementBuilder builder = new CustomElementBuilder(factory, root);
    TestPageObject instance = builder.build(TestPageObject.class);
    assertThat(instance, is(notNullValue()));

    // nothing found
    Exception e = expectThrows(NullPointerException.class,
        () -> getNullBuilder(factory).build(TestPageObject.class));
    assertThat(e.getMessage(), startsWith(NOTHING_FOUND_ERR));

    // nullable
    instance = getNullableBuilder(factory).build(TestPageObject.class);
    assertThat(instance, is(nullValue()));
  }

  @Test
  public void testBuildList() {
    MockAdapter mock = new MockAdapter();
    Element rootElement = mock.getElementAdapter();
    PageObjectsFactory factory = mock.getFactory();
    ElementLocation root = new ElementLocationChain(rootElement);

    // not null
    CustomElementBuilder builder = new CustomElementBuilder(factory, root);
    List<TestPageObject> instances = builder.buildList(TestPageObject.class);
    assertThat(instances, is(not(emptyIterable())));

    // nothing found
    Exception e = expectThrows(NotFoundException.class,
        () -> getNullBuilder(factory).buildList(TestPageObject.class));
    assertThat(e.getMessage(), startsWith(NOTHING_FOUND_ERR));

    // nullable
    instances = getNullableBuilder(factory).buildList(TestPageObject.class);
    assertThat(instances, is(nullValue()));
  }

  @Test
  public void testBuildWithFilter() {
    MockAdapter mock = new MockAdapter();
    Element rootElement = mock.getElementAdapter();
    PageObjectsFactory factory = mock.getFactory();
    ElementLocation root = new ElementLocationChain(rootElement);

    // not null, filter returns true
    CustomElementBuilder builder = new CustomElementBuilder(factory, root);
    TestPageObject instance = builder
        .build(TestPageObject.class, testPageObject -> testPageObject != null);
    assertThat(instance, is(notNullValue()));

    // not null, filter returns false
    NotFoundException e = expectThrows(NotFoundException.class,
        () -> builder.build(TestPageObject.class, testPageObject -> testPageObject.isFalse()));
    assertThat(e.getMessage(), startsWith(NOTHING_FOUND_ERR));

    // nothing found
    e = expectThrows(NotFoundException.class,
        () -> getNullBuilder(factory)
            .build(TestPageObject.class, testPageObject -> testPageObject != null));
    assertThat(e.getMessage(), startsWith(NOTHING_FOUND_ERR));

    // nullable, filter returns false
    instance = getNullableBuilder(factory)
        .build(TestPageObject.class, testPageObject -> testPageObject.isFalse());
    assertThat(instance, is(nullValue()));
  }

  @Test
  public void testBuildListWithFilter() {
    MockAdapter mock = new MockAdapter();
    Element rootElement = mock.getElementAdapter();
    PageObjectsFactory factory = mock.getFactory();
    ElementLocation root = new ElementLocationChain(rootElement);

    // not null, filter returns true
    CustomElementBuilder builder = new CustomElementBuilder(factory, root);
    List<TestPageObject> instances = builder
        .buildList(TestPageObject.class, testPageObject -> testPageObject != null);
    assertThat(instances, is(not(emptyIterable())));

    // not null, filter returns false
    instances = builder
        .buildList(TestPageObject.class, testPageObject -> testPageObject.isFalse());
    assertThat(instances, is(emptyIterable()));

    // nothing found
    NotFoundException e = expectThrows(NotFoundException.class,
        () -> getNullBuilder(factory)
            .buildList(TestPageObject.class, testPageObject -> testPageObject != null));
    assertThat(e.getMessage(), startsWith("can't find element"));

    // nullable, filter returns false
    instances = getNullableBuilder(factory)
        .buildList(TestPageObject.class, testPageObject -> testPageObject.isFalse());
    assertThat(instances, is(emptyIterable()));
  }

  // has to be public to construct with reflections
  public static class TestPageObject extends BasePageObject {

    public boolean isFalse() {
      return false;
    }
  }
}
