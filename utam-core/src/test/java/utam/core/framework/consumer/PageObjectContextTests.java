/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.consumer;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.testng.Assert.expectThrows;
import static utam.core.framework.consumer.PageObjectContextImpl.ERR_GET_IMPL_BY_NAME;
import static utam.core.framework.consumer.PageObjectContextImpl.ERR_GET_INSTANCE_BY_NAME;

import java.util.Collections;
import java.util.HashMap;
import org.testng.annotations.Test;
import utam.core.framework.base.BasePageObject;
import utam.core.framework.base.PageObject;
import utam.core.framework.consumer.impl.TestLoaderConfigPageObjectImpl;

/**
 * @author elizaveta.ivanova
 * @since 234
 */
public class PageObjectContextTests {

  @Test
  public void testEmptyContext() {
    PageObjectContext context = new PageObjectContextImpl(new HashMap<>());
    PageObject pageObject = context.getBean(TestLoaderConfigPageObject.class);
    assertThat(pageObject, is(instanceOf(TestLoaderConfigPageObjectImpl.class)));
    pageObject = context.getBean(TestLoaderConfigPageObjectImpl.class);
    assertThat(pageObject, is(instanceOf(TestLoaderConfigPageObjectImpl.class)));
  }

  @Test
  public void testBeanWithoutDefaultImplThrows() {
    PageObjectContext context = new PageObjectContextImpl(new HashMap<>());
    UtamError e =
        expectThrows(UtamError.class, () -> context.getBean(PageObjectWithoutImplementation.class));
    String message =
        String.format(ERR_GET_IMPL_BY_NAME, PageObjectWithoutImplementation.class.getName());
    assertThat(e.getMessage(), is(equalTo(message)));
  }

  @Test
  public void testBeanCantBeConstructedThrows() {
    PageObjectContext context =
        new PageObjectContextImpl(
            Collections.singletonMap(
                PageObjectWithoutImplementation.class, PageObjectCantBeConstructed.class));
    UtamError e =
        expectThrows(UtamError.class, () -> context.getBean(PageObjectWithoutImplementation.class));
    assertThat(
        e.getMessage(),
        is(
            equalTo(
                String.format(
                    ERR_GET_INSTANCE_BY_NAME, PageObjectWithoutImplementation.class.getName()))));
  }

  interface PageObjectWithoutImplementation extends PageObject {}

  static class PageObjectCantBeConstructed extends BasePageObject
      implements PageObjectWithoutImplementation {

    private PageObjectCantBeConstructed(boolean is) {
      assert is;
    }
  }
}
