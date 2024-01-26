/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.element;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import org.testng.annotations.Test;
import utam.core.element.FindContext.Type;

/**
 * @author elizaveta.ivanova
 * @since 234
 */
public class FindContextTests {

  @Test
  public void testIsNullable() {
    assertThat(FindContext.Type.EXISTING_IN_SHADOW.isNullable(), is(false));
    assertThat(FindContext.Type.NULLABLE_IN_SHADOW.isNullable(), is(true));
    assertThat(FindContext.Type.EXISTING.isNullable(), is(false));
    assertThat(FindContext.Type.NULLABLE.isNullable(), is(true));
  }

  @Test
  public void testIsExpandScopeShadowRoot() {
    assertThat(FindContext.Type.EXISTING_IN_SHADOW.isExpandScopeShadowRoot(), is(true));
    assertThat(FindContext.Type.NULLABLE_IN_SHADOW.isExpandScopeShadowRoot(), is(true));
    assertThat(FindContext.Type.EXISTING.isExpandScopeShadowRoot(), is(false));
    assertThat(FindContext.Type.NULLABLE.isExpandScopeShadowRoot(), is(false));
  }

  @Test
  public void testBuilder() {
    assertThat(FindContext.Type.build(false, false), is(equalTo(Type.EXISTING)));
    assertThat(FindContext.Type.build(false, true), is(equalTo(Type.EXISTING_IN_SHADOW)));
    assertThat(FindContext.Type.build(true, false), is(equalTo(Type.NULLABLE)));
    assertThat(FindContext.Type.build(true, true), is(equalTo(Type.NULLABLE_IN_SHADOW)));
  }
}
