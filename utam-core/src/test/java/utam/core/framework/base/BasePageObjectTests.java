/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.base;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static utam.core.framework.base.BasicElementBuilder.getUnwrappedElement;

import org.testng.annotations.Test;
import utam.core.MockUtilities;
import utam.core.element.Actionable;
import utam.core.element.Clickable;
import utam.core.element.Locator;
import utam.core.framework.element.BasePageElement;
import utam.core.selenium.element.LocatorBy;

/**
 * @author elizaveta.ivanova
 * @since 236
 */
public class BasePageObjectTests {

  @Test
  public void testGetProxyDoesNotThrow() {
    BasePageElement basePageElement = new BasePageElement();
    UnionType unionType = new TestPageObject().getProxy(basePageElement, UnionType.class);
    getUnwrappedElement(unionType);
  }

  @Test
  public void testInitialize() {
    MockUtilities utilities = new MockUtilities();
    ElementDeclaration test = new ElementDeclaration(utilities, LocatorBy.byCss("test"));
    ElementLocation elementLocation = test.one;
    assertThat(elementLocation.locator.getStringValue(), is("css"));
    assertThat(elementLocation.findContext.isNullable(), is(false));
    assertThat(elementLocation.findContext.isExpandScopeShadowRoot(), is(false));
    elementLocation = test.two;
    assertThat(elementLocation.locator.getStringValue(), is("css"));
    assertThat(elementLocation.findContext.isNullable(), is(true));
    assertThat(elementLocation.findContext.isExpandScopeShadowRoot(), is(false));
    elementLocation = test.three;
    assertThat(elementLocation.locator.getStringValue(), is("css"));
    assertThat(elementLocation.findContext.isNullable(), is(false));
    assertThat(elementLocation.findContext.isExpandScopeShadowRoot(), is(true));
  }

  private static class TestPageObject extends BasePageObject {}

  interface UnionType extends Actionable, Clickable {}

  @SuppressWarnings("unused")
  static class ElementDeclaration extends BasePageObject {

    @ElementMarker.Find(css = "css")
    ElementLocation one;

    @ElementMarker.Find(css = "css", nullable = true)
    ElementLocation two;

    @ElementMarker.Find(css = "css", expand = true)
    ElementLocation three;

    ElementDeclaration(MockUtilities utilities, Locator root) {
      utilities.getFactory().bootstrap(this, utilities.getElementAdapter(), root);
    }
  }
}
