/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static utam.core.framework.base.BasicElementBuilder.getUnwrappedElement;

import org.openqa.selenium.By;
import org.openqa.selenium.WebElement;
import org.testng.annotations.Test;
import utam.core.element.Actionable;
import utam.core.element.Clickable;
import utam.core.framework.base.BaseRootPageObject;
import utam.core.framework.base.PageMarker;

/**
 * this test should be in external package to test Proxy
 *
 * @author elizaveta.ivanova
 * @since 238
 */
public class OutsidePackageProxyTest {

  @Test
  public void testProxyElementCanBeUsed() {
    MockUtilities mockUtilities = new MockUtilities();
    WebElement mockElement = mock(WebElement.class);
    when(mockUtilities.getWebDriverMock().findElement(By.cssSelector("root")))
        .thenReturn(mockElement);
    TestProxyPageObject pageObject = mockUtilities.getFactory().create(TestProxyPageObject.class);
    UnionType element = pageObject.getRoot();
    element.isPresent();
    getUnwrappedElement(element);
  }

  interface UnionType extends Actionable, Clickable {}

  @SuppressWarnings("WeakerAccess")
  @PageMarker.Find(css = "root")
  public static class TestProxyPageObject extends BaseRootPageObject {

    public UnionType getRoot() {
      return getProxy(getRootElement(), UnionType.class);
    }
  }
}
