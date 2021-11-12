/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.base;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.openqa.selenium.By;
import org.openqa.selenium.WebElement;
import org.testng.annotations.Test;
import utam.core.MockUtilities;
import utam.core.element.Actionable;
import utam.core.element.Clickable;

/**
 * @author elizaveta.ivanova
 * @since 236
 */
public class BasePageObjectTests {

  @Test
  public void testProxyElementCanBeUsedInsteadRoot() {
    MockUtilities mockUtilities = new MockUtilities();
    WebElement mockElement = mock(WebElement.class);
    when(mockUtilities.getWebDriverMock().findElement(By.cssSelector("root")))
        .thenReturn(mockElement);
    TestProxyPageObject pageObject = mockUtilities.getFactory().create(TestProxyPageObject.class);
    UnionType element = pageObject.getRoot();
    element.isPresent();
  }

  interface UnionType extends Actionable, Clickable { }

  @PageMarker.Find(css = "root")
  public static class TestProxyPageObject extends BaseRootPageObject {

    public UnionType getRoot() {
      return getProxy(getRootElement(), UnionType.class);
    }
  }
}
