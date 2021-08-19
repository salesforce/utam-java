/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.element;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.Mockito.mock;

import org.testng.annotations.Test;
import utam.core.element.Element;
import utam.core.element.FindContext.Type;
import utam.core.selenium.element.LocatorBy;

/**
 * @author elizaveta.ivanova
 * @since 234
 */
public class ElementLocationChainTests {

  @Test
  public void testScope() {
    Element element = mock(Element.class);
    assertThat(new ElementLocationChain(element).scope(LocatorBy.byCss("css"), Type.EXISTING)
            .getLocatorChainString(),
        is(equalTo("driver > element > By.cssSelector: css")));
    assertThat(new ElementLocationChain(LocatorBy.byCss("css1"), Type.NULLABLE)
            .scope(LocatorBy.byCss("css2"), Type.EXISTING_IN_SHADOW).getLocatorChainString(),
        is(equalTo("driver > By.cssSelector: css1 >> By.cssSelector: css2")));
  }
}