/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.selenium.element;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.testng.Assert.assertThrows;

import io.appium.java_client.MobileBy;
import org.testng.annotations.Test;
import utam.core.selenium.element.Selector.Type;
import utam.core.selenium.element.Web.SelectorImpl;

/**
 * @author elizaveta.ivanova
 * @since 232
 */
public class SelectorImplTests {

  @Test
  public void testByMethod() {
    assertThrows(IllegalArgumentException.class, () -> new SelectorImpl(null, "value").by());
    assertThat(new SelectorImpl(Selector.Type.UIAUTOMATOR, "test").by(), is(equalTo(MobileBy.AndroidUIAutomator("test"))));
  }

  @Test
  public void testSelectorStaticMethods() {
    final String selectorStr = "css";
    Selector selector = Selector.byCss(selectorStr);
    assertThat(selector.getType(), is(equalTo(Type.CSS)));
    assertThat(selector.getValue(), is(equalTo(selectorStr)));

    selector = Selector.byClassChain(selectorStr);
    assertThat(selector.getType(), is(equalTo(Type.CLASSCHAIN)));
    assertThat(selector.getValue(), is(equalTo(selectorStr)));

    selector = Selector.byUiAutomator(selectorStr);
    assertThat(selector.getType(), is(equalTo(Type.UIAUTOMATOR)));
    assertThat(selector.getValue(), is(equalTo("new UiSelector().css")));

    selector = Selector.byAccessibilityId(selectorStr);
    assertThat(selector.getType(), is(equalTo(Type.ACCESSID)));
    assertThat(selector.getValue(), is(equalTo(selectorStr)));
  }
}
