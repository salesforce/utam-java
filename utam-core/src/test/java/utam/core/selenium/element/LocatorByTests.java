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
import static org.hamcrest.Matchers.not;
import static utam.core.selenium.element.LocatorBy.getParametersCount;

import org.openqa.selenium.By;
import org.testng.annotations.Test;

/**
 * @author elizaveta.ivanova
 * @since 234
 */
public class LocatorByTests {

  @Test
  public void testParametersCount() {
    assertThat(getParametersCount(""), is(equalTo(0)));
    assertThat(getParametersCount("testString"), is(equalTo(0)));
    assertThat(getParametersCount("testString[%s]"), is(equalTo(1)));
    assertThat(getParametersCount("%s"), is(equalTo(1)));
    assertThat(getParametersCount("testString[%d]"), is(equalTo(1)));
    assertThat(getParametersCount("testString[%f]"), is(equalTo(0)));
  }

  /**
   * The equals method should return true for the same AbstractLocator
   */
  @Test
  public void testEqualsOverride() {
    LocatorBy locator = LocatorByCss.byCss("css");
    assertThat(locator, is(equalTo(locator.getCopy())));
    assertThat(locator, is(equalTo(locator)));
    assertThat(locator, is(not(equalTo(LocatorBy.byClassChain("css")))));
    assertThat(locator.hashCode(), is(equalTo(locator.hashCode())));
  }

  @Test
  public void testLocatorByCss() {
    final String selectorStr = "css";
    LocatorBy locator = LocatorByCss.byCss(selectorStr);
    assertThat(locator.getStringValue(), is(equalTo(selectorStr)));
    assertThat(locator.getValue(), is(equalTo(By.cssSelector(selectorStr))));
    assertThat(locator.getCopy(selectorStr), is(equalTo(locator)));
    assertThat(locator.setParameters(0, "parameters").getValue(), is(equalTo(locator)));
  }
}
