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
import static org.hamcrest.Matchers.sameInstance;
import static org.testng.Assert.assertThrows;
import static utam.core.selenium.element.LocatorBy.getParametersCount;

import java.util.IllegalFormatConversionException;
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

  /** The equals method should return true for the same AbstractLocator */
  @Test
  public void testEqualsOverride() {
    LocatorBy locator = LocatorByCss.byCss("css");
    assertThat(locator, is(equalTo(locator.getCopy("css"))));
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
    assertThat(locator.setParameters("parameters"), is(sameInstance(locator)));
  }

  @Test
  public void testSetParameters() {
    assertThat(LocatorByCss.byCss("css").setParameters(true).getStringValue(), is(equalTo("css")));
    assertThat(
        LocatorByCss.byCss("css[text='%s']").setParameters("str").getStringValue(),
        is(equalTo("css[text='str']")));
    assertThat(
        LocatorByCss.byCss("css[%d]").setParameters(1).getStringValue(), is(equalTo("css[1]")));
    assertThat(
        LocatorByCss.byCss("css[%d][%s]").setParameters(1, "text").getStringValue(),
        is(equalTo("css[1][text]")));
    assertThat(
        "only necessary parameters will be applied - subset st the end of the array",
        LocatorByCss.byCss("css[%d][%s]").setParameters("x", "y", 1, "text").getStringValue(),
        is(equalTo("css[1][text]")));
  }

  @Test
  public void testSetParametersThrows() {
    assertThrows(
        ArrayIndexOutOfBoundsException.class,
        () -> LocatorByCss.byCss("css[%d][%d]").setParameters());
    assertThrows(
        IllegalFormatConversionException.class,
        () -> LocatorByCss.byCss("css[%d]").setParameters("str"));
    assertThrows(
        ArrayIndexOutOfBoundsException.class,
        () -> LocatorByCss.byCss("css[%d][%d]").setParameters(1));
  }
}
