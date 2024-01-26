/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.testng.Assert.expectThrows;

import org.testng.annotations.Test;
import utam.compiler.UtamCompilationError;
import utam.compiler.helpers.LocatorCodeGeneration.SelectorType;
import utam.core.selenium.element.LocatorBy;

/**
 * Provides deserialization tests for the UtamSelector class
 *
 * @author elizaveta.ivanova
 * @since 228
 */
public class UtamRootSelectorTests {

  @Test
  public void testSimpleCssSelector() {
    String value = ".css";
    UtamRootSelector selector = new UtamRootSelector(value, null, null, null);
    assertThat(selector.getLocator(), is(equalTo(LocatorBy.byCss(value))));
    assertThat(selector.getSelectorType(), is(SelectorType.css));
  }

  @Test
  public void testMissingSelectorThrows() {
    String json = "{ \"root\" : true, \"selector\" : {}}";
    RuntimeException e =
        expectThrows(
            UtamCompilationError.class,
            () -> new DeserializerUtilities().getResultFromString(json));
    assertThat(
        e.getMessage(),
        containsString(
            "error 1002: element \"root\" selector: "
                + "one of \"css\", \"accessid\", \"uiautomator\" or \"classchain\" should be set"));
  }

  @Test
  public void testRedundantSelectorThrows() {
    String json =
        "{ \"root\" : true, \"selector\" : { \"css\": \"css\", \"accessid\": \"accessid\"}}";
    RuntimeException e =
        expectThrows(
            UtamCompilationError.class,
            () -> new DeserializerUtilities().getResultFromString(json));
    assertThat(
        e.getMessage(),
        containsString(
            "error 1003: element \"root\" selector: only one of \"css\", \"accessid\","
                + " \"uiautomator\" or \"classchain\" can be set"));
  }
}
