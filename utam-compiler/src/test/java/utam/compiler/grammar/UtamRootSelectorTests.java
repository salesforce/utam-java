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
import static utam.compiler.grammar.TestUtilities.JSON_MAPPING_ERROR;
import static utam.compiler.grammar.TestUtilities.getDeserializedObject;
import static utam.compiler.grammar.UtamRootSelector.ERR_SELECTOR_MISSING;
import static utam.compiler.grammar.UtamRootSelector.ERR_SELECTOR_REDUNDANT;

import org.testng.annotations.Test;
import utam.compiler.helpers.LocatorCodeGeneration.SelectorType;
import utam.core.framework.consumer.UtamError;
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
  public void testRootNodeWithListSelectorThrows() {
    String json =
        "{"
            + "    \"css\": \".invalidList\","
            + "    \"returnAll\": true"
            + "  }";
    UtamError e = expectThrows(UtamError.class,
        () -> getDeserializedObject(json, UtamRootSelector.class));
    assertThat(e.getMessage(), containsString(JSON_MAPPING_ERROR));
  }

  @Test
  public void testRootNodeWithParameterizedSelectorThrows() {
    String json =
        "{"
            + "    \"css\": \"input[value=*(%s)]\","
            + "    \"args\": [ {\"name\": \"text\", \"type\":\"string\" }]"
            + "}";
    UtamError e = expectThrows(UtamError.class,
        () -> getDeserializedObject(json, UtamRootSelector.class));
    assertThat(e.getMessage(), containsString(JSON_MAPPING_ERROR));
  }

  @Test
  public void testMissingSelectorThrows() {
    UtamError e = expectThrows(UtamError.class,  () -> new UtamRootSelector(null, null, null, null));
    assertThat(e.getMessage(), containsString(ERR_SELECTOR_MISSING));
  }

  @Test
  public void testRedundantSelectorThrows() {
    UtamError e = expectThrows(UtamError.class, () -> new UtamRootSelector("one", "two", null, null));
    assertThat(e.getMessage(), is(equalTo(ERR_SELECTOR_REDUNDANT)));
  }
}
