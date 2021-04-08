/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import org.testng.annotations.Test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;

/**
 * Provides deserialization tests for the UtamShadowElement class
 *
 * @author james.evans
 */
public class UtamShadowElement_DeserializationTests {

  /** A UtamShadowElement object should be able to be created through deserialization */
  @Test
  public void testDeserialization() {
    String json = "{  \"elements\": [] }";
    UtamShadowElement shadow = TestUtilities.getDeserializedObject(json, UtamShadowElement.class);
    assertThat(shadow, is(not(nullValue())));
    assertThat(shadow.elements, is(not(nullValue())));
    assertThat(shadow.elements.length, is(equalTo(0)));
  }
}
