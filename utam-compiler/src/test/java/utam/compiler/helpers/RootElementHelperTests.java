/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.helpers;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.testng.Assert.expectThrows;
import static utam.compiler.helpers.RootElementHelper.ERR_UNSUPPORTED_ELEMENT_TYPE;

import org.testng.annotations.Test;
import utam.compiler.grammar.DeserializerUtilities;
import utam.core.framework.consumer.UtamError;

/**
 * @author elizaveta.ivanova
 * @since 236
 */
public class RootElementHelperTests {

  private static final String ELEMENT_NAME = "test";

  private static void getContext(String fileName) {
    new DeserializerUtilities().getContext("validate/element/" + fileName);
  }

  @Test
  public void testElementNodeWithInvalidArrayElementTypeThrows() {
    String expectedError = String.format(ERR_UNSUPPORTED_ELEMENT_TYPE, ELEMENT_NAME, "[ true ]");
    UtamError e = expectThrows(UtamError.class, () -> getContext("wrongBasicTypeArray"));
    assertThat(e.getCause().getMessage(), containsString(expectedError));
  }

  @Test
  public void testElementTypeAsStringWithInvalidValueThrows() {
    String expectedError = String.format(ERR_UNSUPPORTED_ELEMENT_TYPE, ELEMENT_NAME, "\"wrong\"");
    UtamError e = expectThrows(UtamError.class, () -> getContext("wrongBasicType"));
    assertThat(e.getCause().getMessage(), containsString(expectedError));
  }

  @Test
  public void testElementNodeWithInvalidArrayElementThrows() {
    String expectedError = String.format(ERR_UNSUPPORTED_ELEMENT_TYPE, ELEMENT_NAME, "[ \"wrong\" ]");
    UtamError e = expectThrows(UtamError.class, () -> getContext("wrongBasicTypeArrayElement"));
    assertThat(e.getCause().getMessage(), containsString(expectedError));
  }
}
