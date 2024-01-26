/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.consumer;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;

import org.testng.annotations.Test;

/**
 * Provides tests for the UtamError class
 *
 * @author james.evans
 */
public class UtamErrorTests {

  /** A valid UtamError instance should be able to be created */
  @Test
  public void testCreation() {
    UtamError error = new UtamError("foo");
    assertThat(error.getMessage(), is(equalTo("foo")));
  }

  /** A valid UtamError instance should be able to be created with an underlying exception */
  @Test
  public void testCreationWithCause() {
    UtamError error = new UtamError("bar", new UnsupportedOperationException("baz"));
    assertThat(error.getMessage(), is(equalTo("bar")));
    assertThat(error.getCause(), is(instanceOf(UnsupportedOperationException.class)));
    assertThat(error.getCause().getMessage(), is(equalTo("baz")));
  }
}
