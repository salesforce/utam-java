/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.context;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.testng.Assert.expectThrows;

import org.testng.annotations.Test;

/**
 * Provides tests for the PlatformType enumerated type
 *
 * @author james.evans
 */
public class PlatformTypeTests {

  /**
   * The fromString method should return valid values for known types
   */
  @Test
  public void testFromString() {
    assertThat(PlatformType.fromString("native"), is(equalTo(PlatformType.NATIVE)));
    assertThat(PlatformType.fromString("web"), is(equalTo(PlatformType.WEB)));
    assertThat(PlatformType.fromString(null), is(equalTo(PlatformType.NONE)));
    assertThat(PlatformType.fromString(""), is(equalTo(PlatformType.NONE)));
  }

  /**
   * The fromString method should throw the appropriate exception for unknown types
   */
  @Test
  public void testFromStringWithUnknownTypeThrows() {
    IllegalArgumentException e = expectThrows(
        IllegalArgumentException.class,
        () -> PlatformType.fromString("illegal"));
    assertThat(e.getMessage(), containsString("Unknown platform type 'illegal'"));
  }
}
