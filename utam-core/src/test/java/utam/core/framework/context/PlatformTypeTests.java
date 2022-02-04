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
    assertThat(MobileContextType.fromString("native"), is(equalTo(MobileContextType.NATIVE)));
    assertThat(MobileContextType.fromString("web"), is(equalTo(MobileContextType.WEB)));
    assertThat(MobileContextType.fromString(null), is(equalTo(MobileContextType.NONE)));
    assertThat(MobileContextType.fromString(""), is(equalTo(MobileContextType.NONE)));
  }

  /**
   * The fromString method should throw the appropriate exception for unknown types
   */
  @Test
  public void testFromStringWithUnknownTypeThrows() {
    IllegalArgumentException e = expectThrows(
        IllegalArgumentException.class,
        () -> MobileContextType.fromString("illegal"));
    assertThat(e.getMessage(), containsString("Unknown platform type 'illegal'"));
  }
}
