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
import static utam.core.framework.context.StringValueProfile.ERR_NAME_REQUIRED;
import static utam.core.framework.context.StringValueProfile.ERR_VALUE_REQUIRED;

import org.testng.annotations.Test;
import utam.core.framework.consumer.UtamError;

public class StringValueProfileTests {

  @Test
  public void testStringValueProfile() {
    StringValueProfile profile = new StringValueProfile("testName", "testValue");
    assertThat(profile.getName(), is(equalTo("testName")));
    assertThat(profile.getValue(), is(equalTo("testValue")));
    assertThat(profile.getKey(), is(equalTo("testNametestValue")));
  }

  @Test
  public void testStringValueProfileWithNullNameThrows() {
    UtamError e = expectThrows(UtamError.class, () -> new StringValueProfile(null, "testValue"));
    assertThat(e.getMessage(), containsString(ERR_NAME_REQUIRED));
  }

  @Test
  public void testStringValueProfileWithEmptyNameThrows() {
    UtamError e = expectThrows(UtamError.class, () -> new StringValueProfile("", "testValue"));
    assertThat(e.getMessage(), containsString(ERR_NAME_REQUIRED));
  }

  @Test
  public void testStringValueProfileWithNullValueThrows() {
    UtamError e = expectThrows(UtamError.class, () -> new StringValueProfile("testName", null));
    assertThat(e.getMessage(), containsString(ERR_VALUE_REQUIRED));
  }

  @Test
  public void testStringValueProfileWithEmptyValueThrows() {
    UtamError e = expectThrows(UtamError.class, () -> new StringValueProfile("testName", ""));
    assertThat(e.getMessage(), containsString(ERR_VALUE_REQUIRED));
  }

  @Test
  public void testStringValueProfileEquality() {
    StringValueProfile profile = new StringValueProfile("testName", "testValue");
    StringValueProfile anotherProfile = new StringValueProfile("testName", "testValue");
    assertThat(profile.equals(anotherProfile), is(equalTo(true)));
  }

  @Test
  public void testStringValueProfileInequality() {
    StringValueProfile profile = new StringValueProfile("testName", "testValue");
    StringValueProfile anotherProfile = new StringValueProfile("testName", "unmatchedValue");
    assertThat(profile.equals(anotherProfile), is(equalTo(false)));
  }

  @Test
  public void testStringValueProfileInequalityWithDifferentType() {
    StringValueProfile profile = new StringValueProfile("testName", "testValue");
    assertThat(profile.equals(new Object()), is(equalTo(false)));
  }
}
