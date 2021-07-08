/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.translator;

import utam.core.declarative.translator.ProfileConfiguration;
import utam.core.framework.consumer.UtamError;
import utam.core.framework.context.Profile;
import org.testng.annotations.Test;
import utam.core.framework.context.StringValueProfile;

import static utam.compiler.translator.StringValueProfileConfig.*;
import static org.testng.Assert.expectThrows;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;

public class StringValueProfileConfigTests {

  @Test
  public void testStringValueProfileConfig() {
    assertThat(
        new StringValueProfileConfig("testName", "testValue"),
        is(not(nullValue())));
  }

  @Test
  public void testStringValueProfileConfigFromProfiles() {
    Profile profile = new StringValueProfile("testName", "testValue");
    assertThat(
        new StringValueProfileConfig("name", profile),
        is(not(nullValue())));
  }
  
  @Test
  public void testStringValueProfileConfigWithEmptyNameThrows() {
    UtamError e = expectThrows(
        UtamError.class,
        () -> new StringValueProfileConfig("", "testValue"));
    assertThat(e.getMessage(), containsString(ERR_NAME_REQUIRED));
  }
  
  @Test
  public void testStringValueProfileConfigWithNullNameThrows() {
    UtamError e = expectThrows(
        UtamError.class,
        () -> new StringValueProfileConfig(null, "testValue"));
    assertThat(e.getMessage(), containsString(ERR_NAME_REQUIRED));
  }
  
  @Test
  public void testStringValueProfileConfigWithEmptyValuesThrows() {
    UtamError e = expectThrows(
        UtamError.class,
        () -> new StringValueProfileConfig("testName", new String[] {}));
    assertThat(
        e.getMessage(),
        containsString(ERR_VALUES_REQUIRED));
  }
  
  @Test
  public void testStringValueProfileConfigContainingNullValueThrows() {
    UtamError e = expectThrows(
        UtamError.class,
        () -> new StringValueProfileConfig("testName", (String) null));
    assertThat(
        e.getMessage(),
        containsString(ERR_VALUES_REQUIRED));
  }
  
  @Test
  public void testStringValueProfileConfigContainingEmptyStringValueThrows() {
    UtamError e = expectThrows(
        UtamError.class,
        () -> new StringValueProfileConfig("testName", ""));
    assertThat(
        e.getMessage(),
        containsString(ERR_VALUES_REQUIRED));
  }

  @Test
  public void testGetFromString() {
    ProfileConfiguration config = new StringValueProfileConfig(
        "testName", new String[] {"testValue", "anotherTestValue"});
    Profile profile = config.getFromString("testValue");
    assertThat(
        profile.getName(),
        is(equalTo("testName")));
    assertThat(
        profile.getValue(),
        is(equalTo("testValue")));
 }

  @Test
  public void testGetFromStringWithInvalidValueThrows() {
    ProfileConfiguration config = new StringValueProfileConfig(
        "testName", new String[] {"testValue", "anotherTestValue"});
    UtamError e = expectThrows(
        UtamError.class,
        () -> config.getFromString("invalidValue"));
    assertThat(
        e.getMessage(),
        is(equalTo(String.format(
            ERR_PROFILE_VALUE_INCORRECT,
            "testName",
            "invalidValue"))));
  }

  @Test
  public void testGetPropertyKey() {
    ProfileConfiguration config = new StringValueProfileConfig(
        "testName", "testValue");
    assertThat(config.getPropertyKey(), is(equalTo("testName")));
  }

  @Test
  public void testGetSupportedValues() {
    // Note: tests that empty values are filtered out
    ProfileConfiguration config = new StringValueProfileConfig(
        "testName", new String[] {"testValue", "anotherTestValue", ""});
    assertThat(
        config.getSupportedValues(),
        containsInAnyOrder("testValue", "anotherTestValue"));
  }
}
