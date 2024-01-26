/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.translator;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.nullValue;

import org.testng.annotations.Test;
import utam.core.declarative.translator.ProfileConfiguration;
import utam.core.framework.context.Profile;
import utam.core.framework.context.StringValueProfile;

public class StringValueProfileConfigTests {

  @Test
  public void testStringValueProfileConfig() {
    assertThat(new StringValueProfileConfig("testName", "testValue"), is(not(nullValue())));
  }

  @Test
  public void testStringValueProfileConfigFromProfiles() {
    Profile profile = new StringValueProfile("testName", "testValue");
    assertThat(new StringValueProfileConfig("name", profile), is(not(nullValue())));
  }

  @Test
  public void testGetFromString() {
    ProfileConfiguration config =
        new StringValueProfileConfig("testName", new String[] {"testValue", "anotherTestValue"});
    Profile profile = config.getFromString("testValue");
    assertThat(profile.getName(), is(equalTo("testName")));
    assertThat(profile.getValue(), is(equalTo("testValue")));
  }

  @Test
  public void testGetPropertyKey() {
    ProfileConfiguration config = new StringValueProfileConfig("testName", "testValue");
    assertThat(config.getPropertyKey(), is(equalTo("testName")));
  }

  @Test
  public void testGetSupportedValues() {
    ProfileConfiguration config =
        new StringValueProfileConfig("testName", new String[] {"testValue", "anotherTestValue"});
    assertThat(config.getSupportedValues(), containsInAnyOrder("testValue", "anotherTestValue"));
  }

  @Test
  public void testEqualsOverride() {
    StringValueProfileConfig value = new StringValueProfileConfig("testName", "testValue");
    assertThat(value.equals(1), is(false));
    assertThat(new StringValueProfileConfig("testName", "testValue1").equals(value), is(false));
    assertThat(value.equals(value), is(true));
  }
}
