/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.consumer;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.hasItems;
import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.anEmptyMap;
import static org.hamcrest.Matchers.empty;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static org.testng.Assert.assertThrows;
import static org.testng.Assert.expectThrows;
import static utam.core.framework.consumer.JsonBasedLoader.ERR_READING_LOADER_CONFIG;
import static utam.core.framework.consumer.UtamLoaderConfigImpl.ERR_PROFILE_ALREADY_SET;
import static utam.core.framework.context.StringValueProfile.DEFAULT_PROFILE;

import java.time.Duration;
import org.testng.annotations.Test;
import utam.core.driver.DriverTimeouts;
import utam.core.framework.UtamCoreError;
import utam.core.framework.base.PageObject;
import utam.core.framework.context.Profile;
import utam.core.framework.context.StringValueProfile;

public class UtamLoaderConfigTests {

  private static UtamLoaderConfigImpl getDefaultConfig() {
    return new UtamLoaderConfigImpl();
  }

  @Test
  public void testSetBridgeApp() {
    UtamLoaderConfig config = getDefaultConfig();
    config.setBridgeAppTitle("bridge");
    assertThat(config.getDriverContext().getBridgeAppTitle(), is(equalTo("bridge")));
  }

  @Test
  public void testSetTimeout() {
    UtamLoaderConfig config = getDefaultConfig();
    Duration test = Duration.ofSeconds(100);
    config.setFindTimeout(test);
    config.setWaitForTimeout(test);
    config.setPollingInterval(test);
    assertThat(config.getDriverContext().getTimeouts(),
        is(equalTo(new DriverTimeouts(test, test, test))));
  }

  @Test
  public void testEmptyConfigOverrideThrows() {
    UtamLoaderConfig config = getDefaultConfig();
    assertThrows(() -> config.setProfile(null));
  }

  @Test
  public void testDefaultJsonConfigMissing() {
    UtamLoaderConfigImpl config = new UtamLoaderConfigImpl();
    assertThat(config.getModules().iterator().next(), is(nullValue()));
    assertThat(config.getConfiguredProfiles().size(), is(equalTo(1)));
    assertThat(config.getConfiguredProfiles().keySet().iterator().next(),
        is(equalTo(DEFAULT_PROFILE.getConfigName(null))));
    PageObject pageObject = config.getPageContext().getBean(TestLoaderConfigPageObject.class);
    assertThat(pageObject, is(instanceOf(TestLoaderConfigPageObjectOverride.class)));
  }

  @Test
  public void testJsonConfigMissingThrows() {
    UtamCoreError e = expectThrows(UtamCoreError.class, () -> new UtamLoaderConfigImpl("missing"));
    assertThat(e.getMessage(), is(equalTo(String.format(ERR_READING_LOADER_CONFIG, "missing.json"))));
  }

  @Test
  public void testSettingActiveProfile() {
    UtamLoaderConfigImpl config = new UtamLoaderConfigImpl();
    Profile testProfile = new StringValueProfile("test", "profiles");
    config.setProfile(testProfile);
    PageObject pageObject = config.getPageContext().getBean(
        TestLoaderConfigPageObject.class);
    assertThat(pageObject, is(instanceOf(TestLoaderConfigPageObjectOverride.class)));
  }

  @Test
  public void testCustomJSONConfig() {
    UtamLoaderConfig config = new UtamLoaderConfigImpl("module.loader");
    PageObject pageObject = config.getPageContext().getBean(TestLoaderConfigPageObject.class);
    // from default_impl_config
    assertThat(pageObject, is(instanceOf(TestLoaderConfigPageObjectOverride.class)));
  }

  @Test
  public void testCustomJSONConfigSetProfile() {
    UtamLoaderConfig config = new UtamLoaderConfigImpl("module.loader");
    Profile testProfile = new StringValueProfile("test", "profiles");
    config.setProfile(testProfile);
    PageObject pageObject = config.getPageContext().getBean(TestLoaderConfigPageObject.class);
    assertThat(pageObject, is(instanceOf(TestLoaderConfigPageObjectProfile.class)));
  }

  @Test
  public void testSettingSameActiveProfileThrows() {
    UtamLoaderConfigImpl config = new UtamLoaderConfigImpl();
    Profile testProfile = new StringValueProfile("test", "profiles");
    config.setProfile(testProfile);
    UtamError e = expectThrows(UtamError.class, () -> config.setProfile(testProfile));
    assertThat(e.getMessage(), is(equalTo(String.format(ERR_PROFILE_ALREADY_SET, "test_profiles_config"))));
  }

  @Test
  public void testSettingSameConfiguredProfileThrows() {
    UtamLoaderConfigImpl config = new UtamLoaderConfigImpl();
    Profile testProfile = new StringValueProfile("test", "profiles");
    config.setConfiguredProfile(null, testProfile);
    UtamError e = expectThrows(UtamError.class, () -> config.setConfiguredProfile(null, testProfile));
    assertThat(e.getMessage(), is(equalTo(String.format(ERR_PROFILE_ALREADY_SET, "test_profiles_config"))));
  }

  @Test
  public void testConfiguredActiveProfile() {
    UtamLoaderConfigImpl config = new UtamLoaderConfigImpl();
    Profile testProfile = new StringValueProfile("test", "profiles");
    config.setConfiguredProfile(null, testProfile);
    config.setProfile(testProfile);
    assertThat(config.getConfiguredProfiles().size(), is(equalTo(2)));
  }

  @Test
  public void testNullConfig() {
    UtamLoaderConfigImpl config = new UtamLoaderConfigImpl(DriverTimeouts.DEFAULT, null);
    assertThat(config.getConfiguredProfiles().keySet(), hasSize(1));
    assertThat(config.getConfiguredProfiles().keySet().iterator().next(),
        is(equalTo(DEFAULT_PROFILE.getConfigName(null))));
    assertThat(config.getModules(), hasSize(1));
    assertThat(config.getModules().get(0), is(nullValue()));
  }
}
