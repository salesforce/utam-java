/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.consumer;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static org.testng.Assert.expectThrows;
import static utam.core.framework.consumer.UtamLoaderConfigImpl.ERR_DUPLICATE_PROFILE;
import static utam.core.framework.context.StringValueProfile.DEFAULT_PROFILE;

import java.io.File;
import java.time.Duration;
import org.testng.annotations.Test;
import utam.core.driver.DriverTimeouts;
import utam.core.framework.base.PageObject;
import utam.core.framework.context.Profile;
import utam.core.framework.context.StringValueProfile;

public class UtamLoaderConfigTests {

  static UtamLoaderConfigImpl getDefaultConfig() {
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
  public void testDefaultJsonConfig() {
    UtamLoaderConfigImpl config = getDefaultConfig();
    assertThat(config.getModules().iterator().next(), is(nullValue()));
    assertThat(config.getConfiguredProfiles().size(), is(equalTo(1)));
    assertThat(config.getConfiguredProfiles().iterator().next(), is(equalTo(DEFAULT_PROFILE)));
    PageObject pageObject = config.getPageContext().getBean(TestLoaderConfigPageObject.class);
    assertThat(pageObject, is(instanceOf(TestLoaderConfigPageObjectOverride.class)));
  }

  @Test
  public void testSettingActiveProfile() {
    UtamLoaderConfigImpl config = getDefaultConfig();
    Profile testProfile = new StringValueProfile("test", "profiles");
    config.setProfile(testProfile);
    PageObject pageObject = config.getPageContext().getBean(
        TestLoaderConfigPageObject.class);
    assertThat(pageObject, is(instanceOf(TestLoaderConfigPageObjectOverride.class)));
  }

  @Test
  public void testCustomJSONConfig() {
    UtamLoaderConfig config = new UtamLoaderConfigImpl("module.loader.json");
    PageObject pageObject = config.getPageContext().getBean(TestLoaderConfigPageObject.class);
    // from default_impl_config
    assertThat(pageObject, is(instanceOf(TestLoaderConfigPageObjectOverride.class)));
  }

  @Test
  public void testCustomJSONConfigFromFile() {
    File file = new File(this.getClass().getClassLoader().getResource("module.loader.json").getFile());
    UtamLoaderConfig config = new UtamLoaderConfigImpl(file);
    PageObject pageObject = config.getPageContext().getBean(TestLoaderConfigPageObject.class);
    // from default_impl_config
    assertThat(pageObject, is(instanceOf(TestLoaderConfigPageObjectOverride.class)));
  }

  @Test
  public void testCustomJSONConfigSetProfile() {
    UtamLoaderConfig config = new UtamLoaderConfigImpl("module.loader.json");
    Profile testProfile = new StringValueProfile("test", "profiles");
    config.setProfile(testProfile);
    PageObject pageObject = config.getPageContext().getBean(TestLoaderConfigPageObject.class);
    assertThat(pageObject, is(instanceOf(TestLoaderConfigPageObjectProfile.class)));
  }

  @Test
  public void testSettingSameActiveProfileThrows() {
    UtamLoaderConfigImpl config = getDefaultConfig();
    Profile testProfile = new StringValueProfile("test", "profiles");
    config.setProfile(testProfile);
    UtamError e = expectThrows(UtamError.class, () -> config.setProfile(testProfile));
    assertThat(e.getMessage(),
        is(equalTo(String.format(ERR_DUPLICATE_PROFILE, "test", "profiles"))));
  }

  @Test
  public void testSettingSameConfiguredProfileThrows() {
    UtamLoaderConfigImpl config = getDefaultConfig();
    Profile testProfile = new StringValueProfile("test", "profiles");
    config.setConfiguredProfile(null, testProfile);
    UtamError e = expectThrows(UtamError.class,
        () -> config.setConfiguredProfile(null, testProfile));
    assertThat(e.getMessage(),
        is(equalTo(String.format(ERR_DUPLICATE_PROFILE, "test", "profiles"))));
  }

  @Test
  public void testConfiguredActiveProfile() {
    UtamLoaderConfigImpl config = getDefaultConfig();
    Profile testProfile = new StringValueProfile("test", "profiles");
    config.setConfiguredProfile(null, testProfile);
    config.setProfile(testProfile);
    assertThat(config.getConfiguredProfiles().size(), is(equalTo(2)));
  }

  @Test
  public void testNullConfig() {
    UtamLoaderConfigImpl config = getDefaultConfig();
    assertThat(config.getConfiguredProfiles(), hasSize(1));
    assertThat(config.getConfiguredProfiles().iterator().next(), is(equalTo(DEFAULT_PROFILE)));
    assertThat(config.getModules(), hasSize(1));
    assertThat(config.getModules().get(0), is(nullValue()));
  }

  @Test
  public void testDefaultConfigMissingThrows() {
    UtamLoaderConfig config = new UtamLoaderConfigImpl("module.loader.json");
    PageObjectContext context = config.getPageContext();
    UtamError e = expectThrows(UtamError.class,
        () -> context.getBean(TestLoaderConfigDefault.class));
    assertThat(e.getMessage(), containsString("can't find class"));
  }
}
