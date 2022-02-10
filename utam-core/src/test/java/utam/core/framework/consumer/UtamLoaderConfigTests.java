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
import static org.hamcrest.Matchers.is;
import static org.mockito.Mockito.mock;
import static org.testng.Assert.expectThrows;

import java.io.File;
import java.time.Duration;
import org.openqa.selenium.WebDriver;
import org.testng.annotations.Test;
import utam.core.framework.base.PageObject;
import utam.core.framework.consumer.impl.TestLoaderConfigPageObjectImpl;
import utam.core.framework.context.Profile;
import utam.core.framework.context.StringValueProfile;
import utam.core.selenium.element.DriverAdapter;

public class UtamLoaderConfigTests {

  static UtamLoaderConfigImpl getDefaultConfig() {
    return new UtamLoaderConfigImpl();
  }
  private static final Profile TEST_PROFILE = new StringValueProfile("test", "profiles");

  @Test
  public void testSetBridgeApp() {
    UtamLoaderConfig config = getDefaultConfig();
    config.setBridgeAppTitle("bridge");
    assertThat(config.getDriverConfig().getBridgeAppTitle(), is(equalTo("bridge")));
  }

  @Test
  public void testSetTimeout() {
    UtamLoaderConfig config = getDefaultConfig();
    config.setImplicitTimeout(Duration.ofSeconds(1));
    assertThat(config.getDriverConfig().getImplicitTimeout(), is(Duration.ofSeconds(1)));
    config.setExplicitTimeout(Duration.ofSeconds(2));
    assertThat(config.getDriverConfig().getExplicitTimeout(), is(Duration.ofSeconds(2)));
    config.setPollingInterval(Duration.ofSeconds(3));
    assertThat(config.getDriverConfig().getPollingInterval(), is(Duration.ofSeconds(3)));
  }

  @Test
  public void testSettingActiveProfile() {
    UtamLoaderConfigImpl config = getDefaultConfig();
    config.setProfile(TEST_PROFILE);
    PageObject pageObject = config.getPageContext().getBean(
        TestLoaderConfigPageObject.class);
    assertThat(pageObject, is(instanceOf(TestLoaderConfigPageObjectOverride.class)));
  }

  @Test
  public void testCustomJSONConfig() {
    UtamLoaderConfig config = new UtamLoaderConfigImpl("module.loader.json");
    PageObject pageObject = config.getPageContext().getBean(TestLoaderConfigPageObject.class);
    assertThat(pageObject, is(instanceOf(TestLoaderConfigPageObjectImpl.class)));
  }

  @Test
  public void testCustomJSONConfigFromFile() {
    File file = new File(this.getClass().getClassLoader().getResource("module.loader.json").getFile());
    UtamLoaderConfig config = new UtamLoaderConfigImpl(file);
    PageObject pageObject = config.getPageContext().getBean(TestLoaderConfigPageObject.class);
    assertThat(pageObject, is(instanceOf(TestLoaderConfigPageObjectImpl.class)));
  }

  @Test
  public void testCustomJSONConfigSetProfile() {
    UtamLoaderConfig config = new UtamLoaderConfigImpl("module.loader.json");
    config.setProfile(TEST_PROFILE);
    PageObject pageObject = config.getPageContext().getBean(TestLoaderConfigPageObject.class);
    assertThat(pageObject, is(instanceOf(TestLoaderConfigPageObjectProfile.class)));
  }

  @Test
  public void testSettingSameActiveProfileDifferentValue() {
    UtamLoaderConfigImpl config = getDefaultConfig();
    config.setProfile(TEST_PROFILE);
    config.setProfile(new StringValueProfile("test", "profile2"));
    config.getPageContext();
  }

  @Test
  public void testDefaultConfigMissingThrows() {
    UtamLoaderConfig config = new UtamLoaderConfigImpl("module.loader.json");
    PageObjectContext context = config.getPageContext();
    UtamError e = expectThrows(UtamError.class,
        () -> context.getBean(TestLoaderConfigDefault.class));
    assertThat(e.getMessage(), containsString("can't find class"));
  }

  @Test
  public void testProfileConfigPickedUpAfterReset() {
    UtamLoaderConfig config = new UtamLoaderConfigImpl();
    config.setDependencyModule("module1");
    UtamLoader utamLoader = new UtamLoaderImpl(config,
        new DriverAdapter(mock(WebDriver.class), null));
    PageObject instance = config.getPageContext().getBean(TestLoaderConfigPageObject.class);
    // first load default class
    assertThat(instance, is(instanceOf(TestLoaderConfigPageObjectImpl.class)));
    config.setProfile(TEST_PROFILE);
    // this line reloads config and overrides previous
    utamLoader.resetContext();
    instance = utamLoader.create(TestLoaderConfigPageObject.class);
    assertThat(instance, is(instanceOf(TestLoaderConfigPageObjectProfile.class)));
  }
}
