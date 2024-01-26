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
import static org.hamcrest.Matchers.emptyString;
import static org.hamcrest.Matchers.is;
import static org.testng.Assert.expectThrows;
import static utam.core.framework.consumer.PageObjectContextImpl.ERR_GET_IMPL_BY_NAME;

import java.io.File;
import java.time.Duration;
import java.util.Objects;
import org.testng.annotations.Test;
import utam.core.framework.base.PageObject;
import utam.core.framework.consumer.impl.TestLoaderConfigPageObjectImpl;
import utam.core.framework.context.StringValueProfile;

public class UtamLoaderConfigTests {

  private static final String VALID_LOADER_CONFIG_PATH = "loaderconfig/test_loader_config.json";

  static UtamLoaderConfigImpl getDefaultConfig() {
    return new UtamLoaderConfigImpl();
  }

  @Test
  public void testSetBridgeApp() {
    UtamLoaderConfig config = getDefaultConfig();
    assertThat(config.getDriverConfig().getBridgeAppTitle(), is(emptyString()));
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
  public void testCustomJSONConfig() {
    UtamLoaderConfig config = new UtamLoaderConfigImpl(VALID_LOADER_CONFIG_PATH);
    PageObject pageObject = config.getPageContext().getBean(TestLoaderConfigPageObject.class);
    assertThat(pageObject, is(instanceOf(TestLoaderConfigPageObjectProfile.class)));
  }

  @Test
  public void testCustomJSONConfigFromFile() {
    File file =
        new File(
            Objects.requireNonNull(
                    this.getClass().getClassLoader().getResource(VALID_LOADER_CONFIG_PATH))
                .getFile());
    UtamLoaderConfig config = new UtamLoaderConfigImpl(file);
    PageObject pageObject = config.getPageContext().getBean(TestLoaderConfigPageObject.class);
    assertThat(pageObject, is(instanceOf(TestLoaderConfigPageObjectProfile.class)));
  }

  @Test
  public void testSettingSameActiveProfileDifferentValue() {
    UtamLoaderConfigImpl config = new UtamLoaderConfigImpl(VALID_LOADER_CONFIG_PATH);

    config.setProfile(new StringValueProfile("custom", "one"));
    PageObject pageObject = config.getPageContext().getBean(TestLoaderConfigPageObject.class);
    assertThat(pageObject, is(instanceOf(TestLoaderConfigPageObjectOverride.class)));

    config.setProfile(new StringValueProfile("custom", "two"));
    pageObject = config.getPageContext().getBean(TestLoaderConfigPageObject.class);
    assertThat(pageObject, is(instanceOf(TestLoaderConfigPageObjectImpl.class)));

    config.setProfile(new StringValueProfile("custom", "three"));
    // default is picked up
    pageObject = config.getPageContext().getBean(TestLoaderConfigPageObject.class);
    assertThat(pageObject, is(instanceOf(TestLoaderConfigPageObjectProfile.class)));
  }

  @Test
  public void testDefaultConfigMissingThrows() {
    UtamLoaderConfig config =
        new UtamLoaderConfigImpl("loaderconfig/test_one_module_loader_config.json");
    PageObjectContext context = config.getPageContext();
    UtamError e =
        expectThrows(UtamError.class, () -> context.getBean(TestLoaderConfigDefault.class));
    String error = String.format(ERR_GET_IMPL_BY_NAME, TestLoaderConfigDefault.class.getName());
    assertThat(e.getMessage(), containsString(error));
  }

  @Test
  public void testDefaultConfigMergedFromTwoModules() {
    UtamLoaderConfigImpl config = new UtamLoaderConfigImpl(VALID_LOADER_CONFIG_PATH);
    PageObjectContext context = config.getPageContext();
    PageObject bean1 = context.getBean(TestLoaderConfigDefault.class);
    assertThat(bean1, is(instanceOf(TestLoaderConfigPageObjectProfile.class)));
    PageObject bean2 = context.getBean(TestLoaderConfigPageObject.class);
    assertThat(bean2, is(instanceOf(TestLoaderConfigPageObjectProfile.class)));
  }
}
