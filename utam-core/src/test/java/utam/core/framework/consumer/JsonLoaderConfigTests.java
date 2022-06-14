/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.consumer;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.emptyString;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static org.testng.Assert.expectThrows;
import static utam.core.driver.DriverConfig.DEFAULT_EXPLICIT_TIMEOUT;
import static utam.core.driver.DriverConfig.DEFAULT_IMPLICIT_TIMEOUT;
import static utam.core.driver.DriverConfig.DEFAULT_POLLING_INTERVAL;
import static utam.core.framework.consumer.JsonLoaderConfig.ERR_CANT_FIND_LOADER_CONFIG;
import static utam.core.framework.consumer.JsonLoaderConfig.ERR_READING_LOADER_CONFIG;
import static utam.core.framework.consumer.JsonLoaderConfig.loadConfig;

import java.time.Duration;
import org.testng.annotations.Test;

/**
 * @author elizaveta.ivanova
 * @since 234
 */
public class JsonLoaderConfigTests {

  @Test
  public void testValidLoaderConfig() {
    JsonLoaderConfig config = loadConfig("loaderconfig/test_loader_config.json");
    assertThat(config.modules, hasSize(2));
    assertThat(config.modules, containsInAnyOrder("module1", "module2"));
    assertThat(config.bridgeAppTitle, is(equalTo("salesforce")));
    assertThat(config.explicitTimeout, is(equalTo(Duration.ofSeconds(60))));
    assertThat(config.implicitTimeout, is(equalTo(Duration.ZERO)));
    assertThat(config.pollingInterval, is(equalTo(Duration.ofMillis(200))));
  }

  @Test
  public void testEmptyLoaderConfig() {
    JsonLoaderConfig config = loadConfig("loaderconfig/test_empty_config.json");
    assertThat(config.modules, hasSize(0));
    assertThat(config.bridgeAppTitle, is(emptyString()));
    assertThat(config.explicitTimeout, is(equalTo(DEFAULT_EXPLICIT_TIMEOUT)));
    assertThat(config.implicitTimeout, is(equalTo(DEFAULT_IMPLICIT_TIMEOUT)));
    assertThat(config.pollingInterval, is(equalTo(DEFAULT_POLLING_INTERVAL)));
  }

  @Test
  public void testEmptyLoaderConfigConstructor() {
    JsonLoaderConfig config = new JsonLoaderConfig();
    assertThat(config.modules, hasSize(0));
    assertThat(config.bridgeAppTitle, is(emptyString()));
    assertThat(config.explicitTimeout, is(equalTo(DEFAULT_EXPLICIT_TIMEOUT)));
    assertThat(config.implicitTimeout, is(equalTo(DEFAULT_IMPLICIT_TIMEOUT)));
    assertThat(config.pollingInterval, is(equalTo(DEFAULT_POLLING_INTERVAL)));
  }

  @Test
  public void testIncorrectJsonThrows() {
    UtamError e = expectThrows(UtamError.class,
        () -> JsonLoaderConfig.loadConfig("loaderconfig/test_incorrect_config.json"));
    assertThat(e.getMessage(), containsString(ERR_READING_LOADER_CONFIG));
  }

  @Test
  public void testNonExistingLoaderConfigThrows() {
    final String config = "i.do.not.exist";
    UtamError e = expectThrows(UtamError.class, () -> loadConfig(config));
    assertThat(e.getMessage(), containsString(ERR_CANT_FIND_LOADER_CONFIG));
  }
}
