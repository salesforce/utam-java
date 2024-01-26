/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.consumer;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.hasSize;
import static org.testng.Assert.expectThrows;
import static utam.core.framework.consumer.JsonLoaderConfig.Deserializer.ERR_READING_LOADER_CONFIG;
import static utam.core.framework.consumer.JsonLoaderConfig.DeserializerFromUrl.ERR_CANT_FIND_LOADER_CONFIG;
import static utam.core.framework.consumer.JsonLoaderConfig.loadConfig;

import org.testng.annotations.Test;
import utam.core.framework.consumer.JsonLoaderConfig.DeserializerFromUrl;

/**
 * @author elizaveta.ivanova
 * @since 234
 */
public class JsonLoaderConfigTests {

  private static JsonLoaderConfig getConfig(String resource) {
    return loadConfig(new DeserializerFromUrl(resource));
  }

  @Test
  public void testValidLoaderConfig() {
    JsonLoaderConfig config = getConfig("loaderconfig/test_loader_config.json");
    assertThat(config.getInjectionConfigs(), hasSize(2));
    assertThat(
        config.getInjectionConfigs(),
        containsInAnyOrder("module1.config.json", "module2.config.json"));
  }

  @Test
  public void testValidLoaderConfigWithDuplicates() {
    JsonLoaderConfig config = getConfig("loaderconfig/test_duplicate_config.json");
    assertThat(config.getInjectionConfigs(), hasSize(2));
    assertThat(
        config.getInjectionConfigs(),
        containsInAnyOrder("module1.config.json", "module2.config.json"));
  }

  @Test
  public void testEmptyLoaderConfig() {
    JsonLoaderConfig config = getConfig("loaderconfig/test_empty_config.json");
    assertThat(config.getInjectionConfigs(), hasSize(0));
  }

  @Test
  public void testEmptyLoaderConfigConstructor() {
    JsonLoaderConfig config = new JsonLoaderConfig();
    assertThat(config.getInjectionConfigs(), hasSize(0));
    config.setInjectionConfigFile("module");
    assertThat(config.getInjectionConfigs(), containsInAnyOrder("module.config.json"));
  }

  @Test
  public void testJsonUnknownPropertiesThrow() {
    Exception e =
        expectThrows(
            RuntimeException.class, () -> getConfig("loaderconfig/test_incorrect_config.json"));
    assertThat(e.getMessage(), containsString(ERR_READING_LOADER_CONFIG));
    assertThat(e.getCause().getMessage(), containsString("Unrecognized field \"modules\""));
  }

  @Test
  public void testNonExistingLoaderConfigThrows() {
    final String config = "i.do.not.exist";
    UtamError e = expectThrows(UtamError.class, () -> getConfig(config));
    assertThat(e.getMessage(), containsString(ERR_CANT_FIND_LOADER_CONFIG));
  }
}
