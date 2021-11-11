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
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.startsWith;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.emptyIterable;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static org.testng.Assert.expectThrows;
import static utam.core.driver.DriverConfig.DEFAULT_EXPLICIT_TIMEOUT;
import static utam.core.driver.DriverConfig.DEFAULT_IMPLICIT_TIMEOUT;
import static utam.core.driver.DriverConfig.DEFAULT_POLLING_INTERVAL;
import static utam.core.framework.consumer.JsonLoaderConfig.ERR_READING_LOADER_CONFIG;
import static utam.core.framework.consumer.JsonLoaderConfig.loadConfig;
import static utam.core.framework.consumer.UtamLoaderConfigImpl.ERR_DUPLICATE_PROFILE;
import static utam.core.framework.context.StringValueProfile.DEFAULT_PROFILE;

import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.File;
import java.io.IOException;
import java.io.StringWriter;
import java.io.Writer;
import java.time.Duration;
import java.util.ArrayList;
import org.testng.annotations.Test;
import utam.core.driver.DriverConfig;
import utam.core.framework.consumer.JsonLoaderConfig.Module;
import utam.core.framework.consumer.JsonLoaderConfig.Profile;
import utam.core.framework.context.StringValueProfile;

/**
 * @author elizaveta.ivanova
 * @since 234
 */
public class JsonLoaderConfigTests {

  private static JsonLoaderConfig getEmptyConfig() {
    return new JsonLoaderConfig();
  }

  private static JsonLoaderConfig fromResource(String name) {
    return loadConfig(name);
  }

  @Test
  public void testExistingJson() {
    JsonLoaderConfig config = fromResource("module.loader.json");
    assertThat(config.getModules(), hasSize(3));
    StringValueProfile sharedProfile = new StringValueProfile("platform", "ios");

    Module first = config.getModules().get(0);
    assertThat(first.getName(), is(nullValue()));
    assertThat(first.getRawProfiles(), is(emptyIterable()));
    assertThat(first.getModuleProfiles(config.profiles), hasSize(2));
    assertThat(first.getModuleProfiles(config.profiles), hasItems(DEFAULT_PROFILE, sharedProfile));

    Module second = config.getModules().get(1);
    assertThat(second.getName(), is(equalTo("module1")));
    assertThat(second.getModuleProfiles(config.profiles), hasSize(4));
    assertThat(second.getModuleProfiles(config.profiles), hasItems(DEFAULT_PROFILE,
        new StringValueProfile("custom", "one"),
        new StringValueProfile("custom", "two"),
        sharedProfile));

    Module third = config.getModules().get(2);
    assertThat(third.getName(), is(equalTo("module2")));
    assertThat(third.getModuleProfiles(config.profiles), hasSize(2));
    assertThat(third.getModuleProfiles(config.profiles), hasItems(DEFAULT_PROFILE, sharedProfile));

    assertThat(config.profiles, hasSize(1));
  }

  @Test
  public void testIncorrectJsonThrows() {
    UtamError e = expectThrows(UtamError.class, () -> JsonLoaderConfig.loadConfig("incorrect.loader.json"));
    assertThat(e.getMessage(), startsWith("error while reading config"));
  }

  @Test
  public void testWrongFormatThrows() {
    UtamError e = expectThrows(UtamError.class, () -> loadConfig(new File("incorrect")));
    assertThat(e.getMessage(),
        is(equalTo(String.format(ERR_READING_LOADER_CONFIG, "incorrect"))));
  }

  @Test
  public void testDuplicateSharedProfiles() {
    JsonLoaderConfig config = getEmptyConfig();
    Profile profile = new Profile("same", new String[]{"name"});
    config.profiles.add(profile);
    config.profiles.add(profile);
    Module module = new Module("name", new ArrayList<>());
    config.getModules().add(module);
    UtamError e = expectThrows(UtamError.class, () -> config.getModules().get(0).getModuleProfiles(config.profiles));
    assertThat(e.getMessage(),
        is(equalTo(String.format(ERR_DUPLICATE_PROFILE, "same", "name"))));
  }

  @Test
  public void testDuplicateProfilesInModule() {
    Module module = new Module("name", new ArrayList<>());
    Profile profile = new Profile("same", new String[]{"name"});
    module.profiles.add(profile);
    module.profiles.add(profile);
    UtamError e = expectThrows(UtamError.class, () -> module.getModuleProfiles(new ArrayList<>()));
    assertThat(e.getMessage(),
        is(equalTo(String.format(ERR_DUPLICATE_PROFILE, "same", "name"))));
  }

  @Test
  public void testDuplicateProfilesInLoader() {
    JsonLoaderConfig config = getEmptyConfig();
    Profile profile = new Profile("same", new String[]{"name"});
    config.profiles.add(profile);
    Module module = new Module("module", new ArrayList<>());
    module.profiles.add(profile);
    config.getModules().add(module);
    UtamError e = expectThrows(UtamError.class,
        () -> new UtamLoaderConfigImpl(config));
    assertThat(e.getMessage(),
        is(equalTo(String.format(ERR_DUPLICATE_PROFILE, "same", "name"))));
  }

  @Test
  public void testSerializationOfModule() throws IOException {
    ObjectMapper mapper = new ObjectMapper();
    Writer writer = new StringWriter();
    mapper.writeValue(writer, new Module("name", new ArrayList<>()));
    assertThat(writer.toString(), is(equalTo("{\"name\":\"name\",\"profiles\":[]}")));
  }

  @Test
  public void testExistingJsonWithEmptyTimeouts() {
    JsonLoaderConfig config = fromResource("empty_timeouts.loader.json");
    assertThat(config.getModules(), hasSize(1));
    DriverConfig driverConfig = config.driverConfig;
    assertThat(driverConfig, is(notNullValue()));
    assertThat(driverConfig.getImplicitTimeout(), is(DEFAULT_IMPLICIT_TIMEOUT));
    assertThat(driverConfig.getExplicitTimeout(), is(DEFAULT_EXPLICIT_TIMEOUT));
    assertThat(driverConfig.getPollingInterval(), is(DEFAULT_POLLING_INTERVAL));
  }

  @Test
  public void testExistingJsonWithTimeouts() {
    JsonLoaderConfig config = fromResource("timeouts.loader.json");
    DriverConfig driverConfig = config.driverConfig;
    assertThat(driverConfig, is(notNullValue()));
    assertThat(driverConfig.getImplicitTimeout(), is(Duration.ofMillis(1)));
    assertThat(driverConfig.getExplicitTimeout(), is(Duration.ofMillis(2)));
    assertThat(driverConfig.getPollingInterval(), is(Duration.ofMillis(3)));
  }

  @Test
  public void testExistingJsonDefaultTimeouts() {
    JsonLoaderConfig config = fromResource("module.loader.json");
    DriverConfig driverConfig = config.driverConfig;
    assertThat(driverConfig, is(notNullValue()));
    assertThat(driverConfig.getImplicitTimeout(), is(DEFAULT_IMPLICIT_TIMEOUT));
    assertThat(driverConfig.getExplicitTimeout(), is(DEFAULT_EXPLICIT_TIMEOUT));
    assertThat(driverConfig.getPollingInterval(), is(DEFAULT_POLLING_INTERVAL));
  }
}
