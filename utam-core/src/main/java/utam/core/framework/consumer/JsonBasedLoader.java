/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.framework.consumer;

import static utam.core.framework.consumer.UtamLoaderConfigImpl.DEFAULT_LOADER_CONFIG;
import static utam.core.framework.context.StringValueProfile.DEFAULT_PROFILE;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Stream;
import utam.core.framework.UtamCoreError;
import utam.core.framework.context.StringValueProfile;

/**
 * JSON config for UTAM Loader
 *
 * @author elizaveta.ivanova
 * @since 234
 */
class JsonBasedLoader {

  static final String ERR_READING_LOADER_CONFIG = "error while reading config '%s' for UTAM loader";
  private static final Config EMPTY = new Config(new String[0], new ArrayList<>());
  private final Config config;

  JsonBasedLoader(String configFileName) {
    String jsonFileName = configFileName + ".json";
    URL url = getClass().getClassLoader().getResource(jsonFileName);
    if (url == null) {
      if (configFileName.startsWith(DEFAULT_LOADER_CONFIG)) {
        config = EMPTY;
      } else {
        throw new UtamCoreError(String.format(ERR_READING_LOADER_CONFIG, jsonFileName));
      }
    } else {
      try {
        config = new ObjectMapper().readValue(url, Config.class);
      } catch (IOException e) {
        throw new UtamCoreError(String.format(ERR_READING_LOADER_CONFIG, jsonFileName), e);
      }
    }
  }

  List<String> getModules() {
    List<String> allModules = new ArrayList<>();
    // in case some modules were not using module name
    allModules.add(null);
    allModules.addAll(Arrays.asList(config.modules));
    return allModules;
  }

  List<utam.core.framework.context.Profile> getConfiguredProfiles() {
    List<utam.core.framework.context.Profile> allProfiles = new ArrayList<>();
    allProfiles.add(DEFAULT_PROFILE);
    config.profiles
        .forEach(profile -> Stream.of(profile.values)
            .forEach(str -> allProfiles.add(new StringValueProfile(profile.name, str))));
    return allProfiles;
  }

  static class Config {

    private final String[] modules;
    private final List<Profile> profiles;

    @JsonCreator
    Config(
        @JsonProperty(value = "modules", required = true) String[] modules,
        @JsonProperty(value = "profiles", required = true) List<Profile> profiles) {
      this.modules = modules;
      this.profiles = profiles;
    }
  }

  // configured profile to process in JSON files
  static class Profile {

    final String name;
    final String[] values;

    @JsonCreator
    Profile(
        @JsonProperty(value = "name") String name,
        @JsonProperty(value = "values") String[] values) {
      this.name = name;
      this.values = values;
    }
  }
}
