/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.translator;

import static utam.compiler.translator.DefaultSourceConfiguration.DEFAULT_JSON_FILE_MASK_REGEX;
import static utam.core.declarative.translator.UnitTestRunner.validateUnitTestDirectory;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import java.net.URL;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import utam.compiler.translator.DefaultSourceConfiguration.RecursiveScanner;
import utam.compiler.translator.DefaultSourceConfiguration.ScannerConfig;
import utam.core.declarative.translator.ProfileConfiguration;
import utam.core.declarative.translator.TranslatorSourceConfig;
import utam.core.declarative.translator.TranslatorTargetConfig;
import utam.core.declarative.translator.UnitTestRunner;

/**
 * JSON based config for UTAM compiler
 *
 * @author elizaveta.ivanova
 * @since 234
 */
class JsonBasedCompilerConfig {

  static final String ERR_READING_COMPILER_CONFIG = "Error reading compiler config '%s'";
  private static final String DEFAULT_CONFIG_NAME = "utam.config";

  private final ModuleConfig moduleConfig;
  private final String configRoot = System.getProperty("user.dir");

  JsonBasedCompilerConfig(String configFileName) throws IOException {
    String jsonFileName = configFileName + ".json";
    try {
      URL url = getClass().getClassLoader().getResource(jsonFileName);
      if(url == null) {
        throw new IOException("File not found");
      }
      moduleConfig = new ObjectMapper().readValue(url, ModuleConfig.class);
    } catch (IOException e) {
      throw new IOException(String.format(ERR_READING_COMPILER_CONFIG, jsonFileName), e);
    }
  }

  JsonBasedCompilerConfig() throws IOException {
    this(DEFAULT_CONFIG_NAME);
  }

  List<ProfileConfiguration> getConfiguredProfiles() {
    return Stream.of(moduleConfig.profiles).map(Profile::getConfig).collect(Collectors.toList());
  }

  TranslatorTargetConfig getTargetConfig() {
    return moduleConfig.target.getTargetConfig(configRoot);
  }

  TranslatorSourceConfig getSourceConfig() {
    return moduleConfig.source.getSourceConfig(configRoot);
  }

  String getModuleName() {
    return moduleConfig.moduleName;
  }

  // config of one module, currently assumed one module per file
  static class ModuleConfig {

    final String moduleName;
    final Source source;
    final Target target;
    final Profile[] profiles;

    @JsonCreator
    ModuleConfig(
        @JsonProperty(value = "module") String moduleName,
        @JsonProperty(value = "source", required = true) Source source,
        @JsonProperty(value = "target", required = true) Target target,
        @JsonProperty(value = "profiles", defaultValue = "[]") Profile[] profiles
    ) {
      this.moduleName = moduleName;
      this.target = target;
      this.source = source;
      this.profiles = profiles == null ? new Profile[0] : profiles;
    }
  }

  // compiler sources including root folder (resources/spec), file mask and packages mapping
  static class Source {

    final String filesMaskRegex;
    final Map<String, String> packagesMap = new HashMap<>();
    final String pageObjectsRoot;

    @JsonCreator
    Source(
        @JsonProperty(value = "filesMask", defaultValue = DEFAULT_JSON_FILE_MASK_REGEX) String filesMaskRegex,
        @JsonProperty(value = "filesRoot", required = true) String pageObjectsRoot,
        @JsonProperty(value = "packages", required = true) Map<String, String> packagesMap
    ) {
      this.filesMaskRegex = filesMaskRegex == null ? DEFAULT_JSON_FILE_MASK_REGEX : filesMaskRegex;
      this.packagesMap.putAll(packagesMap);
      this.pageObjectsRoot = pageObjectsRoot;
    }

    TranslatorSourceConfig getSourceConfig(String configRoot) {
      RecursiveScanner scanner = new RecursiveScanner(configRoot + pageObjectsRoot);
      ScannerConfig scannerConfig = new ScannerConfig(filesMaskRegex, packagesMap);
      return new DefaultSourceConfiguration(scannerConfig, scanner);
    }
  }

  // compiler target: folder to put page objects, resources and unit tests
  static class Target {

    final String pageObjectsPath;
    final String resourcesHomePath;
    final String unitTestDirectory;
    final UnitTestRunner unitTestRunnerType;

    Target(
        @JsonProperty(value = "pageObjects", required = true) String pageObjectsPath,
        @JsonProperty(value = "resources", required = true) String resourcesHomePath,
        @JsonProperty(value = "unitTests") final String unitTestDirectory,
        @JsonProperty(value = "unitTestsRunner", defaultValue = "NONE") UnitTestRunner unitTestRunner

        ) {
      this.pageObjectsPath = pageObjectsPath;
      this.resourcesHomePath = resourcesHomePath;
      this.unitTestDirectory = unitTestDirectory;
      this.unitTestRunnerType = unitTestRunner == null? UnitTestRunner.NONE : unitTestRunner;
    }

    TranslatorTargetConfig getTargetConfig(String root) {
      return new DefaultTargetConfiguration(
          root + pageObjectsPath,
          root + resourcesHomePath,
          unitTestRunnerType,
          validateUnitTestDirectory(unitTestRunnerType, root + unitTestDirectory));
    }
  }

  // configured profile to process in JSON files
  static class Profile {

    final String name;
    final String[] values;

    Profile(
        @JsonProperty(value = "name", required = true) String name,
        @JsonProperty(value = "values", required = true) String[] values
    ) {
      this.name = name;
      this.values = values;
    }

    ProfileConfiguration getConfig() {
      return new StringValueProfileConfig(name, values);
    }
  }
}
