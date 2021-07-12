/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.translator;

import static utam.compiler.translator.DefaultSourceConfiguration.DEFAULT_JSON_FILE_MASK_REGEX;
import static utam.core.declarative.translator.UnitTestRunner.NONE;
import static utam.core.declarative.translator.UnitTestRunner.validateUnitTestDirectory;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import utam.compiler.UtamCompilationError;
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
public class JsonCompilerConfig {

  static final String ERR_READING_COMPILER_CONFIG = "Error reading compiler config '%s'";
  static final String ERR_TARGET_NOT_SET = "Target configuration is not set";
  private static final String DEFAULT_CONFIG_NAME = "utam.config.json";

  private final Module moduleConfig;
  private final String configRoot = System.getProperty("user.dir");

  // used in tests
  JsonCompilerConfig(String configFileName) throws IOException {
    try {
      URL url = getClass().getClassLoader().getResource(configFileName);
      moduleConfig = new ObjectMapper().readValue(url, Module.class);
    } catch (IOException | IllegalArgumentException e) {
      throw new IOException(String.format(ERR_READING_COMPILER_CONFIG, configFileName), e);
    }
  }

  // used by CLI runner
  JsonCompilerConfig(File configFile) throws IOException {
    try {
      moduleConfig = new ObjectMapper().readValue(configFile, Module.class);
    } catch (IOException e) {
      throw new IOException(String.format(ERR_READING_COMPILER_CONFIG, configFile.toString()), e);
    }
  }

  JsonCompilerConfig() throws IOException {
    this(DEFAULT_CONFIG_NAME);
  }

  List<ProfileConfiguration> getConfiguredProfiles() {
    return Stream.of(moduleConfig.profiles).map(Profile::getConfiguredProfile).collect(Collectors.toList());
  }

  TranslatorTargetConfig getTargetConfig() {
    return moduleConfig.getTargetConfig(configRoot);
  }

  public TranslatorSourceConfig getSourceConfig() {
    return moduleConfig.getSourceConfig(configRoot);
  }

  /**
   * Gets the name of the module.
   * @return the name of the module
   */
  public String getModuleName() {
    return moduleConfig.moduleName;
  }

  /**
   * Represents a module in a cloned repository.
   *
   * @author james.evans
   */
  public static class Module {

    final String moduleName;
    final String pageObjectsDirectory;
    final String pageObjectFileMask;
    final Namespace[] namespaces;
    final Profile[] profiles;
    final Target target;

    /**
     * Initializes a new instance of the Module class. Instantiated via JSON deserialization.
     *
     * @param moduleName           the arbitrary name of the module in the source repository
     * @param pageObjectsDirectory the directory in the source repository in which to recursively
     *                             search for UTAM Page Object declarative description files
     * @param namespaces           an array of Namespace objects describing the namespaces within
     *                             the module
     * @param profiles             an array of Profile objects representing the profiles used in
     *                             JSON files of the module
     */
    @JsonCreator
    public Module(
        @JsonProperty(value = "name", required = true) String moduleName,
        @JsonProperty(value = "filesMask", defaultValue = DEFAULT_JSON_FILE_MASK_REGEX) String filesMaskRegex,
        @JsonProperty(value = "pageObjectsDirectory", required = true) String pageObjectsDirectory,
        @JsonProperty(value = "target") Target target,
        @JsonProperty(value = "namespaces", required = true) Namespace[] namespaces,
        @JsonProperty(value = "profiles", required = true) Profile[] profiles
    ) {
      this.moduleName = moduleName;
      this.pageObjectsDirectory = pageObjectsDirectory;
      this.namespaces = namespaces;
      this.profiles = profiles;
      this.pageObjectFileMask =
          filesMaskRegex == null ? DEFAULT_JSON_FILE_MASK_REGEX : filesMaskRegex;
      this.target = target;
    }

    public TranslatorSourceConfig getSourceConfig(String configRoot) {
      RecursiveScanner scanner = new RecursiveScanner(configRoot + pageObjectsDirectory);
      Map<String,String> packages = new HashMap<>();
      Stream.of(namespaces)
          .forEach(namespace -> packages.put(namespace.typePrefix, namespace.pathMatchRegex));
      return new DefaultSourceConfiguration(new ScannerConfig(pageObjectFileMask, packages), scanner);
    }

    public TranslatorTargetConfig getTargetConfig(String root) {
      // target can be null when set from distribution repo!
      if(target == null) {
        throw new UtamCompilationError(ERR_TARGET_NOT_SET);
      }
      return new DefaultTargetConfiguration(
          root + target.pageObjectsPath,
          root + target.resourcesHomePath,
          target.unitTestRunnerType,
          target.unitTestDirectory);
    }
  }

  /**
   * Represents a profile configuration for a UTAM Page Object repository.
   *
   * @author james.evans
   */
  public static class Profile {

    private final String name;
    private final String[] values;

    /**
     * Initializes a new instance of the Profile class. Instantiated via JSON deserialization.
     * @param name the name of the profile
     * @param values the values of the profile
     */
    @JsonCreator
    public Profile(
        @JsonProperty(value = "name") String name,
        @JsonProperty(value = "values") String[] values) {
      this.name = name;
      this.values = values;
    }

    public ProfileConfiguration getConfiguredProfile() {
      return new StringValueProfileConfig(name, values);
    }
  }

  /**
   * Represents a namespace within a UTAM Page Object library.
   *
   * @author james.evans
   */
  public static class Namespace {

    private final String typePrefix;
    private final String pathMatchRegex;

    /**
     * Initializes a new instance of the Namespace class. Instantiated via JSON deserialization.
     * @param typePrefix     type prefix
     * @param pathMatchRegex the mapped name used by the UTAM compiler to generate packages for the generated
     *            Page Object source code
     */
    @JsonCreator
    public Namespace(
        @JsonProperty(value = "typePrefix", required = true) String typePrefix,
        @JsonProperty(value = "pathMatchRegex", required = true) String pathMatchRegex) {
      this.typePrefix = typePrefix;
      this.pathMatchRegex = pathMatchRegex;
    }

    /**
     * Gets the mapped name used by the UTAM compiler to generate packages for the generated Page
     * Object source code. public because used in distribution JSON config
     * @return the mapped name used by the UTAM compiler to generate packages for the generated Page
     *         Object source code
     */
    public String getTypePrefix() {
      return typePrefix;
    }

    /**
     * public because used in distribution JSON config
     * @return regex to use for scanning folders
     */
    public String getPathMatchRegex() {
      return pathMatchRegex;
    }
  }

  // compiler target: folder to put page objects, resources and unit tests
  public static class Target {

    final String pageObjectsPath;
    final String resourcesHomePath;
    final String unitTestDirectory;
    final UnitTestRunner unitTestRunnerType;

    public Target(
        @JsonProperty(value = "pageObjects", required = true) String pageObjectsPath,
        @JsonProperty(value = "resources", required = true) String resourcesHomePath,
        @JsonProperty(value = "unitTests") final String unitTestDirectory,
        @JsonProperty(value = "unitTestsRunner", defaultValue = "NONE") UnitTestRunner unitTestRunner
    ) {
      this.pageObjectsPath = pageObjectsPath;
      this.resourcesHomePath = resourcesHomePath;
      this.unitTestDirectory = validateUnitTestDirectory(unitTestRunner, unitTestDirectory);
      this.unitTestRunnerType = unitTestRunner == null? NONE : unitTestRunner;
    }

    // can be used by distribution repo
    public Target(String pageObjectsPath, String resourcesHomePath) {
      this(pageObjectsPath, resourcesHomePath, null, NONE);
    }
  }
}
