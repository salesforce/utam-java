/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.translator;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Properties;
import java.util.stream.Collectors;
import utam.compiler.lint.LintingConfigJson;
import utam.compiler.translator.DefaultSourceConfiguration.FilesScanner;
import utam.compiler.translator.DefaultSourceConfiguration.RecursiveScanner;
import utam.compiler.translator.DefaultSourceConfiguration.ScannerConfig;
import utam.core.declarative.errors.CompilerErrorsConfig;
import utam.core.declarative.lint.LintingConfig;
import utam.core.declarative.translator.ProfileConfiguration;
import utam.core.declarative.translator.TranslationTypesConfig;
import utam.core.declarative.translator.TranslatorConfig;
import utam.core.declarative.translator.TranslatorSourceConfig;
import utam.core.declarative.translator.TranslatorTargetConfig;

/**
 * The default configuration object for executing the generator in a runner class
 *
 * @author james.evans
 * @since 230
 */
public class DefaultTranslatorConfiguration implements TranslatorConfig {

  private final List<ProfileConfiguration> profileConfigurations = new ArrayList<>();
  private final TranslatorSourceConfig translatorSourceConfig;
  private final TranslationTypesConfig translatorTypesConfig;
  private final TranslatorTargetConfig translatorTargetConfig;
  private final LintingConfig lintingConfiguration;
  private final CompilerOutputOptions outputOptions;
  private final CompilerErrorsConfig compilerErrorsConfig;

  /**
   * Initializes a new instance of the translator configuration class
   *
   * @param outputOptions compiler output options
   * @param lintingConfiguration configured lint rules
   * @param typesConfig types provider config
   * @param sourceConfig configuration to scan for page object sources
   * @param targetConfig information about output folders for page objects, configs and unit tests
   * @param profileDefinitions list of known profiles and their values
   */
  DefaultTranslatorConfiguration(
      CompilerOutputOptions outputOptions,
      LintingConfig lintingConfiguration,
      CompilerErrorsConfig errorsConfig,
      TranslationTypesConfig typesConfig,
      TranslatorSourceConfig sourceConfig,
      TranslatorTargetConfig targetConfig,
      List<ProfileConfiguration> profileDefinitions) {
    this.outputOptions = outputOptions;
    this.translatorSourceConfig = sourceConfig;
    this.translatorTargetConfig = targetConfig;
    this.translatorTypesConfig = typesConfig;
    for (ProfileConfiguration profileDefinition : profileDefinitions) {
      setConfiguredProfile(profileDefinition);
    }
    this.lintingConfiguration = LintingConfigJson.getLintingConfig(lintingConfiguration);
    this.compilerErrorsConfig = errorsConfig;
  }

  /**
   * Initializes a new instance of the translator configuration class
   *
   * @param outputOptions compiler output options
   * @param lintingConfiguration configured lint rules
   * @param errorsConfig configuration to handle compiler errors
   * @param sourceConfig configuration to scan for page object sources
   * @param targetConfig information about output folders for page objects, configs and unit tests
   * @param profileDefinitions list of known profiles and their values
   */
  public DefaultTranslatorConfiguration(
      CompilerOutputOptions outputOptions,
      LintingConfig lintingConfiguration,
      CompilerErrorsConfig errorsConfig,
      TranslatorSourceConfig sourceConfig,
      TranslatorTargetConfig targetConfig,
      List<ProfileConfiguration> profileDefinitions) {
    this(
        outputOptions,
        lintingConfiguration,
        errorsConfig,
        new TranslationTypesConfigJava(),
        sourceConfig,
        targetConfig,
        profileDefinitions);
  }

  /**
   * Initializes a new instance of the translator configuration class
   *
   * @param outputOptions compiler output options
   * @param sourceConfig configuration to scan for page object sources
   * @param targetConfig information about output folders for page objects, configs and unit tests
   * @param profileDefinitions list of known profiles and their values
   */
  public DefaultTranslatorConfiguration(
      CompilerOutputOptions outputOptions,
      TranslatorSourceConfig sourceConfig,
      TranslatorTargetConfig targetConfig,
      List<ProfileConfiguration> profileDefinitions) {
    this(
        outputOptions,
        null,
        new CompilerErrors.Throws(),
        new TranslationTypesConfigJava(),
        sourceConfig,
        targetConfig,
        profileDefinitions);
  }

  /**
   * Initializes a new instance of the translator configuration class, only used in unit tests
   *
   * @param sourceConfig configuration to scan for page object sources
   * @param targetConfig information about output folders for page objects, configs and unit tests
   */
  // used in tests
  DefaultTranslatorConfiguration(
      TranslatorSourceConfig sourceConfig, TranslatorTargetConfig targetConfig) {
    this(
        CompilerOutputOptions.DEFAULT_COMPILER_OUTPUT_OPTIONS,
        sourceConfig,
        targetConfig,
        new ArrayList<>());
  }

  /**
   * get scanner based on input directory or files public because used in downstream projects
   *
   * @param inputDirectory the root input directory to be recursively searched for the *.utam.json
   *     Page Object description files
   * @param inputFiles the list of the *.utam.json Page Object description files to be generated
   * @return instance of a source configuration
   */
  public static RecursiveScanner getScanner(File inputDirectory, List<File> inputFiles) {
    if (inputFiles != null && !inputFiles.isEmpty()) {
      return new FilesScanner(inputFiles);
    } else {
      return new RecursiveScanner(inputDirectory.toString());
    }
  }

  /**
   * Read the package mapping file, and translate the properties therein into an in-memory map
   * public because used in downstream projects
   *
   * @param packageMapFile name of the file
   * @return configured packages for scanner
   * @throws IOException if packages config is missing
   */
  public static ScannerConfig getScannerConfig(File packageMapFile) throws IOException {
    InputStream packageInput = new FileInputStream(packageMapFile.toString());
    Properties packageProperties = new Properties();
    packageProperties.load(packageInput);
    return new ScannerConfig(
        packageProperties.entrySet().stream()
            .collect(
                Collectors.toMap(
                    entry -> entry.getKey().toString(), entry -> entry.getValue().toString())));
  }

  /**
   * read profiles config and translate into list of ProfileConfigurations public because used in
   * downstream projects
   *
   * @param profileDefinitionsFile profiles configuration file
   * @return list of configured profiles
   * @throws IOException if properties file does not exist
   */
  public static List<ProfileConfiguration> getConfiguredProfiles(File profileDefinitionsFile)
      throws IOException {
    if (profileDefinitionsFile == null) {
      return new ArrayList<>();
    }
    FileInputStream packageInput = new FileInputStream(profileDefinitionsFile.toString());
    Properties packageProperties = new Properties();
    packageProperties.load(packageInput);
    return packageProperties.entrySet().stream()
        .map(
            entry ->
                new StringValueProfileConfig(
                    entry.getKey().toString(), entry.getValue().toString().split(",")))
        .collect(Collectors.toList());
  }

  /**
   * Sets the configured profile, only used in profiles unit tests
   *
   * @param profileConfiguration the profile configuration to use
   */
  // used in profiles tests in another package, hence public
  public void setConfiguredProfile(ProfileConfiguration profileConfiguration) {
    this.profileConfigurations.add(profileConfiguration);
  }

  @Override
  public TranslatorSourceConfig getConfiguredSource() {
    return translatorSourceConfig;
  }

  @Override
  public TranslatorTargetConfig getConfiguredTarget() {
    return translatorTargetConfig;
  }

  @Override
  public Collection<ProfileConfiguration> getConfiguredProfiles() {
    return profileConfigurations;
  }

  @Override
  public TranslationTypesConfig getTranslationTypesConfig() {
    return translatorTypesConfig;
  }

  @Override
  public String getModuleName() {
    return outputOptions.moduleName;
  }

  @Override
  public String getPageObjectsVersion() {
    return outputOptions.pageObjectsVersion;
  }

  @Override
  public List<String> getCopyright() {
    return outputOptions.configuredCopyright;
  }

  @Override
  public LintingConfig getLintingConfig() {
    return lintingConfiguration;
  }

  @Override
  public CompilerErrorsConfig getErrorsConfig() {
    return compilerErrorsConfig;
  }

  /**
   * helper class to collect parameters related to page object output
   *
   * @author elizaveta.ivanova
   * @since 238
   */
  public static class CompilerOutputOptions {

    /** used in utam-core-util in consumer, so should be public */
    public static final CompilerOutputOptions DEFAULT_COMPILER_OUTPUT_OPTIONS =
        new CompilerOutputOptions("", "", new ArrayList<>());

    final String moduleName;
    final String pageObjectsVersion;
    final List<String> configuredCopyright;

    /**
     * @param moduleName name of the module with page object sources
     * @param pageObjectsVersion version of page objects to add to javadoc
     * @param configuredCopyright copyright lines from compiler config
     */
    public CompilerOutputOptions(
        String moduleName, String pageObjectsVersion, List<String> configuredCopyright) {
      this.moduleName = moduleName;
      this.pageObjectsVersion = pageObjectsVersion;
      this.configuredCopyright = configuredCopyright;
    }
  }
}
