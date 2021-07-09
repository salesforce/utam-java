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
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Properties;
import java.util.stream.Collectors;
import utam.compiler.translator.DefaultSourceConfiguration.FilesScanner;
import utam.compiler.translator.DefaultSourceConfiguration.RecursiveScanner;
import utam.compiler.translator.DefaultSourceConfiguration.ScannerConfig;
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
  private final String moduleName;

  /**
   * Initializes a new instance of the translator configuration class
   *
   * @param moduleName         name of the module with page object sources
   * @param typesConfig        types provider config
   * @param sourceConfig       configuration to scan for page object sources
   * @param targetConfig       information about output folders for page objects, configs and unit
   *                           tests
   * @param profileDefinitions list of known profiles and their values
   */
  DefaultTranslatorConfiguration(
      String moduleName,
      TranslationTypesConfig typesConfig,
      TranslatorSourceConfig sourceConfig,
      TranslatorTargetConfig targetConfig,
      List<ProfileConfiguration> profileDefinitions) {
    this.moduleName = moduleName;
    this.translatorSourceConfig = sourceConfig;
    this.translatorTargetConfig = targetConfig;
    this.translatorTypesConfig = typesConfig;
    for (ProfileConfiguration profileDefinition : profileDefinitions) {
      setConfiguredProfile(profileDefinition);
    }
  }

  public DefaultTranslatorConfiguration(
      String moduleName,
      TranslatorSourceConfig sourceConfig,
      TranslatorTargetConfig targetConfig,
      List<ProfileConfiguration> profileDefinitions) {
    this(moduleName, new TranslationTypesConfigJava(), sourceConfig, targetConfig, profileDefinitions);
  }

  // used in tests
  DefaultTranslatorConfiguration(
      TranslatorSourceConfig sourceConfig,
      TranslatorTargetConfig targetConfig) {
    this("", new TranslationTypesConfigJava(), sourceConfig, targetConfig, new ArrayList<>());
  }

  /**
   * @param inputDirectory the root input directory to be recursively searched for the *.utam.json
   *                       Page Object description files
   * @param inputFiles     the list of the *.utam.json Page Object description files to be
   *                       generated
   * @return instance of a source configuration
   */
  static RecursiveScanner getScanner(File inputDirectory, List<File> inputFiles) {
    if (inputFiles != null && inputFiles.size() > 0) {
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
    FileInputStream packageInput = new FileInputStream(packageMapFile.toString());
    Properties packageProperties = new Properties();
    packageProperties.load(packageInput);
    return new ScannerConfig(packageProperties.entrySet().stream()
        .collect(Collectors.toMap(
            entry -> entry.getKey().toString(),
            entry -> entry.getValue().toString())));
  }

  /**
   * read profiles config and translate into list of ProfileConfigurations
   * public because used in downstream projects
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
        .map(entry -> new StringValueProfileConfig(entry.getKey().toString(),
            entry.getValue().toString().split(",")))
        .collect(Collectors.toList());
  }

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
    return moduleName;
  }
}
