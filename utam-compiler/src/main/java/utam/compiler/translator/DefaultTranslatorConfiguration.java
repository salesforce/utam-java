/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.translator;

import utam.core.declarative.translator.ProfileConfiguration;
import utam.core.declarative.translator.UnitTestRunner;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.stream.Collectors;

/**
 * The default configuration object for executing the generator in a runner class
 *
 * @author james.evans
 *
 */
public class DefaultTranslatorConfiguration extends AbstractTranslatorConfiguration {

  /**
   * Initializes a new instance of the translator configuration class
   * @param inputDirectory the root input directory to be recursively searched for the *.utam.json
   *                       Page Object description files
   * @param outputDirectory the root output directory where the generated Page Object source files
   *                        will be written
   * @param unitTestDirectory the root output directory where generated unit tests for generated
   *                          Page Objects will be written
   * @param testRunner the test runner to use when generating unit tests
   * @param packageMap the mapping between directories containing the description files and
   *                   directories containing the generated source files
   * @param profileConfigDirectory the output directory in which to write profile information
   * @param profileDefinitions the map defining the list of known profiles and their values
   * @throws IOException if there are any issues reading or writing files
   */
  public DefaultTranslatorConfiguration(
      String inputDirectory,
      String outputDirectory,
      String unitTestDirectory,
      String testRunner,
      Map<String, String> packageMap,
      String profileConfigDirectory,
      Map<String, List<String>> profileDefinitions) throws IOException {
    super(new DefaultTargetConfiguration(
        outputDirectory,
        profileConfigDirectory,
        validateUnitTestDirectory(testRunner, unitTestDirectory)));
    setUnitTestRunner(UnitTestRunner.fromString(testRunner));
    setSourceConfig(new DefaultSourceConfiguration(inputDirectory, packageMap));
    for (Map.Entry<String, List<String>> profileDefinition : profileDefinitions.entrySet()) {
      setConfiguredProfile(
          createProfileConfiguration(profileDefinition.getKey(), profileDefinition.getValue()));
    }
  }

  /**
   * Initializes a new instance of the translator configuration class
   * @param inputFiles the list of the *.utam.json Page Object description files to be generated
   * @param outputDirectory the root output directory where the generated Page Object source files
   *                        will be written
   * @param unitTestDirectory the root output directory where generated unit tests for generated
   *                          Page Objects will be written
   * @param testRunner the test runner to use when generating unit tests
   * @param packageMap the mapping between directories containing the description files and
   *                   directories containing the generated source files
   * @param profileConfigDirectory the output directory in which to write profile information
   * @param profileDefinitions the map defining the list of known profiles and their values
   */
  public DefaultTranslatorConfiguration(
      List<File> inputFiles,
      String outputDirectory,
      String unitTestDirectory,
      String testRunner,
      Map<String, String> packageMap,
      String profileConfigDirectory,
      Map<String, List<String>> profileDefinitions) {
    super(new DefaultTargetConfiguration(
        outputDirectory,
        profileConfigDirectory,
        validateUnitTestDirectory(testRunner, unitTestDirectory)));
    setUnitTestRunner(UnitTestRunner.fromString(testRunner));
    setSourceConfig(new DefaultSourceConfiguration(inputFiles, packageMap));
    for (Map.Entry<String, List<String>> profileDefinition : profileDefinitions.entrySet()) {
      setConfiguredProfile(
          createProfileConfiguration(profileDefinition.getKey(), profileDefinition.getValue()));
    }
  }

  public static Map<String, String> createPackageMap(String packageMapFile) throws IOException {
    // Read the package mapping file, and translate the properties therein
    // into an in-memory map
    FileInputStream packageInput = new FileInputStream(packageMapFile);
    Properties packageProperties = new Properties();
    packageProperties.load(packageInput);
    return packageProperties.entrySet().stream()
        .collect(Collectors.toMap(
            entry -> entry.getKey().toString(),
            entry -> entry.getValue().toString()));
  }

  public static Map<String, List<String>> createProfileMap(String profileMapFile) throws IOException {
    if (profileMapFile == null || profileMapFile.isEmpty()) {
      return new HashMap<>();
    }

    // Read the profile mapping file, and translate the properties therein
    // into an in-memory map
    FileInputStream packageInput = new FileInputStream(profileMapFile);
    Properties packageProperties = new Properties();
    packageProperties.load(packageInput);
    return packageProperties.entrySet().stream()
        .collect(Collectors.toMap(
            entry -> entry.getKey().toString(),
            entry -> Arrays.stream(entry.getValue().toString().split(",")).collect(Collectors.toList())));
  }

  private static ProfileConfiguration createProfileConfiguration(String key, List<String> value) {
    String[] profileValues = new String[value.size()];
    return new StringValueProfileConfig(key, value.toArray(profileValues));
  }

  private static String validateUnitTestDirectory(String testRunner, String unitTestDirectory) {
    if (testRunner == null || testRunner.equals("none")) {
      return "";
    }
    return unitTestDirectory;
  }
}
