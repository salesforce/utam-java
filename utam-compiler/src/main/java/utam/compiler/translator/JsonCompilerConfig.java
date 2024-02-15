/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.translator;

import static com.fasterxml.jackson.core.JsonParser.Feature.ALLOW_COMMENTS;
import static utam.core.declarative.translator.UnitTestRunner.NONE;
import static utam.core.declarative.translator.UnitTestRunner.validateUnitTestDirectory;
import static utam.core.framework.UtamLogger.info;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;
import utam.compiler.UtamCompilationError;
import utam.compiler.lint.LintingConfigJson;
import utam.compiler.translator.CompilerErrors.Report;
import utam.compiler.translator.DefaultSourceConfiguration.FilesScanner;
import utam.compiler.translator.DefaultSourceConfiguration.RecursiveScanner;
import utam.compiler.translator.DefaultSourceConfiguration.ScannerConfig;
import utam.compiler.translator.DefaultSourceConfiguration.SourceWithoutPackages;
import utam.compiler.translator.DefaultTranslatorConfiguration.CompilerOutputOptions;
import utam.core.declarative.errors.CompilerErrorsConfig;
import utam.core.declarative.lint.LintingConfig;
import utam.core.declarative.translator.ProfileConfiguration;
import utam.core.declarative.translator.TranslatorConfig;
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

  private final Module moduleConfig;
  private final String filePathsRoot;
  private final List<File> inputFiles;

  /**
   * Initializes a new instance of the JsonCompilerConfig class
   *
   * @param configFile the configuration file
   * @param compilerRoot the root directory for the compiler
   * @param fileList the explicit list of Page Object files for compilation
   * @throws IOException thrown if there is an exception during the execution
   */
  public JsonCompilerConfig(File configFile, File compilerRoot, List<File> fileList)
      throws IOException {
    try {
      ObjectMapper mapper = new ObjectMapper();
      mapper.enable(ALLOW_COMMENTS);
      // config can contain custom properties, for example shared by JS and Java
      mapper.disable(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES);
      info("Read compiler config " + configFile.toString());
      moduleConfig = mapper.readValue(configFile, Module.class);
      filePathsRoot = compilerRoot.toString();
      info(getConfigLoggerMessage("compiler root", filePathsRoot));
      info(getConfigLoggerMessage("compiler config", configFile.toString()));
      inputFiles = new ArrayList<>();
      if (fileList != null) {
        inputFiles.addAll(fileList);
      }
    } catch (IOException e) {
      throw new IOException(String.format(ERR_READING_COMPILER_CONFIG, configFile), e);
    }
  }

  private static String getConfigLoggerMessage(String optionName, Object optionValue) {
    String value =
        optionValue == null
            ? "null"
            : (optionValue.toString().isEmpty() ? "empty string" : optionValue.toString());
    return String.format("Compiler config: %s is set to %s", optionName, value);
  }

  /**
   * Gets a value indicating whether the list source files to compile is valid
   *
   * @return true if a root directory for recursive scanning is specified, or a list of input files
   *     is specified, but not both.
   */
  public boolean isValidSourceFileSpecification() {
    return moduleConfig.pageObjectsRootDirectory != null && inputFiles.isEmpty()
        || moduleConfig.pageObjectsRootDirectory == null && !inputFiles.isEmpty();
  }

  /**
   * Gets the list of configured profiles
   *
   * @return the list of configured profiles
   */
  private List<ProfileConfiguration> getConfiguredProfiles() {
    return moduleConfig.getConfiguredProfiles();
  }

  /**
   * Gets the target configuration
   *
   * @return the target configuration
   */
  private TranslatorTargetConfig getTargetConfig() {
    return moduleConfig.getTargetConfig(filePathsRoot);
  }

  /**
   * Gets the source configuration
   *
   * @return the source configuration
   */
  public TranslatorSourceConfig getSourceConfig() {
    return moduleConfig.getSourceConfig(filePathsRoot, inputFiles);
  }

  CompilerErrorsConfig getErrorsConfig(TranslatorTargetConfig targetConfig) {
    String absolutePath = targetConfig.getErrorsReportPath();
    return absolutePath == null ? new CompilerErrors.Throws() : new Report(absolutePath);
  }

  /**
   * Gets the module name
   *
   * @return the name of the module
   */
  public String getModuleName() {
    return moduleConfig.getName();
  }

  /**
   * get version to mark page objects javaDocs, used only in unit tests
   *
   * @return version string or null
   */
  String getVersion() {
    return moduleConfig.outputOptions.pageObjectsVersion;
  }

  /**
   * get configured copyright, used only in unit tests
   *
   * @return list of strings
   */
  List<String> getCopyright() {
    return moduleConfig.outputOptions.configuredCopyright;
  }

  /**
   * get module configuration, used only in unit tests
   *
   * @return module instance
   */
  Module getModule() {
    return moduleConfig;
  }

  /**
   * Gets the translator configuration
   *
   * @return the translator configurtion
   */
  public TranslatorConfig getTranslatorConfig() {
    TranslatorSourceConfig sourceConfig = getSourceConfig();
    TranslatorTargetConfig targetConfig = getTargetConfig();
    List<ProfileConfiguration> profiles = getConfiguredProfiles();
    CompilerErrorsConfig errorsConfig = getErrorsConfig(targetConfig);
    return new DefaultTranslatorConfiguration(
        moduleConfig.outputOptions,
        moduleConfig.lintingConfiguration,
        errorsConfig,
        sourceConfig,
        targetConfig,
        profiles);
  }

  /**
   * JSON mapping that represents a Page Objects module with namespaces and profiles
   *
   * @author james.evans
   * @since 234
   */
  public static class Module {

    static final String DEFAULT_JSON_FILE_MASK_REGEX = "(.*)\\.utam\\.json$";
    static final String ERR_DUPLICATE_PROFILE = "Profile '%s' is already configured";
    static final String ERR_FILES_WITHOUT_NAMESPACE =
        "Must use namespaces when explicitly setting input file list";
    private final List<Profile> profiles = new ArrayList<>();
    final List<Namespace> namespaces = new ArrayList<>();
    private final String pageObjectFileMaskRegex;
    private final String pageObjectsRootDirectory;
    private final String pageObjectsOutputDir;
    private final String resourcesOutputDir;
    private final String unitTestsOutputDir;
    private final UnitTestRunner unitTestRunnerType;
    private final CompilerOutputOptions outputOptions;
    private final LintingConfig lintingConfiguration;
    private final String compilerErrorsFile;

    /**
     * Initializes a new instance of the Module class. Instantiated via JSON deserialization.
     *
     * @param moduleName the arbitrary name of the module in the source repository
     * @param pageObjectsVersion optional name of the version for JavaDoc
     * @param filesMaskRegex used by scanner to distinguish JSON with page objects
     * @param pageObjectsRootDirectory the directory in the source repository in which to
     *     recursively search for UTAM Page Object declarative description files
     * @param pageObjectsOutputDir target root folder for generated Page Objects
     * @param resourcesOutputDir target folder for generated resources like profiles configurations
     * @param unitTestDirectory target root folder for generated unit tests
     * @param unitTestRunner type of the unit tests runner for unit tests generation, default is
     *     NONE, can also be "testng" or "junit"
     * @param namespaces an array of Namespace objects describing the namespaces within the module
     * @param profiles an array of Profile objects representing the profiles used in JSON files of
     *     the module
     * @param copyright lines for copyright header
     * @param lintingConfiguration configured linting
     * @param isErrorsReportFile boolean flag to write compiler errors into a file vs. interrupt
     *     with exception
     */
    @JsonCreator
    public Module(
        @JsonProperty(value = "module") String moduleName,
        @JsonProperty(value = "version") String pageObjectsVersion,
        @JsonProperty(value = "pageObjectsFilesMask") String filesMaskRegex,
        @JsonProperty(value = "pageObjectsRootDir") String pageObjectsRootDirectory,
        @JsonProperty(value = "pageObjectsOutputDir", required = true) String pageObjectsOutputDir,
        @JsonProperty(value = "resourcesOutputDir", required = true) String resourcesOutputDir,
        @JsonProperty(value = "unitTestsOutputDir") final String unitTestDirectory,
        @JsonProperty(value = "unitTestsRunner", defaultValue = "NONE")
            UnitTestRunner unitTestRunner,
        @JsonProperty(value = "namespaces") List<Namespace> namespaces,
        @JsonProperty(value = "profiles") List<Profile> profiles,
        @JsonProperty(value = "copyright") List<String> copyright,
        @JsonProperty(value = "lint") LintingConfigJson lintingConfiguration,
        @JsonProperty(value = "interruptCompilerOnError") Boolean isErrorsReportFile) {
      this.pageObjectsRootDirectory = pageObjectsRootDirectory;
      info(getConfigLoggerMessage("pageObjectsRootDir", pageObjectsRootDirectory));

      this.pageObjectFileMaskRegex =
          Objects.requireNonNullElse(filesMaskRegex, DEFAULT_JSON_FILE_MASK_REGEX);
      info(getConfigLoggerMessage("pageObjectsFilesMask", pageObjectFileMaskRegex));

      this.pageObjectsOutputDir = pageObjectsOutputDir;
      info(getConfigLoggerMessage("pageObjectsOutputDir", pageObjectsOutputDir));

      this.resourcesOutputDir = resourcesOutputDir;
      info(getConfigLoggerMessage("resourcesOutputDir", resourcesOutputDir));

      this.unitTestsOutputDir = validateUnitTestDirectory(unitTestRunner, unitTestDirectory);
      info(getConfigLoggerMessage("unitTestsOutputDir", this.unitTestsOutputDir));

      this.unitTestRunnerType = Objects.requireNonNullElse(unitTestRunner, NONE);
      info(getConfigLoggerMessage("unitTestRunner", unitTestRunner));

      this.namespaces.addAll(Objects.requireNonNullElse(namespaces, new ArrayList<>()));
      String namespacesString =
          this.namespaces.stream().map(n -> n.typePrefix).collect(Collectors.joining(", "));
      info(getConfigLoggerMessage("namespaces", "[ " + namespacesString + " ]"));

      setUniqueProfiles(profiles);
      String profilesString =
          this.profiles.stream().map(p -> p.name).collect(Collectors.joining(", "));
      info(getConfigLoggerMessage("profiles", "[ " + profilesString + " ]"));

      this.outputOptions =
          new CompilerOutputOptions(
              Objects.requireNonNullElse(moduleName, ""),
              Objects.requireNonNullElse(pageObjectsVersion, ""),
              Objects.requireNonNullElse(copyright, new ArrayList<>()));
      this.lintingConfiguration = LintingConfigJson.getLintingConfig(lintingConfiguration);
      info(getConfigLoggerMessage("lint configuration", this.lintingConfiguration.toString()));
      this.compilerErrorsFile =
          Boolean.FALSE == isErrorsReportFile ? CompilerErrors.ERR_REPORT_FILE : null;
      info(getConfigLoggerMessage("errorsReportFile", this.compilerErrorsFile));
    }

    void setUniqueProfiles(List<Profile> profiles) {
      if (profiles == null || profiles.isEmpty()) {
        return;
      }
      // check duplicates
      Set<String> configuredProfiles = new HashSet<>();
      profiles.forEach(
          profile -> {
            if (configuredProfiles.contains(profile.name)) {
              throw new UtamCompilationError(String.format(ERR_DUPLICATE_PROFILE, profile.name));
            }
            configuredProfiles.add(profile.name);
          });
      this.profiles.addAll(profiles);
    }

    /**
     * Gets the translator source configuration
     *
     * @param compilerRootFolderName the name of the root folder for the compiler
     * @param inputFiles the explicit list of files to compile, if specified
     * @return the translator source configuration
     */
    public TranslatorSourceConfig getSourceConfig(
        String compilerRootFolderName, List<File> inputFiles) {
      if (namespaces.isEmpty()) {
        if (inputFiles != null && !inputFiles.isEmpty()) {
          throw new UtamCompilationError(ERR_FILES_WITHOUT_NAMESPACE);
        }
        return new SourceWithoutPackages(
            compilerRootFolderName + pageObjectsRootDirectory, pageObjectFileMaskRegex);
      }
      RecursiveScanner scanner =
          inputFiles != null && !inputFiles.isEmpty()
              ? new FilesScanner(inputFiles)
              : new RecursiveScanner(compilerRootFolderName + pageObjectsRootDirectory);
      return new DefaultSourceConfiguration(
          new ScannerConfig(pageObjectFileMaskRegex, getPackagesMapping()), scanner);
    }

    /**
     * Gets the translator target configuration
     *
     * @param compilerRootFolderName the name of the root folder for the compiler
     * @return the translator target configuration
     */
    public TranslatorTargetConfig getTargetConfig(String compilerRootFolderName) {
      return new DefaultTargetConfiguration(
          compilerRootFolderName,
          compilerRootFolderName + pageObjectsOutputDir,
          compilerRootFolderName + resourcesOutputDir,
          unitTestRunnerType,
          compilerRootFolderName + unitTestsOutputDir,
          compilerErrorsFile);
    }

    /**
     * Gets the mapping for the packages
     *
     * @return the mapping for the packages
     */
    public Map<String, String> getPackagesMapping() {
      Map<String, String> packages = new HashMap<>();
      namespaces.forEach(namespace -> namespace.setPackageMapping(packages));
      return packages;
    }

    /**
     * Gets the name of the module.
     *
     * @return the name of the module
     */
    public String getName() {
      return outputOptions.moduleName;
    }

    /**
     * Gets the directory in the source repository in which to recursively search for UTAM Page
     * Object declarative description files
     *
     * @return the directory in the source repository in which to recursively search for UTAM Page
     *     Object declarative description files
     */
    public String getPageObjectsRootDirectory() {
      return pageObjectsRootDirectory;
    }

    /**
     * get list of configured profiles
     *
     * @return list of profile configurations
     */
    public List<ProfileConfiguration> getConfiguredProfiles() {
      List<ProfileConfiguration> all = new ArrayList<>();
      profiles.forEach(p -> p.setConfiguredProfile(all));
      return all;
    }

    /**
     * get list of raw profiles from config
     *
     * @return list of profiles from JSON
     */
    public List<Profile> getRawProfiles() {
      return profiles;
    }

    /**
     * file mask to recognize JSON with a page object
     *
     * @return string with mask
     */
    public String getPageObjectFileMaskRegex() {
      return pageObjectFileMaskRegex;
    }

    /**
     * target root folder for generated Page Objects
     *
     * @return string with file path
     */
    public String getPageObjectsOutputDir() {
      return pageObjectsOutputDir;
    }

    /**
     * target root folder for generated resources
     *
     * @return string with file path
     */
    public String getResourcesOutputDir() {
      return resourcesOutputDir;
    }

    /**
     * target root folder for generated Page Objects unit tests
     *
     * @return string with file path
     */
    public String getUnitTestsOutputDir() {
      return unitTestsOutputDir;
    }

    /**
     * type of the unit tests to generate, one of: "none", "junit", "testng"
     *
     * @return string with file path
     */
    public UnitTestRunner getUnitTestRunnerType() {
      return unitTestRunnerType;
    }
  }

  /**
   * JSON mapping that represents a profile configured for a UTAM Page Objects module
   *
   * @author james.evans
   * @since 234
   */
  public static class Profile {

    static final String ERR_DUPLICATE_PROFILE_DIFF_VALUES =
        "Profile '%s' is already configured with different values";
    private final String name;
    private final String[] values;

    /**
     * Initializes a new instance of the Profile class. Instantiated via JSON deserialization.
     *
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

    /**
     * Gets the profile configuration
     *
     * @return the profile configuration
     */
    public ProfileConfiguration getProfileConfiguration() {
      return new StringValueProfileConfig(name, values);
    }

    /**
     * Sets the configured profile
     *
     * @param profiles the list of profiles to configure
     */
    public void setConfiguredProfile(List<ProfileConfiguration> profiles) {
      ProfileConfiguration profileConfiguration = getProfileConfiguration();
      for (ProfileConfiguration alreadyConfigured : profiles) {
        if (profileConfiguration.equals(alreadyConfigured)) {
          // same profile config already added
          return;
        }
        // check for profile with same name but different values
        if (alreadyConfigured.getPropertyKey().equals(profileConfiguration.getPropertyKey())) {
          throw new UtamCompilationError(
              String.format(ERR_DUPLICATE_PROFILE_DIFF_VALUES, alreadyConfigured.getPropertyKey()));
        }
      }
      profiles.add(profileConfiguration);
    }
  }

  /**
   * JSON mapping that represents a namespace within a UTAM Page Objects module
   *
   * @author james.evans
   */
  public static class Namespace {

    static final String ERR_DUPLICATE_MAPPING =
        "Namespace config: value for '%s' is already configured as '%s'";

    private final String typePrefix;
    private final String pathMatchRegex;

    /**
     * Initializes a new instance of the Namespace class. Instantiated via JSON deserialization.
     *
     * @param typePrefix type prefix
     * @param pathMatchRegex the mapped name used by the UTAM compiler to generate packages for the
     *     generated Page Object source code
     */
    @JsonCreator
    public Namespace(
        @JsonProperty(value = "typeMatch", required = true) String typePrefix,
        @JsonProperty(value = "pathMatch", required = true) String pathMatchRegex) {
      this.typePrefix = typePrefix;
      this.pathMatchRegex = pathMatchRegex;
    }

    /**
     * Gets the mapped name used by the UTAM compiler to generate packages for the generated Page
     * Object source code. public because used in distribution JSON config
     *
     * @return the mapped name used by the UTAM compiler to generate packages for the generated Page
     *     Object source code
     */
    public String getTypeMatch() {
      return typePrefix;
    }

    /**
     * public because can be used in distribution JSON config in a downstream project
     *
     * @return regex to use for scanning folders
     */
    public String getPathMatch() {
      return pathMatchRegex;
    }

    /**
     * Key to create types to packages mapping, distribution setup can override this method
     *
     * @return key for package mapping
     */
    protected String getPackageMappingKey() {
      return getTypeMatch();
    }

    /**
     * Value to create types to packages mapping, distribution setup can override this method
     *
     * @return value for package mapping
     */
    protected String getPackageMappingValue() {
      return getPathMatch();
    }

    // setter ensures that namespaces are unique within a module
    void setPackageMapping(Map<String, String> packages) {
      String key = getPackageMappingKey();
      String value = getPackageMappingValue();
      if (packages.containsKey(key) && !packages.get(key).equals(value)) {
        throw new UtamCompilationError(
            String.format(ERR_DUPLICATE_MAPPING, key, packages.get(key)));
      }
      packages.put(key, value);
    }
  }
}
