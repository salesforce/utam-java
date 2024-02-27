/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.translator;

import static utam.compiler.diagnostics.ValidationUtilities.VALIDATION;
import static utam.core.framework.UtamLogger.info;
import static utam.core.framework.consumer.JsonLoaderConfig.INJECTION_CONFIG_FILE_MASK;
import static utam.core.framework.consumer.UtamLoaderConfigImpl.DEFAULT_PROFILE;

import com.google.common.io.CharStreams;
import java.io.File;
import java.io.IOException;
import java.io.Writer;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import utam.compiler.UtamCompilationError;
import utam.compiler.grammar.JsonDeserializer;
import utam.compiler.helpers.TranslationContext;
import utam.core.declarative.errors.CompilerErrorsContext;
import utam.core.declarative.lint.LintingConfig;
import utam.core.declarative.lint.LintingContext;
import utam.core.declarative.representation.PageObjectClass;
import utam.core.declarative.representation.PageObjectDeclaration;
import utam.core.declarative.representation.PageObjectInterface;
import utam.core.declarative.representation.TypeProvider;
import utam.core.declarative.translator.ProfileConfiguration;
import utam.core.declarative.translator.TranslatorConfig;
import utam.core.declarative.translator.TranslatorRunner;
import utam.core.declarative.translator.TranslatorSourceConfig;
import utam.core.declarative.translator.TranslatorTargetConfig;
import utam.core.declarative.translator.UnitTestRunner;
import utam.core.framework.context.Profile;

/**
 * abstract runner
 *
 * @author elizaveta.ivanova
 * @since 226
 */
public class DefaultTranslatorRunner implements TranslatorRunner {

  static final String ERR_PROFILE_PATH_NOT_CONFIGURED = "profile config path is null or empty";
  static final String DUPLICATE_PAGE_OBJECT_NAME = "declaration '%s' already generated";
  static final String DUPLICATE_IMPL_WITH_PROFILE_ERR =
      "can't set dependency as '%s' for type '%s', it was already set as '%s' for profile %s";
  static final String ERR_MODULE_NAME_NOT_CONFIGURED =
      "module name is not configured, can't write dependencies config file";
  private static final String ERR_PROFILE_PATH_DOES_NOT_EXIST =
      "can't write profiles output, profile path '%s' does not exist and cannot be created";
  private static final String ERR_MANIFEST_PATH_DOES_NOT_EXIST =
      "can't write manifest output, manifest path '%s' does not exist and cannot be created";
  private static final String PAGE_OBJECT_MANIFEST_FILE_NAME = "pageObjectManifest.json";
  private final TranslatorConfig translatorConfig;
  private final Map<String, PageObjectDeclaration> generated = new HashMap<>();
  private final Map<String, String> jsonSources = new HashMap<>();
  private final Manifest manifest = new Manifest();
  private final Map<Profile, Map<String, String>> profileDependenciesMapping = new HashMap<>();
  // max number of POs to generate for generator performance measurements
  private int maxPageObjectsCounter = Integer.MAX_VALUE;

  /**
   * Initializes a new instance of the DefaultTranslatorRunner class
   *
   * @param translatorConfig the configuration of the translator
   */
  public DefaultTranslatorRunner(TranslatorConfig translatorConfig) {
    this.translatorConfig = translatorConfig;
    for (ProfileConfiguration configuration : translatorConfig.getConfiguredProfiles()) {
      for (String value : configuration.getSupportedValues()) {
        Profile profile = configuration.getFromString(value);
        profileDependenciesMapping.put(profile, new HashMap<>());
      }
    }
    profileDependenciesMapping.put(DEFAULT_PROFILE, new HashMap<>());
  }

  private static String getStringFromReader(
      TranslatorSourceConfig translatorSourceConfig, String pageObjectURI) {
    try {
      return CharStreams.toString(translatorSourceConfig.getDeclarationReader(pageObjectURI));
    } catch (IOException e) {
      throw new UtamRunnerError(String.format("Error in the page object '%s'", pageObjectURI), e);
    }
  }

  final PageObjectDeclaration getGeneratedObject(String name) {
    // As written, pageObjectURI *must* be in generated, because getObject
    // is only ever called by iterating over the keyset of generated. If
    // that ever changes, a guard will need to be placed here.
    return generated.get(name);
  }

  final Collection<String> getGeneratedPageObjectsNames() {
    return generated.keySet();
  }

  @Override
  public void write() throws IOException {
    int counter = 0;
    int filesCounter = 0;
    long timer = System.currentTimeMillis();
    for (String name : getGeneratedPageObjectsNames()) {
      if (counter >= maxPageObjectsCounter) {
        break;
      }
      PageObjectDeclaration object = getGeneratedObject(name);
      PageObjectInterface pageObjectInterface = object.getInterface();
      String pageObjectJsonSource = jsonSources.get(name);
      if (writePageObjectJsonSource(name, pageObjectJsonSource)) {
        filesCounter++;
      }
      if (object.isClassWithInterface()) {
        write(pageObjectInterface.getInterfaceType(), pageObjectInterface.getGeneratedCode());
        filesCounter++;
      } else {
        info(
            String.format(
                "interface %s already exists",
                pageObjectInterface.getInterfaceType().getFullName()));
      }
      if (!object.isInterfaceOnly()) {
        write(
            object.getImplementation().getClassType(),
            object.getImplementation().getGeneratedCode());
        filesCounter++;
        if (writeUnitTest(object.getImplementation())) {
          filesCounter++;
        }
      }
      counter++;
    }
    info(
        String.format(
            "generated %d files for %d page objects, took %d msec",
            filesCounter, counter, System.currentTimeMillis() - timer));
  }

  private boolean writePageObjectJsonSource(String name, String pageObjectJsonSource)
      throws IOException {
    Writer writer = getTargetConfig().getResourceWriter(name);
    // Legitimate case for writer == null is that the runner configuration
    // wants to write JSON source files, but skip files that already exist.
    if (writer != null) {
      try {
        info(String.format("writing JSON source for %s", name));
        writer.write(pageObjectJsonSource);
        writer.flush();
        return true;
      } catch (IOException e) {
        throw new RuntimeException(e);
      }
    }
    return false;
  }

  /**
   * Sets the maximum number of POs to generate. Only used for generator performance measurements.
   *
   * @param number the maximum number of Page Objects to generate.
   */
  protected void setMaxToGenerate(int number) {
    this.maxPageObjectsCounter = number;
  }

  // returns true if new file was written
  private boolean writeUnitTest(PageObjectClass impl) throws IOException {
    if (translatorConfig.getConfiguredTarget().getUnitTestRunnerType() == UnitTestRunner.NONE) {
      return false;
    }
    TypeProvider typeProvider = impl.getClassType();
    Writer writer = getTargetConfig().getUnitTestWriter(typeProvider);
    // Legitimate case for writer == null is that the runner configuration
    // wants to write unit tests, but skip files that already exist.
    if (writer != null) {
      try {
        info(String.format("generating unit test for %s", typeProvider.getFullName()));
        writer.write(
            impl.getGeneratedUnitTestCode(
                translatorConfig.getConfiguredTarget().getUnitTestRunnerType()));
        writer.flush();
      } catch (IOException e) {
        throw new RuntimeException(e);
      }
    }
    return true;
  }

  private void write(TypeProvider typeProvider, String code) throws IOException {
    String name = String.format("page object %s", typeProvider.getFullName());
    info("write " + name);
    Writer writer = getTargetConfig().getClassWriter(typeProvider);
    try {
      writer.write(code);
      writer.flush();
    } catch (IOException e) {
      throw new UtamRunnerError("error while writing " + name, e);
    }
  }

  private TranslatorTargetConfig getTargetConfig() {
    return translatorConfig.getConfiguredTarget();
  }

  @Override
  public RunnerOutput run() {
    int counter = 0;
    long timer = System.currentTimeMillis();
    TranslatorSourceConfig sourceConfig = translatorConfig.getConfiguredSource();
    TranslatorTargetConfig targetConfig = getTargetConfig();
    LintingConfig linting = translatorConfig.getLintingConfig();
    sourceConfig.recursiveScan();
    LintingContext lintingContext = linting.start();
    CompilerErrorsContext errorsContext = translatorConfig.getErrorsConfig().newContext();
    for (String pageObjectURI : sourceConfig.getPageObjects()) {
      if (counter >= maxPageObjectsCounter) {
        break;
      }
      info(String.format("de-serialize Page Object %s", pageObjectURI));
      TranslationContext translationContext =
          new TranslationContext(
              pageObjectURI, sourceConfig.getSourcePath(pageObjectURI), translatorConfig);
      String jsonSource = getStringFromReader(sourceConfig, pageObjectURI);
      JsonDeserializer deserializer = new JsonDeserializer(translationContext, jsonSource);
      PageObjectDeclaration object = deserializer.getObject();
      if (object != null) {
        jsonSources.put(pageObjectURI, jsonSource);
        manifest.addWrittenResource(targetConfig.getJsonResourcePathForUri(pageObjectURI));
        setPageObject(pageObjectURI, object);
        linting.lint(lintingContext, translationContext.getLintingObject());
      } else {
        errorsContext.setError(translationContext.getCompilerError());
      }
      counter++;
    }
    linting.finish(lintingContext);
    linting.writeReport(lintingContext, translatorConfig.getConfiguredTarget().getLintReportPath());
    info(
        String.format(
            "generated %d page objects, took %d msec",
            counter, System.currentTimeMillis() - timer));
    String compilationErrors = translatorConfig.getErrorsConfig().report(errorsContext);
    if (compilationErrors != null) {
      throw new UtamCompilationError(compilationErrors);
    }
    return lintingContext::getErrors;
  }

  /**
   * build string with dependencies config path
   *
   * @param moduleName name of the module
   * @return full path
   */
  final String buildDependenciesConfigPath(String moduleName) {
    if (moduleName == null || moduleName.isEmpty()) {
      throw new UtamRunnerError(ERR_MODULE_NAME_NOT_CONFIGURED);
    }
    String profilesRoot = getTargetConfig().getInjectionConfigRootFilePath();
    if (profilesRoot == null || profilesRoot.isEmpty()) {
      throw new UtamRunnerError(ERR_PROFILE_PATH_NOT_CONFIGURED);
    }
    File resourcesRoot = new File(profilesRoot);
    if (!resourcesRoot.exists() && !resourcesRoot.mkdirs()) {
      throw new UtamRunnerError(String.format(ERR_PROFILE_PATH_DOES_NOT_EXIST, resourcesRoot));
    }
    return resourcesRoot + File.separator + String.format(INJECTION_CONFIG_FILE_MASK, moduleName);
  }

  @Override
  public void writeDependenciesConfigs() {
    if (profileDependenciesMapping.isEmpty()) {
      return;
    }
    String moduleName = translatorConfig.getModuleName();
    String configPath = buildDependenciesConfigPath(moduleName);
    JsonCompilerOutput output = getCompilerOutput();
    output.writeConfigToFile(configPath);
  }

  /**
   * build string with manifest path
   *
   * @return full path
   */
  final String buildManifestPath() {
    String resourcesRootPath = getTargetConfig().getInjectionConfigRootFilePath();
    if (resourcesRootPath == null || resourcesRootPath.isEmpty()) {
      throw new UtamRunnerError(ERR_PROFILE_PATH_NOT_CONFIGURED);
    }
    File resourcesRoot = new File(resourcesRootPath);
    if (!resourcesRoot.exists() && !resourcesRoot.mkdirs()) {
      throw new UtamRunnerError(String.format(ERR_MANIFEST_PATH_DOES_NOT_EXIST, resourcesRoot));
    }
    return resourcesRoot + File.separator + PAGE_OBJECT_MANIFEST_FILE_NAME;
  }

  @Override
  public void writeManifest() {
    String manifestPath = buildManifestPath();
    manifest.writeToPath(manifestPath);
  }

  // separate method to trigger from tests
  final JsonCompilerOutput getCompilerOutput() {
    return new JsonCompilerOutput(profileDependenciesMapping);
  }

  // used from tests and during compilation
  final void setPageObject(String name, PageObjectDeclaration object) {
    if (generated.containsKey(name)) {
      throw new UtamRunnerError(String.format(DUPLICATE_PAGE_OBJECT_NAME, name));
    }
    generated.put(name, object);
    String typeName = object.getInterface().getInterfaceType().getFullName();
    // interface only
    if (object.isInterfaceOnly()) {
      return;
    }
    String classTypeName = object.getImplementation().getClassType().getFullName();
    if (!object.isClassWithInterface()) {
      List<Profile> profiles = object.getImplementation().getProfiles();
      if (profiles.isEmpty()) {
        setImplementation(DEFAULT_PROFILE, typeName, classTypeName);
      } else {
        for (Profile profile : profiles) {
          setImplementation(profile, typeName, classTypeName);
        }
      }
    }
  }

  final void setImplementation(Profile profile, String typeName, String classTypeName) {
    if (!profileDependenciesMapping.containsKey(profile)) {
      throw new UtamRunnerError(
          VALIDATION.getErrorMessage(803, profile.getName(), profile.getValue()));
    }
    if (profileDependenciesMapping.get(profile).containsKey(typeName)) {
      String profileValue = String.format("{ %s : %s }", profile.getName(), profile.getValue());
      String implType = profileDependenciesMapping.get(profile).get(typeName);
      throw new UtamRunnerError(
          String.format(
              DUPLICATE_IMPL_WITH_PROFILE_ERR, classTypeName, typeName, implType, profileValue));
    }
    profileDependenciesMapping.get(profile).put(typeName, classTypeName);
  }

  /**
   * used in unit tests - get profile mapping
   *
   * @param profile profile value
   * @return map
   */
  final Map<String, String> testProfileMapping(Profile profile) {
    return profileDependenciesMapping.get(profile);
  }
}
