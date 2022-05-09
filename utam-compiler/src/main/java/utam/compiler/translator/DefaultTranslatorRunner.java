/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.translator;

import static utam.core.framework.UtamLogger.info;
import static utam.core.framework.consumer.JsonInjectionsConfig.CONFIG_FILE_MASK;

import com.google.common.io.CharStreams;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.Writer;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import utam.compiler.UtamCompilationError;
import utam.compiler.grammar.JsonDeserializer;
import utam.compiler.guardrails.GlobalValidation;
import utam.compiler.helpers.TranslationContext;
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
import utam.core.framework.consumer.UtamError;
import utam.core.framework.context.Profile;

/**
 * abstract runner
 *
 * @author elizaveta.ivanova
 * @since 226
 */
public class DefaultTranslatorRunner implements TranslatorRunner {

  static final String ERR_PROFILE_PATH_DOES_NOT_EXIST =
      "can't write profiles output, profile path '%s' does not exist and cannot be created";
  static final String ERR_PROFILE_PATH_NOT_CONFIGURED = "profile config path is null or empty";
  static final String DUPLICATE_PAGE_OBJECT_NAME = "declaration '%s' already generated";
  static final String DUPLICATE_IMPL_WITH_PROFILE_ERR =
      "can't set dependency as '%s' for type '%s', it was already set as '%s' for profile %s";
  private static final String ERR_MODULE_NAME =
      "module name is not configured, can't write dependencies config file";
  private final TranslatorConfig translatorConfig;
  private final Map<String, PageObjectDeclaration> generated = new HashMap<>();
  private final Map<Profile, Map<String, String>> profileDependenciesMapping = new HashMap<>();
  // max number of POs to generate for generator performance measurements
  private int maxPageObjectsCounter = Integer.MAX_VALUE;

  private static String getStringFromReader(TranslatorSourceConfig translatorSourceConfig, String pageObjectURI) {
    try {
      return CharStreams.toString(translatorSourceConfig.getDeclarationReader(pageObjectURI));
    } catch (IOException e) {
      throw new UtamCompilationError(String.format("Error in the page object '%s'", pageObjectURI), e);
    }
  }

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
      if (object.isClassWithInterface()) {
        info(
            String.format(
                "write interface %s", pageObjectInterface.getInterfaceType().getFullName()));
        write(pageObjectInterface.getInterfaceType(), pageObjectInterface.getGeneratedCode());
        filesCounter++;
      } else {
        info(
            String.format(
                "interface %s already exists",
                pageObjectInterface.getInterfaceType().getFullName()));
      }
      if (!object.isInterfaceOnly()) {
        info(
            String.format(
                "write class %s", object.getImplementation().getClassType().getFullName()));
        write(object.getImplementation().getClassType(), object.getImplementation().getGeneratedCode());
        filesCounter++;
        if (writeUnitTest(object.getImplementation())) {
          filesCounter++;
        }
      }
      counter++;
    }
    info(String.format("generated %d files for %d page objects, took %d msec", filesCounter, counter,
        System.currentTimeMillis() - timer));
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
        writer.write(impl.getGeneratedUnitTestCode(
            translatorConfig.getConfiguredTarget().getUnitTestRunnerType()));
        writer.flush();
      } catch (IOException e) {
        throw new RuntimeException(e);
      }
    }
    return true;
  }

  private void write(TypeProvider typeProvider, String code) throws IOException {
    Writer writer = getTargetConfig().getClassWriter(typeProvider);
    try {
      writer.write(code);
      writer.flush();
    } catch (IOException e) {
      throw new RuntimeException(e);
    }
  }

  private TranslatorTargetConfig getTargetConfig() {
    return translatorConfig.getConfiguredTarget();
  }

  @Override
  public void run() {
    int counter = 0;
    long timer = System.currentTimeMillis();
    TranslatorSourceConfig sourceConfig = translatorConfig.getConfiguredSource();
    GlobalValidation globalGuardrails = new GlobalValidation(translatorConfig.getValidationMode());
    sourceConfig.recursiveScan();
    for (String pageObjectURI : sourceConfig.getPageObjects()) {
      if (counter >= maxPageObjectsCounter) {
        break;
      }
      info(String.format("de-serialize Page Object %s", pageObjectURI));
      TranslationContext translationContext = new TranslationContext(pageObjectURI, translatorConfig);
      String jsonSource = getStringFromReader(sourceConfig, pageObjectURI);
      JsonDeserializer deserializer = new JsonDeserializer(translationContext, jsonSource);
      PageObjectDeclaration object = deserializer.getObject();
      setPageObject(pageObjectURI, object);
      deserializer.getPageObjectContext().setGlobalGuardrailsContext(globalGuardrails);
      counter++;
    }
    globalGuardrails.validate();
    info(String.format("generated %d page objects, took %d msec", counter,
        System.currentTimeMillis() - timer));
  }

  final String getResourcesRoot() {
    String profilesRoot = getTargetConfig().getInjectionConfigRootFilePath();
    if (profilesRoot == null || profilesRoot.isEmpty()) {
      throw new UtamError(ERR_PROFILE_PATH_NOT_CONFIGURED);
    }
    return profilesRoot;
  }

  @Override
  public void writeDependenciesConfigs() {
    JsonCompilerOutput jsonCompilerOutput = new JsonCompilerOutput(profileDependenciesMapping);
    String moduleName = translatorConfig.getModuleName();
    if(moduleName == null || moduleName.isEmpty()) {
      // if module not set and there is no dependencies - no issue
      if(profileDependenciesMapping.isEmpty()) {
        return;
      }
      throw new UtamCompilationError(ERR_MODULE_NAME);
    }
    File resourcesRoot = new File(getResourcesRoot());
    if (!resourcesRoot.exists() && !resourcesRoot.mkdirs()) {
      throw new UtamError(String.format(ERR_PROFILE_PATH_DOES_NOT_EXIST, resourcesRoot));
    }
    String profileConfigPath =
        resourcesRoot + File.separator + String.format(CONFIG_FILE_MASK, moduleName);
    try {
      Writer writer = new FileWriter(profileConfigPath);
      jsonCompilerOutput.writeConfig(writer);
    } catch (IOException e) {
      throw new UtamCompilationError(
          String.format("error while writing profile configuration '%s'", profileConfigPath),
          e);
    }
  }

  // used from tests and during compilation
  final void setPageObject(String name, PageObjectDeclaration object) {
    if (generated.containsKey(name)) {
      throw new UtamCompilationError(String.format(DUPLICATE_PAGE_OBJECT_NAME, name));
    }
    generated.put(name, object);
    String typeName = object.getInterface().getInterfaceType().getFullName();
    // interface only
    if (object.isInterfaceOnly()) {
      return;
    }
    String classTypeName = object.getImplementation().getClassType().getFullName();
    if (!object.isClassWithInterface()) {
      for (Profile profile : object.getImplementation().getProfiles()) {
        setImplementation(profile, typeName, classTypeName);
      }
    }
  }

  final void setImplementation(Profile profile, String typeName, String classTypeName) {
    if (!profileDependenciesMapping.containsKey(profile)) {
      throw new UtamCompilationError(translatorConfig.getErrorMessage(803, profile.getName(), profile.getValue()));
    }
    if (profileDependenciesMapping.get(profile).containsKey(typeName)) {
      String profileValue = String.format("{ %s : %s }", profile.getName(), profile.getValue());
      String implType = profileDependenciesMapping.get(profile).get(typeName);
      throw new UtamCompilationError(
          String.format(
              DUPLICATE_IMPL_WITH_PROFILE_ERR,
              classTypeName,
              typeName,
              implType,
              profileValue));
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
