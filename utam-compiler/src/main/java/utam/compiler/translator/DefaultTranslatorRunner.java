/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.translator;

import static utam.core.framework.UtamLogger.info;
import static utam.core.framework.context.StringValueProfile.DEFAULT_PROFILE;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.Writer;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;
import utam.compiler.UtamCompilationError;
import utam.compiler.grammar.JsonDeserializer;
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
      "can't write profiles output, profile path '%s' does not exist";
  static final String ERR_PROFILE_PATH_NOT_CONFIGURED = "profile config path is null or empty";
  static final String DUPLICATE_PAGE_OBJECT_NAME = "declaration '%s' already generated";
  static final String DUPLICATE_IMPL_ERR =
      "default implementation for type '%s' is already set as '%s'";
  static final String DUPLICATE_IMPL_WITH_PROFILE_ERR =
      "default implementation for type '%s' is already set as '%s' for profile '%s'";
  static final String PROFILE_NOT_CONFIGURED_ERR = "profile '%s' is not configured";
  private final TranslatorConfig translatorConfig;
  private final Map<String, PageObjectDeclaration> generated = new HashMap<>();
  private final Map<Profile, Map<String, String>> profilesMapping = new HashMap<>();
  private final Profile defaultProfile;
  // max number of POs to generate for generator performance measurements
  private int maxPageObjectsCounter = Integer.MAX_VALUE;

  public DefaultTranslatorRunner(TranslatorConfig translatorConfig) {
    this.translatorConfig = translatorConfig;
    for (ProfileConfiguration configuration : translatorConfig.getConfiguredProfiles()) {
      for (String value : configuration.getSupportedValues()) {
        Profile profile = configuration.getFromString(value);
        profilesMapping.put(profile, new HashMap<>());
      }
    }
    defaultProfile = DEFAULT_PROFILE;
    profilesMapping.put(defaultProfile, new HashMap<>());
  }

  final Collection<Profile> getAllProfiles() {
    return profilesMapping.keySet();
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
        write(pageObjectInterface.getInterfaceType(), pageObjectInterface.getApiCode());
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
        write(object.getImplementation().getClassType(), object.getImplementation().getImplCode());
        filesCounter++;
        if (writeUnitTest(object.getImplementation())) {
          filesCounter++;
        }
      }
      counter++;
    }
    info(String.format("wrote %d files for %d page objects, took %d msec", filesCounter, counter,
        System.currentTimeMillis() - timer));
  }

  // set max number of POs to generate for generator performance measurements
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
    sourceConfig.recursiveScan();
    for (String pageObjectURI : sourceConfig.getPageObjects()) {
      if (counter >= maxPageObjectsCounter) {
        break;
      }
      info(String.format("de-serialize Page Object %s", pageObjectURI));
      PageObjectDeclaration object =
          new JsonDeserializer(translatorConfig, sourceConfig, pageObjectURI).getObject();
      setPageObject(pageObjectURI, object);
      counter++;
    }
    info(String.format("generated %d page objects, took %d msec", counter,
        System.currentTimeMillis() - timer));
  }

  private String getResourcesRoot() {
    String profilesRoot = getTargetConfig().getInjectionConfigRootFilePath();
    if (profilesRoot == null || profilesRoot.isEmpty()) {
      throw new UtamError(ERR_PROFILE_PATH_NOT_CONFIGURED);
    }
    if (!new File(profilesRoot).exists()) {
      throw new UtamError(String.format(ERR_PROFILE_PATH_DOES_NOT_EXIST, profilesRoot));
    }
    return profilesRoot;
  }

  @Override
  public void writeDependenciesConfigs() {
    String profilesRoot = getResourcesRoot();
    String moduleName = translatorConfig.getModuleName();
    for (Profile profile : getAllProfiles()) {
      Properties configToWrite = getProfileMapping(profile);
      if (!configToWrite.isEmpty()) {
        String profileConfigPath = String
            .format("%s/%s.properties", profilesRoot, profile.getConfigName(moduleName));
        try {
          Writer writer = new FileWriter(profileConfigPath);
          configToWrite.store(writer, "profile configuration");
        } catch (IOException e) {
          throw new UtamCompilationError(
              String.format("error while writing profile configuration '%s'", profileConfigPath),
              e);
        }
      }
    }
  }

  final Properties getProfileMapping(Profile profile) {
    if (!profilesMapping.containsKey(profile)) {
      throw new UtamError(String.format(PROFILE_NOT_CONFIGURED_ERR, profile.toString()));
    }
    Properties properties = new Properties();
    profilesMapping.get(profile).forEach(properties::setProperty);
    return properties;
  }

  // used from tests and during compilation
  final void setPageObject(String name, PageObjectDeclaration object) {
    if (generated.containsKey(name)) {
      throw new UtamError(String.format(DUPLICATE_PAGE_OBJECT_NAME, name));
    }
    generated.put(name, object);
    String typeName = object.getInterface().getInterfaceType().getFullName();
    // interface only
    if (object.isInterfaceOnly()) {
      setInterfaceOnly(typeName);
      return;
    }
    String classTypeName = object.getImplementation().getClassType().getFullName();
    // default case - same file has both api and impl
    if (object.isClassWithInterface()) {
      // no profiles are set, it's default case, no need to do anything
      return;
    }
    // class implements other interface and does not have profiles
    if (!object.isClassWithProfiles()) {
      setImplOnly(typeName, classTypeName);
      return;
    }
    // class that implements other interface and has profiles
    for (Profile profile : object.getImplementation().getProfiles()) {
      setImplOnlyForProfile(profile, typeName, classTypeName);
    }
  }

  private void setInterfaceOnly(String typeName) {
    Map<String, String> implMapping = profilesMapping.get(defaultProfile);
    if (implMapping.containsKey(typeName)) {
      throw new UtamError(String.format(DUPLICATE_PAGE_OBJECT_NAME, typeName));
    }
    implMapping.put(typeName, "");
  }

  private void setImplOnly(String typeName, String classTypeName) {
    Map<String, String> implMapping = profilesMapping.get(defaultProfile);
    if (implMapping.containsKey(typeName) && !implMapping.get(typeName).isEmpty()) {
      throw new UtamError(String.format(DUPLICATE_IMPL_ERR, typeName, implMapping.get(typeName)));
    }
    implMapping.put(typeName, classTypeName);
  }

  void setImplOnlyForProfile(Profile profile, String typeName, String classTypeName) {
    if (!profilesMapping.containsKey(profile)) {
      throw new UtamError(String.format(PROFILE_NOT_CONFIGURED_ERR, profile.toString()));
    }
    if (profilesMapping.get(profile).containsKey(typeName)) {
      throw new UtamError(
          String.format(
              DUPLICATE_IMPL_WITH_PROFILE_ERR,
              typeName,
              profilesMapping.get(profile).get(typeName),
              profile.toString()));
    }
    profilesMapping.get(profile).put(typeName, classTypeName);
  }
}
