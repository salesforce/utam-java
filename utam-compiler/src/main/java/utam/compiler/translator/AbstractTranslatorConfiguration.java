/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.translator;

import utam.core.framework.consumer.UtamError;
import utam.core.declarative.translator.*;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Objects;

public class AbstractTranslatorConfiguration implements TranslatorConfig {

  private final List<ProfileConfiguration> profileConfigurations = new ArrayList<>();
  private final List<TranslatorSourceConfig> translatorSourceConfigs = new ArrayList<>();
  private TranslationTypesConfig translatorTypesConfigNullable;
  private UnitTestRunner unitTestRunnerTypeNullable;
  private final TranslatorTargetConfig translatorTargetConfig;

  public static final String ERR_PROFILE_NOT_CONFIGURED = "profile '%s' is not configured";

  protected AbstractTranslatorConfiguration(TranslatorTargetConfig translatorTargetConfig) {
    this.translatorTargetConfig = translatorTargetConfig;
  }

  protected final void setUnitTestRunner(UnitTestRunner testRunnerType) {
    this.unitTestRunnerTypeNullable = testRunnerType;
  }

  final void setTranslatorTypesConfig(TranslationTypesConfig translatorTypesConfig) {
    this.translatorTypesConfigNullable = translatorTypesConfig;
  }

  @Override
  public UnitTestRunner getUnitTestRunnerType() {
    if(unitTestRunnerTypeNullable == null) {
      unitTestRunnerTypeNullable = UnitTestRunner.NONE;
    }
    return unitTestRunnerTypeNullable;
  }

  @Override
  public void setConfiguredProfile(ProfileConfiguration profileConfig) {
    profileConfigurations.add(profileConfig);
  }

  @Override
  public Collection<ProfileConfiguration> getConfiguredProfiles() {
    return profileConfigurations;
  }

  @Override
  public TranslationTypesConfig getTranslationTypesConfig() {
    if(translatorTypesConfigNullable == null) {
      translatorTypesConfigNullable = new TranslationTypesConfigJava();
    }
    return translatorTypesConfigNullable;
  }

  @Override
  public void setSourceConfig(TranslatorSourceConfig translatorSourceConfig) {
    translatorSourceConfigs.add(translatorSourceConfig);
  }

  @Override
  public Collection<TranslatorSourceConfig> getConfiguredSources() {
    return translatorSourceConfigs;
  }

  @Override
  public TranslatorTargetConfig getConfiguredTarget() {
    return translatorTargetConfig;
  }

  @Override
  public ProfileConfiguration getProfileConfiguration(String jsonKey) {
    return profileConfigurations.stream()
                    .filter(configuration -> configuration.getPropertyKey().equals(jsonKey))
                    .findAny()
                    .orElseThrow(() -> new UtamError(String.format(ERR_PROFILE_NOT_CONFIGURED, jsonKey)));
  }

  public static String getHomeDirectory(Class loader, String moduleName) {
    String current = Objects.requireNonNull(loader.getClassLoader().getResource("")).getPath();
    int index = current.lastIndexOf(moduleName);
    if (index < 0) {
      throw new UtamError(
              String.format(
                      "cant find module '%s' in path '%s' for loader class %s",
                      moduleName, current, loader.getName()));
    }
    return current.substring(0, current.lastIndexOf(moduleName));
  }
}
