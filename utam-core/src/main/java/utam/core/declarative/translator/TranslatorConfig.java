/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.declarative.translator;

import java.util.Collection;

/**
 * configuration for translator
 *
 * @author elizaveta.ivanova
 * @since 226
 */
public interface TranslatorConfig {

  /**
   * get all configured profiles for translator
   *
   * @return collection of configured profiles
   */
  Collection<ProfileConfiguration> getConfiguredProfiles();

  /**
   * get configured mapping for Page Object URIs to be translated to Java packages
   *
   * @return configuration
   */
  TranslationTypesConfig getTranslationTypesConfig();

  /**
   * there could be multiple sources of configured JSON files
   *
   * @return all configured sources
   */
  TranslatorSourceConfig getConfiguredSource();

  /**
   * get configured target for generated files
   *
   * @return configured target
   */
  TranslatorTargetConfig getConfiguredTarget();

  /**
   * name of the module for all page objects from this source
   *
   * @return string with module name
   */
  String getModuleName();

  /**
   * translator is configured to set guardrails mode
   *
   * @return type of validations
   */
  GuardrailsMode getValidationMode();

  /**
   * version of the page objects to add to the JavaDoc
   *
   * @return not nullable string
   */
  String getPageObjectsVersion();
}
