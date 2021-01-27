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
   * set profile configuration for translator
   *
   * @param profileConfig configuration
   */
  void setConfiguredProfile(ProfileConfiguration profileConfig);

  /**
   * get all configured profiles for translator
   *
   * @return collection of configured profiles
   */
  Collection<ProfileConfiguration> getConfiguredProfiles();

  /**
   * get profile configuration for JSON property name
   * @param jsonKey JSON property name
   * @return profile configuration
   */
  ProfileConfiguration getProfileConfiguration(String jsonKey);

  /**
   * get configured type for unit test generator
   *
   * @return type of the unit tests to generate
   */
  UnitTestRunner getUnitTestRunnerType();

  /**
   * get configured mapping for Page Object URIs to be translated to Java packages
   *
   * @return configuration
   */
  TranslationTypesConfig getTranslationTypesConfig();

  /**
   * set configured source with JSON files
   *
   * @param translatorSourceConfig translator source config (one of many)
   */
  void setSourceConfig(TranslatorSourceConfig translatorSourceConfig);

  /**
   * there could be multiple sources of configured JSON files
   *
   * @return all configured sources
   */
  Collection<TranslatorSourceConfig> getConfiguredSources();

  /**
   * get configured target for generated files
   *
   * @return configured target
   */
  TranslatorTargetConfig getConfiguredTarget();
}
