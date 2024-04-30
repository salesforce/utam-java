/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import static utam.compiler.translator.DefaultTranslatorConfiguration.CompilerOutputOptions.DEFAULT_COMPILER_OUTPUT_OPTIONS;

import java.util.ArrayList;
import utam.compiler.helpers.TranslationContext;
import utam.compiler.translator.CompilerErrors;
import utam.compiler.translator.DefaultSourceConfigurationTests;
import utam.compiler.translator.DefaultTargetConfigurationTests;
import utam.compiler.translator.DefaultTranslatorConfiguration;
import utam.compiler.translator.TranslationTypesConfigJava;
import utam.core.declarative.representation.PageClassField;
import utam.core.declarative.representation.PageObjectDeclaration;
import utam.core.declarative.representation.TypeProvider;
import utam.core.declarative.translator.TranslationTypesConfig;
import utam.core.declarative.translator.TranslatorConfig;
import utam.core.declarative.translator.TranslatorSourceConfig;
import utam.core.declarative.translator.TranslatorTargetConfig;
import utam.core.element.Locator;
import utam.core.selenium.element.LocatorBy;

/**
 * test utilities
 *
 * @author elizaveta.ivanova
 * @since 230
 */
public class TestUtilities {

  public static final String TEST_URI = "utam-test/pageObjects/test/test";
  private static final TranslationTypesConfig TYPES_CONFIG = new TranslationTypesConfigJava();
  public static final TypeProvider TEST_PAGE_OBJECT = TYPES_CONFIG.getInterfaceType(TEST_URI);

  public static TranslationContext getTestTranslationContext() {
    return new TestTranslationContext(getDefaultConfig());
  }

  public static Locator getCssSelector(String value) {
    return LocatorBy.byCss(value);
  }

  public static JsonDeserializer getJsonStringDeserializer(String json) {
    return getJsonStringDeserializer(json, getDefaultConfig());
  }

  public static JsonDeserializer getJsonStringDeserializer(
      String json, TranslatorConfig translatorConfig) {
    TranslationContext translationContext = new TestTranslationContext(translatorConfig);
    return new JsonDeserializer(translationContext, json);
  }

  public static PageObjectDeclaration getPageObject(String json) {
    TranslationContext translationContext = getTestTranslationContext();
    JsonDeserializer deserializer = new JsonDeserializer(translationContext, json);
    return deserializer.getObject();
  }

  public static DefaultTranslatorConfiguration getDefaultConfig() {
    TranslatorTargetConfig targetConfig = new DefaultTargetConfigurationTests.Mock();
    TranslatorSourceConfig sourceConfig = new DefaultSourceConfigurationTests.Mock();
    return new DefaultTranslatorConfiguration(
        DEFAULT_COMPILER_OUTPUT_OPTIONS,
        null,
        new CompilerErrors.Throws(),
        sourceConfig,
        targetConfig,
        new ArrayList<>());
  }

  static class TestTranslationContext extends TranslationContext {

    TestTranslationContext(TranslatorConfig translatorConfiguration) {
      super(TEST_URI, TEST_URI, translatorConfiguration);
    }

    TestTranslationContext(String name, TranslatorConfig translatorConfiguration) {
      super(name, name, translatorConfiguration);
    }
  }

  /**
   * Utility to find declared field with a given name
   *
   * @param context translation context
   * @param fieldName name of the field
   * @return field object
   * @throws AssertionError if field is not found
   */
  public static PageClassField findField(TranslationContext context, String fieldName) {
    return context.getFields().stream()
        .filter(f -> f.getName().equals(fieldName))
        .findAny()
        .orElseThrow(
            () -> new AssertionError(String.format("Can't find field with name '%s'!", fieldName)));
  }
}
