/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import utam.compiler.helpers.TranslationContext;
import utam.core.declarative.representation.PageObjectDeclaration;
import utam.core.declarative.representation.TypeProvider;
import utam.core.declarative.translator.TranslationTypesConfig;
import utam.compiler.translator.TranslationTypesConfigJava;
import utam.core.declarative.translator.TranslatorConfig;
import utam.core.framework.consumer.UtamError;
import utam.core.selenium.element.Selector;

import java.io.IOException;

import static utam.compiler.translator.TranslationUtilities.getElementGetterMethodName;
import static utam.compiler.translator.TranslatorMockUtilities.getDefaultConfig;

public class TestUtilities {

  public static final String TEST_URI = "utam-test/pageObjects/test/test";
  private static final TranslationTypesConfig TRANSLATION_TYPES_CONFIG = new TranslationTypesConfigJava();
  static final String TEST_URI_PROCESSED = TRANSLATION_TYPES_CONFIG.getInterfaceType(TEST_URI).getFullName();
  static final String JACKSON_MISSING_REQUIRED_PROPERTY_ERROR = "Missing required creator property";
  static final String JACKSON_WRONG_PROPERTY_TYPE =
      "Cannot deserialize value of type `%s` from String \"%s\"";
  private static final TranslationTypesConfig TYPES_CONFIG = new TranslationTypesConfigJava();
  public static final TypeProvider TEST_PAGE_OBJECT = TYPES_CONFIG.getInterfaceType(TEST_URI);

  public static TranslationContext getTestTranslationContext() {
    return new TranslationContext(TEST_URI, getDefaultConfig());
  }

  static <T> T getDeserializedObject(String json, Class<T> tClass) {
    try {
      return JsonDeserializer.deserialize(tClass, json);
    } catch (IOException e) {
      throw new UtamError("error", e);
    }
  }

  public static Selector getCssSelector(String value) {
    return Selector.byCss(value);
  }

  public static JsonDeserializer getJsonStringDeserializer(String json) {
    return new JsonDeserializer(TEST_URI, json, getDefaultConfig());
  }

  public static JsonDeserializer getJsonStringDeserializer(
      String json, TranslatorConfig translatorConfig) {
    return new JsonDeserializer(TEST_URI, json, translatorConfig);
  }

  public static PageObjectDeclaration getPageObject(String json) {
    return new JsonDeserializer.Object(
        getDeserializedObject(json, UtamPageObject.class), getTestTranslationContext());
  }

  public static String getElementPrivateMethodCalled(String name) {
    return "this." + getElementGetterMethodName(name, false);
  }

  static String getElementPrivateMethod(String name) {
    return getElementGetterMethodName(name, false);
  }

  static String getURIasTypeName(String pageObjectURI) {
    return TRANSLATION_TYPES_CONFIG.getInterfaceType(pageObjectURI).getFullName();
  }

  public static void compile(UtamElement utamElement, TranslationContext context) {
    UtamPageObject pageObject = new UtamPageObject();
    pageObject.elements = new UtamElement[] { utamElement };
    pageObject.compile(context);
  }
}
