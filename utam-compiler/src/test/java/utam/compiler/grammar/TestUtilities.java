/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.TextNode;
import utam.compiler.UtamCompilationError;
import utam.compiler.helpers.TranslationContext;
import utam.core.declarative.representation.PageObjectDeclaration;
import utam.core.declarative.representation.TypeProvider;
import utam.core.declarative.translator.TranslationTypesConfig;
import utam.compiler.translator.TranslationTypesConfigJava;
import utam.core.declarative.translator.TranslatorConfig;
import utam.core.element.Locator;

import java.io.IOException;
import java.util.Arrays;

import utam.core.selenium.element.LocatorBy;

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
  static final String JSON_MAPPING_ERROR = "Unrecognized field";

  public static TranslationContext getTestTranslationContext() {
    return new TranslationContext(TEST_URI, getDefaultConfig());
  }

  public static <T> T getDeserializedObject(String json, Class<T> tClass) {
    try {
      return JsonDeserializer.deserialize(tClass, json);
    } catch (IOException e) {
      throw new UtamCompilationError(e);
    }
  }

  public static Locator getCssSelector(String value) {
    return LocatorBy.byCss(value);
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

  public static class UtamEntityCreator {
    public static UtamElement createUtamElement(String name) {
      return createUtamElement(name, null);
    }

    public static UtamElement createUtamElement(String name, UtamSelector selector) {
      return createUtamElement(name, (String)null, selector);
    }

    public static UtamElement createUtamElement(
        String name, String type, UtamSelector selector) {
      return createUtamElement(name, type, selector, null);
    }

    public static UtamElement createUtamElement(
        String name, String type, UtamSelector selector, Boolean isNullable) {
      return new UtamElement(
          createStringTypeNode(type), name, false, isNullable, null, selector,
          null, null, null);
    }

    public static UtamElement createUtamElement(
        String name, String[] type, UtamSelector selector) {
      return createUtamElement(name, type, selector, null);
    }

    public static UtamElement createUtamElement(
        String name, String[] type, UtamSelector selector, Boolean isNullable) {
      return new UtamElement(
          createArrayTypeNode(type), name, false, isNullable, null, selector,
          null, null, null);
    }

    public static UtamMethod createUtamMethod(String name, UtamMethodAction[] compose) {
      return new UtamMethod(name, compose, null, null, null, null);
    }

    public static UtamMethod createUtamMethod(String name, String returns, UtamArgument[] args) {
      return new UtamMethod(
          name, null, null, args, createStringTypeNode(returns), null);
    }

    public static UtamMethod createUtamMethod(String name, String[] returns, UtamArgument[] args) {
      return new UtamMethod(
          name, null, null, args, createArrayTypeNode(returns), null);
    }

    public static UtamMethod createUtamMethod(
        String name, String returns, UtamMethodChainLink[] chain) {
      return new UtamMethod(
          name, null, chain, null, createStringTypeNode(returns), null);
    }

    private static JsonNode createStringTypeNode(String type) {
      if (type == null) {
        return null;
      }
      return new TextNode(type);
    }

    private static JsonNode createArrayTypeNode(String[] type) {
      if (type == null) {
        return null;
      }
      ArrayNode node = new ArrayNode(JsonNodeFactory.instance);
      Arrays.stream(type).forEach(node::add);
      return node;
    }
  }
}
