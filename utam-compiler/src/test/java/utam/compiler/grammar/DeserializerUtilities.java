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
import utam.core.declarative.translator.TranslatorConfig;
import utam.core.framework.consumer.UtamError;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.stream.Collectors;

import static utam.compiler.grammar.TestUtilities.TEST_URI;
import static utam.compiler.translator.TranslatorMockUtilities.getDefaultConfig;

/**
 * deserialize json from resources folder
 *
 * @author elizaveta.ivanova
 * @since 232
 */
public class DeserializerUtilities {

  private final TranslatorConfig translatorConfig;
  private final String type;

  public DeserializerUtilities() {
    this.type = TEST_URI;
    this.translatorConfig = getDefaultConfig();
  }

  private static String readJSON(String fileName) {
    InputStream stream =
        DeserializerUtilities.class.getClassLoader().getResourceAsStream(fileName + ".json");
    return new BufferedReader(new InputStreamReader(stream))
        .lines()
        .parallel()
        .collect(Collectors.joining("\n"));
  }

  Result getResultFromFile(String fileName) {
    String content = readJSON(fileName);
    return getResultFromString(content);
  }

  public Result getResultFromString(String content) {
    JsonDeserializer deserializer = new JsonDeserializer(type, content, translatorConfig);
    return new Result(deserializer.getObject(), deserializer.getContext());
  }

  TranslationContext getContext(String fileName) {
    return getResultFromFile(fileName).getContext();
  }

  static <T> T getObjectFromFile(String fileName, Class<T> tClass) {
    try {
      return JsonDeserializer.deserialize(tClass, readJSON(fileName));
    } catch (IOException e) {
      throw new UtamError("error", e);
    }
  }

  public static class Result {
    private final PageObjectDeclaration pageObjectDeclaration;
    private final TranslationContext translationContext;

    private Result(
        PageObjectDeclaration pageObjectDeclaration, TranslationContext translationContext) {
      this.pageObjectDeclaration = pageObjectDeclaration;
      this.translationContext = translationContext;
    }

    public PageObjectDeclaration getPageObject() {
      return pageObjectDeclaration;
    }

    public TranslationContext getContext() {
      return translationContext;
    }
  }
}
