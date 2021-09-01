/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import static utam.compiler.grammar.JsonDeserializer.getDeserializerMapper;
import static utam.compiler.grammar.TestUtilities.TEST_URI;
import static utam.compiler.translator.TranslatorMockUtilities.getDefaultConfig;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.type.CollectionType;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.List;
import java.util.stream.Collectors;
import utam.compiler.helpers.TranslationContext;
import utam.compiler.translator.ClassSerializer;
import utam.compiler.translator.InterfaceSerializer;
import utam.core.declarative.representation.PageObjectDeclaration;
import utam.core.declarative.translator.TranslatorConfig;
import utam.core.framework.consumer.UtamError;

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
    String testFileName = fileName + ".json";
    InputStream stream =
        DeserializerUtilities.class.getClassLoader().getResourceAsStream(testFileName);
    if (stream == null) {
      throw new AssertionError(String.format("JSON file '%s' not found", testFileName));
    }
    return new BufferedReader(new InputStreamReader(stream))
        .lines()
        .parallel()
        .collect(Collectors.joining("\n"));
  }

  TranslatorConfig getTranslatorConfig() {
    return translatorConfig;
  }

  Result getResultFromFile(String fileName) {
    String content = readJSON(fileName);
    return getResultFromString(content);
  }

  public Result getResultFromString(String content) {
    JsonDeserializer deserializer = new JsonDeserializer(type, content, translatorConfig);
    PageObjectDeclaration declaration = deserializer.getObject();
    // to ensure that code can be generated
    new InterfaceSerializer(declaration.getInterface()).toString();
    if(!declaration.isInterfaceOnly()) {
      new ClassSerializer(declaration.getImplementation(), deserializer.getPageObjectContext())
          .toString();
    }
    return new Result(deserializer.getObject(), deserializer.getPageObjectContext());
  }

  public TranslationContext getContext(String fileName) {
    return getResultFromFile(fileName).getContext();
  }

  static <T> T getObjectFromFile(String fileName, Class<T> tClass) {
    try {
      return JsonDeserializer.deserialize(tClass, readJSON(fileName));
    } catch (IOException e) {
      throw new UtamError("error", e);
    }
  }

  /**
   * read JSON file with array of objects and deserialize
   * @param contentType type of objects in json
   * @param fileName name of the file without extension
   * @return list of deserialized objects
   */
  static <T> List<T> getDeserializedObjects(Class<T> contentType, String fileName) {
    ArrayResult<T> res = new ArrayResult(contentType);
    return res.getObjects(fileName);
  }

  static class ArrayResult<T> {
    private final Class<T> type;

    ArrayResult(Class<T> type) {
      this.type = type;
    }

    List<T> getObjects(String fileName) {
      try {
        String fullFileName = fileName + ".json";
        ObjectMapper mapper = getDeserializerMapper();
        CollectionType collectionType = mapper.getTypeFactory()
            .constructCollectionType(List.class, type);
        InputStream stream = getClass().getClassLoader().getResourceAsStream(fullFileName);
        if(stream == null) {
          throw new AssertionError(String.format("JSON file '%s' not found", fullFileName));
        }
        return mapper.readValue(stream, collectionType);
      } catch (Exception e) {
        throw new UtamError("error in " + fileName, e);
      }
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
