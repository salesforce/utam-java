/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import static org.testng.Assert.expectThrows;
import static utam.compiler.grammar.TestUtilities.TEST_URI;
import static utam.compiler.grammar.TestUtilities.getDefaultConfig;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URL;
import java.util.stream.Collectors;
import utam.compiler.UtamCompilationError;
import utam.compiler.grammar.TestUtilities.TestTranslationContext;
import utam.compiler.helpers.TranslationContext;
import utam.compiler.translator.ClassSerializer;
import utam.compiler.translator.InterfaceSerializer;
import utam.compiler.translator.StringValueProfileConfig;
import utam.core.declarative.representation.PageObjectDeclaration;
import utam.core.declarative.translator.TranslatorConfig;
import utam.core.framework.UtamLogger;

/**
 * test utilities to deserialize json
 *
 * @author elizaveta.ivanova
 * @since 232
 */
public class DeserializerUtilities {

  private final TranslatorConfig translatorConfig;
  private final String type;

  public DeserializerUtilities(String pageObjectName) {
    this.type = pageObjectName;
    this.translatorConfig = getDefaultConfig();
    // profile is required for implementations POs
    this.translatorConfig
        .getConfiguredProfiles()
        .add(new StringValueProfileConfig("profile", "test"));
  }

  public DeserializerUtilities() {
    this(TEST_URI);
  }

  static UtamCompilationError expectCompilerError(String json) {
    return expectThrows(
        UtamCompilationError.class, () -> new DeserializerUtilities().getResultFromString(json));
  }

  static UtamCompilationError expectCompilerErrorFromFile(String fileName) {
    return expectThrows(
        UtamCompilationError.class, () -> new DeserializerUtilities().getContext(fileName));
  }

  TranslatorConfig getTranslatorConfig() {
    return translatorConfig;
  }

  private static String getJsonString(String fileName) {
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

  public TranslationContext getContextWithPath(String fileName) {
    String testFileName = fileName + ".json";
    URL url = DeserializerUtilities.class.getClassLoader().getResource(testFileName);
    if (url == null) {
      throw new AssertionError(String.format(" File %s does not exist!", testFileName));
    }
    String path = url.getFile();
    String content = getJsonString(fileName);
    try {
      return getResultFromString(content, path).translationContext;
    } catch (Exception e) {
      UtamLogger.error(String.format("ERROR IN FILE %s", fileName));
      throw e;
    }
  }

  public Result getResultFromFile(String fileName) {
    String content = getJsonString(fileName);
    try {
      return getResultFromString(content);
    } catch (Exception e) {
      UtamLogger.error(String.format("ERROR IN FILE %s", fileName));
      throw e;
    }
  }

  Result getResultFromString(String content, String path) {
    TranslationContext context = new TranslationContext(type, path, translatorConfig);
    // to test implementations
    translatorConfig.getConfiguredProfiles().add(new StringValueProfileConfig("name", "value"));
    JsonDeserializer deserializer = new JsonDeserializer(context, content);
    PageObjectDeclaration declaration = deserializer.getObject();
    // to ensure that code can be generated
    new InterfaceSerializer(declaration.getInterface()).toString();
    if (!declaration.isInterfaceOnly()) {
      new ClassSerializer(declaration.getImplementation()).toString();
    }
    return new Result(deserializer.getObject(), deserializer.getPageObjectContext());
  }

  public Result getResultFromString(String content) {
    TranslationContext context = new TestTranslationContext(type, translatorConfig);
    // to test implementations
    translatorConfig.getConfiguredProfiles().add(new StringValueProfileConfig("name", "value"));
    JsonDeserializer deserializer = new JsonDeserializer(context, content);
    PageObjectDeclaration declaration = deserializer.getObject();
    // to ensure that code can be generated
    new InterfaceSerializer(declaration.getInterface()).toString();
    if (!declaration.isInterfaceOnly()) {
      new ClassSerializer(declaration.getImplementation()).toString();
    }
    return new Result(deserializer.getObject(), deserializer.getPageObjectContext());
  }

  public TranslationContext getContext(String fileName) {
    return getResultFromFile(fileName).getContext();
  }

  /**
   * utility class to combine de-serialization results
   *
   * @author elizaveta.ivanova
   * @since 232
   */
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
