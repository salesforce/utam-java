/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import static com.fasterxml.jackson.core.JsonParser.Feature.ALLOW_COMMENTS;
import static com.fasterxml.jackson.core.JsonParser.Feature.STRICT_DUPLICATE_DETECTION;
import static com.fasterxml.jackson.databind.DeserializationFeature.ACCEPT_SINGLE_VALUE_AS_ARRAY;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.module.SimpleModule;
import java.io.IOException;
import java.util.List;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Collectors;
import utam.compiler.UtamCompilationError;
import utam.compiler.UtamCompilerIntermediateError;
import utam.compiler.helpers.TranslationContext;
import utam.compiler.translator.ClassSerializer;
import utam.compiler.translator.InterfaceSerializer;
import utam.compiler.translator.UnitTestSerializer;
import utam.core.declarative.representation.AnnotationProvider;
import utam.core.declarative.representation.MethodDeclaration;
import utam.core.declarative.representation.PageClassField;
import utam.core.declarative.representation.PageObjectClass;
import utam.core.declarative.representation.PageObjectDeclaration;
import utam.core.declarative.representation.PageObjectInterface;
import utam.core.declarative.representation.PageObjectMethod;
import utam.core.declarative.representation.TypeProvider;
import utam.core.declarative.representation.UnionType;
import utam.core.declarative.translator.UnitTestRunner;
import utam.core.framework.context.Profile;

/**
 * entry point for JSON deserialization
 *
 * @author elizaveta.ivanova
 * @since 228
 */
public final class JsonDeserializer extends
    com.fasterxml.jackson.databind.JsonDeserializer<UtamPageObject> {

  private final UtamPageObject utamPageObject;
  private final TranslationContext context;

  /**
   * Initializes a new instance of the JsonDeserializer class
   *
   * @param jsonString         the JSON string with content of the Page Object
   * @param translationContext context of the Page Object
   */
  public JsonDeserializer(TranslationContext translationContext, String jsonString) {
    this.context = translationContext;
    ObjectMapper mapper = getObjectMapperWithSettings();
    SimpleModule module = new SimpleModule();
    module.addDeserializer(UtamPageObject.class, this);
    mapper.registerModule(module);
    try {
      this.utamPageObject = mapper.readValue(jsonString, UtamPageObject.class);
    } catch (IOException e) {
      // this is never actually thrown
      throw new IllegalStateException(e);
    }
  }

  private static ObjectMapper getObjectMapperWithSettings() {
    ObjectMapper mapper = new ObjectMapper();
    // parser options
    mapper.enable(ALLOW_COMMENTS);
    mapper.enable(ACCEPT_SINGLE_VALUE_AS_ARRAY);
    mapper.enable(STRICT_DUPLICATE_DETECTION);
    return mapper;
  }

  /**
   * traverse exception stack to find compiler error thrown by internal parsers
   *
   * @param error thrown error
   * @return exception cause or null
   */
  private static RuntimeException unwrapError(Throwable error) {
    Throwable e = error.getCause();
    while (e != null) {
      if (e instanceof UtamCompilerIntermediateError) {
        return (UtamCompilerIntermediateError) e;
      }
      if (e instanceof UtamCompilationError) {
        return (UtamCompilationError) e;
      }
      e = e.getCause();
    }
    return null;
  }

  /**
   * simple parser to deserialize json node
   *
   * @param node node to deserialize
   * @param type type to deserialize
   * @param <T>  generic type parameter
   * @return de-serialized object
   */
  static <T> T readNode(JsonNode node, Class<T> type, Function<Exception, RuntimeException> err) {
    ObjectMapper mapper = getObjectMapperWithSettings();
    if (node == null || node.isNull()) {
      return null;
    }
    try {
      return mapper.readValue(node.toString(), type);
    } catch (UtamCompilationError e) {
      throw e;
    } catch (UtamCompilerIntermediateError e) {
      throw e.addJsonSource(node).get();
    } catch (Exception e) {
      RuntimeException runtimeException = unwrapError(e);
      if (runtimeException != null) { //from parser of inner elements
        throw runtimeException;
      }
      throw err.apply(e);
    }
  }

  /**
   * if error is thrown by compiler after parser, process it and use error codes
   *
   * @param context translation context
   * @param parser  json parser
   * @param error   error thrown from parser
   * @return object with error message
   */
  private static Supplier<UtamCompilationError> processErrorMessage(TranslationContext context,
      JsonParser parser, Throwable error) {
    RuntimeException compilerErrorOrNull = unwrapError(error);
    if (compilerErrorOrNull != null) {
      if (compilerErrorOrNull instanceof UtamCompilationError) {
        return () -> (UtamCompilationError) compilerErrorOrNull;
      }
      if (compilerErrorOrNull instanceof UtamCompilerIntermediateError) {
        return () -> ((UtamCompilerIntermediateError) compilerErrorOrNull)
            .getCompilationError(context, parser, error).get();
      }
    }
    if (error instanceof JsonProcessingException) {
      String errSplitMarker = "problem:";
      String message = error.getMessage();
      int index = message.contains(errSplitMarker) ? message.indexOf(errSplitMarker)
          + errSplitMarker.length() : 0;
      String errorStr = context.getErrorMessage("UPO000", message.substring(index));
      return () -> new UtamCompilationError(parser, errorStr, error);
    }
    return () -> new UtamCompilationError(parser, context.getRawErrorMessage(error.getMessage()),
        error);
  }

  @Override
  public UtamPageObject deserialize(JsonParser parser, DeserializationContext ctxt) {
    try {
      ObjectMapper mapper = getObjectMapperWithSettings();
      UtamPageObject utamPageObject = mapper.readValue(parser, UtamPageObject.class);
      utamPageObject.compile(this.context, parser);
      this.context.guardrailsValidation();
      return utamPageObject;
    } catch (UtamCompilationError e) {
      throw e;
    } catch (UtamCompilerIntermediateError e) {
      throw e.getCompilationError(context, parser, e.getCause()).get();
    } catch (Exception e) {
      throw processErrorMessage(context, parser, e).get();
    }
  }

  /**
   * Gets the Page Object translation context
   *
   * @return the Page Object translation context
   */
  public final TranslationContext getPageObjectContext() {
    return context;
  }

  /**
   * Gets the Page Object declaration
   *
   * @return the Page Object declaration
   */
  public PageObjectDeclaration getObject() {
    return new Object(utamPageObject, context);
  }

  static class Object implements PageObjectDeclaration {

    private final UtamPageObject utamPageObject;
    private final PageObjectInterface pageObjectInterface;
    private final PageObjectClass pageObjectClass;

    Object(UtamPageObject utamPageObject, TranslationContext context) {
      this.utamPageObject = utamPageObject;
      this.pageObjectInterface = new Interface(context, utamPageObject);
      this.pageObjectClass = new Implementation(context, utamPageObject, pageObjectInterface);
    }

    @Override
    public boolean isInterfaceOnly() {
      return utamPageObject.isAbstract;
    }

    @Override
    public boolean isClassWithInterface() {
      return utamPageObject.implementsType == null;
    }

    @Override
    public boolean isClassWithProfiles() {
      return !pageObjectClass.getProfiles().isEmpty();
    }

    @Override
    public PageObjectClass getImplementation() {
      return pageObjectClass;
    }

    @Override
    public PageObjectInterface getInterface() {
      return pageObjectInterface;
    }
  }

  static class Interface implements PageObjectInterface {

    private final TranslationContext context;
    private final UtamPageObject utamPageObject;
    private final TypeProvider implementedType;

    Interface(TranslationContext context, UtamPageObject utamPageObject) {
      this.context = context;
      this.utamPageObject = utamPageObject;
      this.implementedType = context.getSelfType();
    }

    @Override
    public List<MethodDeclaration> getDeclaredApi() {
      return context.getMethods().stream()
          .filter(PageObjectMethod::isPublic)
          .map(PageObjectMethod::getDeclaration)
          .collect(Collectors.toList());
    }

    @Override
    public TypeProvider getInterfaceType() {
      return implementedType;
    }

    @Override
    public TypeProvider getBaseInterfaceType() {
      return utamPageObject.getBaseType();
    }

    @Override
    public String getGeneratedCode() {
      return new InterfaceSerializer(this).toString();
    }

    @Override
    public List<String> getDescription() {
      return utamPageObject.getDescription(context);
    }

    @Override
    public List<UnionType> getUnionTypes() {
      return context.getInterfaceUnionTypes();
    }

    @Override
    public boolean isDeprecated() {
      return utamPageObject.isDeprecated();
    }

    @Override
    public List<String> getCopyright() {
      return context.getCopyright();
    }
  }

  static class Implementation implements PageObjectClass {

    private final TranslationContext context;
    private final UtamPageObject utamPageObject;
    private final PageObjectInterface pageObjectInterface;
    private final List<Profile> profiles;

    Implementation(
        TranslationContext context,
        UtamPageObject utamPageObject,
        PageObjectInterface pageObjectInterface) {
      this.context = context;
      this.utamPageObject = utamPageObject;
      this.pageObjectInterface = pageObjectInterface;
      this.profiles = utamPageObject.getProfiles(context);
    }

    @Override
    public List<PageClassField> getFields() {
      return context.getFields();
    }

    @Override
    public List<PageObjectMethod> getMethods() {
      return context.getMethods();
    }

    @Override
    public List<AnnotationProvider> getClassAnnotations() {
      return utamPageObject.getAnnotations();
    }

    @Override
    public TypeProvider getClassType() {
      return context.getClassType();
    }

    @Override
    public TypeProvider getBaseClassType() {
      return utamPageObject.getBaseClass();
    }

    @Override
    public String getGeneratedCode() {
      return new ClassSerializer(this).toString();
    }

    @Override
    public String getGeneratedUnitTestCode(UnitTestRunner unitTestRunner) {
      return new UnitTestSerializer(this, this.context, unitTestRunner).toString();
    }

    @Override
    public PageObjectInterface getImplementedType() {
      return pageObjectInterface;
    }

    @Override
    public List<Profile> getProfiles() {
      return this.profiles;
    }

    @Override
    public List<String> getDescription() {
      return utamPageObject.getDescription(context);
    }

    @Override
    public List<UnionType> getUnionTypes() {
      return context.getClassUnionTypes();
    }

    @Override
    public List<String> getCopyright() {
      return context.getCopyright();
    }
  }
}
