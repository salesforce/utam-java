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
import static utam.compiler.UtamCompilationError.processMapperError;
import static utam.compiler.grammar.UtamProfile.getConfiguredProfiles;
import static utam.compiler.helpers.AnnotationUtils.DEPRECATED_ANNOTATION;
import static utam.compiler.helpers.AnnotationUtils.getPageObjectAnnotation;
import static utam.compiler.helpers.AnnotationUtils.getPagePlatformAnnotation;
import static utam.compiler.helpers.TypeUtilities.BASE_PAGE_OBJECT_CLASS;
import static utam.compiler.helpers.TypeUtilities.BASE_ROOT_PAGE_OBJECT_CLASS;
import static utam.compiler.helpers.TypeUtilities.PAGE_OBJECT;
import static utam.compiler.helpers.TypeUtilities.ROOT_PAGE_OBJECT;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.module.SimpleModule;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import utam.compiler.UtamCompilationError;
import utam.compiler.UtamCompilationError.ErrorSupplier;
import utam.compiler.helpers.TranslationContext;
import utam.compiler.representation.JavadocObject;
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
public final class JsonDeserializer
    extends com.fasterxml.jackson.databind.JsonDeserializer<UtamPageObject> {

  private final UtamPageObject utamPageObject;
  private final TranslationContext context;

  /**
   * Initializes a new instance of the JsonDeserializer class
   *
   * @param jsonString the JSON string with content of the Page Object
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
    mapper.disable(ALLOW_COMMENTS);
    mapper.enable(ACCEPT_SINGLE_VALUE_AS_ARRAY);
    mapper.enable(STRICT_DUPLICATE_DETECTION);
    return mapper;
  }

  /**
   * Simple parser to deserialize json node. Called from constructor so does not have access to
   * context.
   *
   * @param node node to deserialize
   * @param type type to deserialize
   * @param <T> generic type parameter
   * @return de-serialized object
   */
  static <T> T readNode(JsonNode node, Class<T> type, String errorMessage) {
    if (isEmptyNode(node)) {
      return null;
    }
    ObjectMapper mapper = getObjectMapperWithSettings();
    try {
      return mapper.readValue(node.toString(), type);
    } catch (Exception e) {
      ErrorSupplier error = processMapperError(node, e, errorMessage);
      if (error.getCause() == null) {
        throw new UtamCompilationError(error.getMessage());
      }
      throw new UtamCompilationError(error.getMessage(), error.getCause());
    }
  }

  @Override
  public UtamPageObject deserialize(JsonParser parser, DeserializationContext ctxt) {
    try {
      ObjectMapper mapper = getObjectMapperWithSettings();

      // Maps the incoming JSON to a class, directly without processing
      UtamPageObject utamPageObject = mapper.readValue(parser, UtamPageObject.class);

      // Touch up the JSON, adjusting things like names for `waitFor`, etc.
      utamPageObject.preProcess();

      // Compile the page object now that it has been corrected
      utamPageObject.compile(this.context);
      return utamPageObject;
    } catch (Exception e) {
      this.context.processError(parser, e);
      // if we collect errors from all objects, need to proceed without writing anything
      return null;
    }
  }

  /**
   * Gets the Page Object translation context
   *
   * @return the Page Object translation context
   */
  public TranslationContext getPageObjectContext() {
    return context;
  }

  /**
   * Gets the Page Object declaration
   *
   * @return the Page Object declaration
   */
  public PageObjectDeclaration getObject() {
    if (utamPageObject == null) {
      // if we collect errors from all objects, need to proceed without writing anything
      return null;
    }
    return new Object(utamPageObject, context);
  }

  /**
   * utility method to check if node is empty
   *
   * @param node JSON node
   * @return true if null or empty
   */
  public static boolean isEmptyNode(JsonNode node) {
    return node == null || node.isNull();
  }

  /**
   * Return node value as a string for error messages. "toPrettyString()" can add extra quotes, so
   * remove them
   *
   * @param node json node
   * @return string representation
   */
  public static String nodeToString(JsonNode node) {
    if (isEmptyNode(node)) {
      return "null";
    }
    // remove extra quotes, otherwise error message can have ""weird double quoted string""
    return node.toPrettyString().replaceAll("^\"|\"$", "");
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
    private final JavadocObject description;

    Interface(TranslationContext context, UtamPageObject utamPageObject) {
      this.context = context;
      this.utamPageObject = utamPageObject;
      this.implementedType = context.getSelfType();
      this.description = utamPageObject.description.getDescription(context);
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
      return utamPageObject.isRootPageObject ? ROOT_PAGE_OBJECT : PAGE_OBJECT;
    }

    @Override
    public String getGeneratedCode() {
      return new InterfaceSerializer(this).toString();
    }

    @Override
    public List<String> getDescription() {
      return description.getJavadoc();
    }

    @Override
    public List<UnionType> getUnionTypes() {
      return context.getInterfaceUnionTypes();
    }

    @Override
    public boolean isDeprecated() {
      return description.isDeprecated();
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
    private final List<AnnotationProvider> annotations = new ArrayList<>();
    private final JavadocObject description;

    Implementation(
        TranslationContext context,
        UtamPageObject utamPageObject,
        PageObjectInterface pageObjectInterface) {
      this.context = context;
      this.utamPageObject = utamPageObject;
      this.pageObjectInterface = pageObjectInterface;
      this.profiles = getConfiguredProfiles(utamPageObject.profile, context);
      this.description = utamPageObject.description.getDescription(context);
      if (utamPageObject.rootLocator != null) {
        annotations.add(getPageObjectAnnotation(utamPageObject.rootLocator));
      }
      if (utamPageObject.platform != null) {
        annotations.add(getPagePlatformAnnotation(utamPageObject.platform));
      }
      if (description.isDeprecated()) {
        annotations.add(DEPRECATED_ANNOTATION);
      }
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
      return annotations;
    }

    @Override
    public TypeProvider getClassType() {
      return context.getClassType();
    }

    @Override
    public TypeProvider getBaseClassType() {
      return utamPageObject.isRootPageObject ? BASE_ROOT_PAGE_OBJECT_CLASS : BASE_PAGE_OBJECT_CLASS;
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
      return description.getJavadoc();
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
