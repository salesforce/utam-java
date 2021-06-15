/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.module.SimpleModule;
import com.google.common.io.CharStreams;
import utam.compiler.helpers.TranslationContext;
import utam.compiler.helpers.TypeUtilities;
import utam.compiler.representation.ElementMethod;
import utam.core.framework.consumer.UtamError;
import utam.core.framework.context.Profile;
import utam.compiler.translator.ClassSerializer;
import utam.compiler.translator.InterfaceSerializer;
import utam.compiler.translator.UnitTestSerializer;
import utam.core.declarative.representation.*;
import utam.core.declarative.translator.TranslatorConfig;
import utam.core.declarative.translator.TranslatorSourceConfig;
import utam.core.declarative.translator.UnitTestRunner;

import java.io.IOException;
import java.util.*;
import java.util.stream.Collectors;

import static com.fasterxml.jackson.core.JsonParser.Feature.ALLOW_COMMENTS;
import static com.fasterxml.jackson.databind.DeserializationFeature.ACCEPT_SINGLE_VALUE_AS_ARRAY;
import static utam.compiler.helpers.TypeUtilities.BASE_PAGE_OBJECT;

/**
 * entry point for JSON deserialization
 *
 * @author elizaveta.ivanova
 * @since 228
 */
public final class JsonDeserializer {

  private final String pageObjectURI;
  private final UtamPageObject utamPageObject;
  private final TranslationContext context;

  JsonDeserializer(
      String pageObjectURI, String jsonString, TranslatorConfig translatorConfiguration) {
    this.pageObjectURI = pageObjectURI;
    try {
      this.utamPageObject = deserialize(UtamPageObject.class, jsonString);
      this.context = new TranslationContext(pageObjectURI, translatorConfiguration);
      this.utamPageObject.compile(this.context);
    } catch (IOException e) {
      throw new UtamError(getErrorPrefix(), e);
    }
  }

  public JsonDeserializer(
      TranslatorConfig translatorConfig,
      TranslatorSourceConfig translatorSourceConfig,
      String pageObjectURI) {
    this(
        pageObjectURI,
        getStringFromReader(translatorSourceConfig, pageObjectURI),
        translatorConfig);
  }

  private static String getStringFromReader(
      TranslatorSourceConfig translatorSourceConfig, String pageObjectURI) {
    try {
      return CharStreams.toString(translatorSourceConfig.getDeclarationReader(pageObjectURI));
    } catch (IOException e) {
      throw new UtamError(getErrorPrefix(pageObjectURI), e);
    }
  }

  static String getErrorPrefix(String pageObjectURI) {
    return String.format("Error in the page object '%s'", pageObjectURI);
  }

  private static ObjectMapper getDeserializerMapper() {
    ObjectMapper mapper = new ObjectMapper();
    mapper.enable(ALLOW_COMMENTS);
    mapper.enable(ACCEPT_SINGLE_VALUE_AS_ARRAY);
    SimpleModule module = new SimpleModule();
    module.addDeserializer(UtamProfile.class, new UtamProfile.Deserializer());
    module.addDeserializer(UtamArgument.class, new UtamArgumentDeserializer());
    mapper.registerModule(module);
    return mapper;
  }

  static <T> T deserialize(Class<T> type, String jsonString) throws IOException {
    return getDeserializerMapper().readValue(jsonString, type);
  }

  // to access from test utilities
  final TranslationContext getContext() {
    return context;
  }

  private String getErrorPrefix() {
    return getErrorPrefix(pageObjectURI);
  }

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
      return pageObjectClass.getProfiles().length > 0;
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
      this.implementedType = context.getInterfaceType(utamPageObject.implementsType);
    }

    @Override
    public Collection<MethodDeclaration> getDeclaredApi() {
      return context.getMethods().stream()
          .filter(PageObjectMethod::isPublic)
          .map(PageObjectMethod::getDeclaration)
          .collect(Collectors.toList());
    }

    @Override
    public Collection<TypeProvider> getNestedInterfaces() {
      List<TypeProvider> typeList = context.getMethods().stream()
          .filter(method -> method.isElementMethod())
          .map(method -> method.getDeclaration().getReturnType())
          .map(returnType -> {
            if (returnType instanceof TypeUtilities.ListOf) {
              return ((TypeUtilities.ListOf)returnType).getElementType();
            }
            return returnType;
          })
          .collect(Collectors.toList());
      return typeList;
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
    public String getApiCode() {
      return new InterfaceSerializer(this).toString();
    }

    @Override
    public String getComments() {
      return utamPageObject.comments;
    }
  }

  static class Implementation implements PageObjectClass {

    private final TranslationContext context;
    private final UtamPageObject utamPageObject;
    private final PageObjectInterface pageObjectInterface;

    Implementation(
        TranslationContext context,
        UtamPageObject utamPageObject,
        PageObjectInterface pageObjectInterface) {
      this.context = context;
      this.utamPageObject = utamPageObject;
      this.pageObjectInterface = pageObjectInterface;
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
      return BASE_PAGE_OBJECT;
    }

    @Override
    public String getImplCode() {
      return new ClassSerializer(this, this.context).toString();
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
    public Profile[] getProfiles() {
      return utamPageObject.getProfiles(context);
    }

    @Override
    public String getComments() {
      return utamPageObject.comments;
    }
  }
}
