/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.helpers;

import static utam.compiler.helpers.ElementContext.DOCUMENT_ELEMENT_NAME;
import static utam.compiler.helpers.ElementContext.Document.DOCUMENT_ELEMENT;
import static utam.compiler.helpers.ElementContext.ROOT_ELEMENT_NAME;
import static utam.compiler.helpers.ElementContext.SELF_ELEMENT_NAME;
import static utam.compiler.helpers.ElementContext.Self.SELF_ELEMENT;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import utam.compiler.guardrails.GlobalValidation;
import utam.compiler.guardrails.PageObjectValidation;
import utam.compiler.helpers.ElementContext.Document;
import utam.compiler.helpers.ElementContext.Self;
import utam.compiler.representation.BasicElementGetterMethod;
import utam.core.declarative.representation.PageClassField;
import utam.core.declarative.representation.PageObjectMethod;
import utam.core.declarative.representation.TypeProvider;
import utam.core.declarative.representation.UnionType;
import utam.core.declarative.translator.ProfileConfiguration;
import utam.core.declarative.translator.TranslationTypesConfig;
import utam.core.declarative.translator.TranslatorConfig;
import utam.core.framework.consumer.UtamError;
import utam.core.framework.context.Profile;

/**
 * instance is created for every Page Object that is being translated <br> contains reference to the
 * Page Object context and translator context
 *
 * @author elizaveta.ivanova
 * @since 228
 */
public final class TranslationContext {

  public static final String ERR_CONTEXT_ELEMENT_NOT_FOUND =
      "referenced element '%s' not found in context";
  static final String ERR_CONTEXT_DUPLICATE_METHOD = "duplicate method '%s'";
  static final String ERR_CONTEXT_DUPLICATE_FIELD = "duplicate field '%s'";
  static final String ERR_CONTEXT_DUPLICATE_ELEMENT_NAME =
      "element with name '%s' already exists in same JSON";
  static final String ERR_PROFILE_NOT_CONFIGURED = "profile '%s' is not configured";
  private final List<PageClassField> pageObjectFields = new ArrayList<>();
  private final List<PageObjectMethod> pageObjectMethods = new ArrayList<>();
  private final List<BasicElementGetterMethod> elementGetters = new ArrayList<>();
  private final Set<String> methodNames = new HashSet<>();
  private final Map<String, ElementContext> elementContextMap =
      Collections.synchronizedMap(new HashMap<>());
  private final String pageObjectURI;
  private final TranslationTypesConfig translationTypesConfig;
  private final TranslatorConfig translatorConfiguration;
  private final Set<String> usedPrivateMethods = new HashSet<>();
  private final Map<String, ElementUnitTestHelper> testableElements = new HashMap<>();
  private boolean isAbstractPageObject = false;
  private boolean isImplementationPageObject = false;
  private final TypeProvider pageObjectClassType;
  private TypeProvider pageObjectInterfaceType;


  public TranslationContext(String pageObjectURI, TranslatorConfig translatorConfiguration) {
    this.pageObjectURI = pageObjectURI;
    this.translationTypesConfig = translatorConfiguration.getTranslationTypesConfig();
    this.translatorConfiguration = translatorConfiguration;
    // register elements to prevent names collisions
    setElement(Document.DOCUMENT_ELEMENT);
    setElement(Self.SELF_ELEMENT);
    // has impl prefixes
    this.pageObjectClassType = translationTypesConfig.getClassType(pageObjectURI);
    this.pageObjectInterfaceType = translationTypesConfig.getInterfaceType(pageObjectURI);
  }

  public void guardrailsValidation() {
    PageObjectValidation pageObjectValidation = new PageObjectValidation(
        translatorConfiguration.getValidationMode(), pageObjectURI, elementContextMap.values());
    pageObjectValidation.validate();
  }

  public void setGlobalGuardrailsContext(GlobalValidation validation) {
    validation.setPageObjectElements(pageObjectURI, elementContextMap.values());
  }

  public void setAbstract() {
    this.isAbstractPageObject = true;
  }

  public boolean isAbstractPageObject() {
    return isAbstractPageObject;
  }

  public boolean isImplementationPageObject() {
    return isImplementationPageObject;
  }

  public void setImplementedType(String implementsProperty) {
    this.isImplementationPageObject = true;
    this.pageObjectInterfaceType = translationTypesConfig.getInterfaceType(implementsProperty);
  }

  public TypeProvider getSelfType() {
    return pageObjectInterfaceType;
  }

  public TypeProvider getClassType() {
    return pageObjectClassType;
  }

  public TypeProvider getType(String type) {
    return translationTypesConfig.getInterfaceType(type);
  }

  public TypeProvider getUtilityType(String type) {
    return translationTypesConfig.getUtilityType(type);
  }

  public void setElement(ElementContext element) {
    if (elementContextMap.containsKey(element.getName())) {
      throw new UtamError(String.format(ERR_CONTEXT_DUPLICATE_ELEMENT_NAME, element.getName()));
    }
    elementContextMap.put(element.getName(), element);
  }

  public void setClassField(PageClassField field) {
    if (pageObjectFields.stream().anyMatch(f -> f.getName().equals(field.getName()))) {
      throw new UtamError(String.format(ERR_CONTEXT_DUPLICATE_FIELD, field.getName()));
    }
    pageObjectFields.add(field);
  }

  public void setMethod(PageObjectMethod method) {
    // first check if same method already exists
    if (methodNames.contains(method.getDeclaration().getName())) {
      throw new UtamError(
          String.format(ERR_CONTEXT_DUPLICATE_METHOD, method.getDeclaration().getName()));
    }
    // no duplicates - add method
    methodNames.add(method.getDeclaration().getName());
    if(method instanceof BasicElementGetterMethod) {
      elementGetters.add((BasicElementGetterMethod) method);
    } else {
      pageObjectMethods.add(method);
    }
  }

  public ElementContext getRootElement() {
    return getElement(ROOT_ELEMENT_NAME);
  }

  public ElementContext getElement(String name) {
    if (name == null || SELF_ELEMENT_NAME.equals(name)) {
      return SELF_ELEMENT;
    }
    if (DOCUMENT_ELEMENT_NAME.equals(name)) {
      return DOCUMENT_ELEMENT;
    }
    if (!elementContextMap.containsKey(name)) {
      throw new UtamError(String.format(ERR_CONTEXT_ELEMENT_NOT_FOUND, name));
    }
    return elementContextMap.get(name);
  }

  public List<PageObjectMethod> getMethods() {
    List<PageObjectMethod> allMethods = new ArrayList<>();
    // only used ones!
    allMethods.addAll(getBasicElementsGetters());
    allMethods.addAll(pageObjectMethods);
    return allMethods;
  }

  public List<PageClassField> getFields() {
    return pageObjectFields;
  }

  public Profile getProfile(String jsonKey, String value) {
    ProfileConfiguration profileConfiguration = translatorConfiguration.getConfiguredProfiles()
        .stream()
        .filter(configuration -> configuration.getPropertyKey().equals(jsonKey))
        .findAny()
        .orElseThrow(() -> new UtamError(String.format(ERR_PROFILE_NOT_CONFIGURED, jsonKey)));
    return profileConfiguration.getFromString(value);
  }

  /**
   * if private method was never used, we should not generate code for it to prevent tests coverage
   * violations. It might happen with getter for private element if it's only used as scope or in
   * compose as "apply"
   *
   * @param name method name
   */
  public void setMethodUsage(String name) {
    usedPrivateMethods.add(name);
  }

  public PageObjectMethod getMethod(String name) {
    PageObjectMethod result = pageObjectMethods.stream()
        .filter(method -> method.getDeclaration().getName().equals(name))
        .findFirst()
        .orElse(null);
    if(result == null) {
      result = elementGetters.stream()
          .filter(method -> method.getDeclaration().getName().equals(name))
          .findFirst()
          .orElse(null);
    }
    if(result == null) {
      throw new AssertionError(String.format("method '%s' not found in JSON", name));
    }
    return result;
  }

  /**
   * used from unit test deserializer to get full list of elements <br> some elements are not
   * declared as fields
   *
   * @return collection of element contexts
   */
  public Map<String, ElementUnitTestHelper> getTestableElements() {
    return testableElements;
  }

  /**
   * remember element for unit test deserializer
   *
   * @param elementName name of the element
   * @param helper      information used to generate unit test
   */
  public void setTestableElement(String elementName, ElementUnitTestHelper helper) {
    this.testableElements.put(elementName, helper);
  }

  private List<BasicElementGetterMethod> getBasicElementsGetters() {
    // only used element getter should be returned
    return elementGetters.stream()
        .filter(getter -> getter.isPublic() || usedPrivateMethods.contains(getter.getDeclaration().getName()))
        .collect(Collectors.toList());
  }

  /**
   * get list of union types to be declared in the implementation class
   *
   * @return list of union types
   */
  public List<UnionType> getClassUnionTypes() {
    List<BasicElementGetterMethod> getters = getBasicElementsGetters();
    List<UnionType> unionTypes = new ArrayList<>();
    if(isImplementationPageObject()) {
      // if impl only PO has private basic elements - declare interface as well
      getters.forEach(getter -> {
        if(!getter.isPublic() && getter.getInterfaceUnionType() != null) {
          unionTypes.add(getter.getInterfaceUnionType());
        }
      });
    }
    getters.forEach(getter -> {
      if(getter.getClassUnionType() != null) {
        unionTypes.add(getter.getClassUnionType());
      }
    });
    return unionTypes;
  }

  /**
   * get list of union types to be declared in the interface
   *
   * @return list of union types
   */
  public List<UnionType> getInterfaceUnionTypes() {
    List<BasicElementGetterMethod> getters = getBasicElementsGetters();
    List<UnionType> unionTypes = new ArrayList<>();
    getters.forEach(getter -> {
      if(getter.getInterfaceUnionType() != null) {
        unionTypes.add(getter.getInterfaceUnionType());
      }
    });
    return unionTypes;
  }
}
