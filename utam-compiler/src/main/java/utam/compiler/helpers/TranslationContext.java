/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.helpers;

import static utam.compiler.UtamCompilationError.processParserError;
import static utam.compiler.diagnostics.ValidationUtilities.VALIDATION;
import static utam.compiler.helpers.ElementContext.DOCUMENT_ELEMENT_NAME;
import static utam.compiler.helpers.ElementContext.Document.DOCUMENT_ELEMENT;
import static utam.compiler.helpers.ElementContext.NAVIGATION_OBJECT_NAME;
import static utam.compiler.helpers.ElementContext.Navigation.NAVIGATION_OBJECT;
import static utam.compiler.helpers.ElementContext.ROOT_ELEMENT_NAME;
import static utam.compiler.helpers.ElementContext.SELF_ELEMENT_NAME;
import static utam.compiler.helpers.ElementContext.Self.SELF_ELEMENT;

import com.fasterxml.jackson.core.JsonParser;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import utam.compiler.UtamCompilationError;
import utam.compiler.helpers.ElementContext.Document;
import utam.compiler.helpers.ElementContext.Self;
import utam.compiler.helpers.TypeUtilities.PageObjectWithNamesCollisionType;
import utam.compiler.lint.PageObjectLintingImpl;
import utam.compiler.representation.BasicElementGetterMethod;
import utam.compiler.translator.CompilerErrors.StringError;
import utam.core.declarative.errors.CompilerErrorsContext;
import utam.core.declarative.lint.PageObjectLinting;
import utam.core.declarative.representation.PageClassField;
import utam.core.declarative.representation.PageObjectMethod;
import utam.core.declarative.representation.TypeProvider;
import utam.core.declarative.representation.UnionType;
import utam.core.declarative.translator.ProfileConfiguration;
import utam.core.declarative.translator.TranslationTypesConfig;
import utam.core.declarative.translator.TranslatorConfig;

/**
 * Instance of this type is created for every Page Object that is being translated, contains
 * reference to the Page Object context and translator context
 *
 * @author elizaveta.ivanova
 * @since 228
 */
public class TranslationContext {
  private final Map<String, PageClassField> pageObjectFields = new HashMap<>();
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
  private final TypeProvider pageObjectClassType;

  /** track possible names collisions for custom elements types */
  private final Map<String, TypeProvider> customTypesMap = new HashMap<>();

  private final PageObjectLinting contextForLinting;
  private boolean isImplementationPageObject = false;
  private TypeProvider pageObjectInterfaceType;

  private CompilerErrorsContext.CompilerError error = null;

  /**
   * Initializes a new instance of the TranslationContext class
   *
   * @param pageObjectURI the Page Object URI
   * @param filePath path to the JSON source file, used by SARIF to link with source file
   * @param translatorConfiguration the translator configuration
   */
  public TranslationContext(
      String pageObjectURI, String filePath, TranslatorConfig translatorConfiguration) {
    this.pageObjectURI = pageObjectURI;
    this.translationTypesConfig = translatorConfiguration.getTranslationTypesConfig();
    this.translatorConfiguration = translatorConfiguration;
    // register elements to prevent names collisions
    setElement(Document.DOCUMENT_ELEMENT, null);
    setElement(Self.SELF_ELEMENT, null);
    setElement(NAVIGATION_OBJECT, null);
    // has impl prefixes
    this.pageObjectClassType = translationTypesConfig.getClassType(pageObjectURI);
    this.pageObjectInterfaceType = translationTypesConfig.getInterfaceType(pageObjectURI);
    this.contextForLinting =
        new PageObjectLintingImpl(pageObjectURI, filePath, this.pageObjectInterfaceType);
  }

  /**
   * Gets a value indicating that the Page Object is an implementation-only Page Object
   *
   * @return true if the Page Object is an implementation-only Page Object; otherwise, false
   */
  public boolean isImplementationPageObject() {
    return isImplementationPageObject;
  }

  /**
   * Sets the type that this Page Object implements
   *
   * @param implementsProperty the type that the Page Object implements
   */
  public void setImplementedType(String implementsProperty) {
    this.isImplementationPageObject = true;
    this.pageObjectInterfaceType = translationTypesConfig.getInterfaceType(implementsProperty);
  }

  /**
   * Gets the interface type of this Page Object
   *
   * @return the interface type of this Page Object
   */
  public TypeProvider getSelfType() {
    return pageObjectInterfaceType;
  }

  /**
   * Gets the implementation type of this Page Object
   *
   * @return the implementation type of this Page Object
   */
  public TypeProvider getClassType() {
    return pageObjectClassType;
  }

  /**
   * Gets the specified type provider. If type with same name already exists, wrap into new type
   *
   * @param typeStr string with type name
   * @return the type provider of the specified type
   */
  public TypeProvider getType(String typeStr) {
    TypeProvider type = translationTypesConfig.getInterfaceType(typeStr);
    if (customTypesMap.containsKey(type.getSimpleName())) {
      TypeProvider alreadyDeclared = customTypesMap.get(type.getSimpleName());
      // if simple name is same, but full is not - resolve collision by using full name instead
      // short name
      // this allows to avoid importing class with simple names twice
      if (!alreadyDeclared.getFullName().equals(type.getFullName())) {
        type = new PageObjectWithNamesCollisionType(type);
      }
    } else {
      customTypesMap.put(type.getSimpleName(), type);
    }
    return type;
  }

  /**
   * Gets the imperative extensions type of this Page Object
   *
   * @param type the type of the imperative extensions class
   * @return the imperative extensions type of this Page Object
   */
  public TypeProvider getUtilityType(String type) {
    return translationTypesConfig.getUtilityType(type);
  }

  /**
   * Sets the element context for this Page Object
   *
   * @param element the element context to set
   * @param field field to set, can be null
   */
  public void setElement(ElementContext element, PageClassField field) {
    if (elementContextMap.containsKey(element.getName())) {
      throw new UtamCompilationError(VALIDATION.getErrorMessage(202, element.getName()));
    }
    elementContextMap.put(element.getName(), element);
    if (field != null) {
      pageObjectFields.put(element.getName(), field);
    }
  }

  /**
   * Sets a method on the Page Object
   *
   * @param method the method to set
   */
  public void setMethod(PageObjectMethod method) {
    // first check if same method already exists
    if (methodNames.contains(method.getDeclaration().getName())) {
      throw new UtamCompilationError(
          VALIDATION.getErrorMessage(504, method.getDeclaration().getName()));
    }
    // no duplicates - add method
    methodNames.add(method.getDeclaration().getName());
    if (method instanceof BasicElementGetterMethod) {
      elementGetters.add((BasicElementGetterMethod) method);
    } else {
      pageObjectMethods.add(method);
    }
  }

  /**
   * Gets the root element of this Page Object
   *
   * @return the root element of this Page Object
   */
  public ElementContext getRootElement() {
    return getElement(ROOT_ELEMENT_NAME);
  }

  /**
   * Gets an element by name in this Page Object
   *
   * @param name the name of the element to get
   * @return the element in the Page Object or null if none found
   */
  public ElementContext getElement(String name) {
    if (name == null || SELF_ELEMENT_NAME.equals(name)) {
      return SELF_ELEMENT;
    }
    if (DOCUMENT_ELEMENT_NAME.equals(name)) {
      return DOCUMENT_ELEMENT;
    }
    if (NAVIGATION_OBJECT_NAME.equals(name)) {
      return NAVIGATION_OBJECT;
    }
    if (!elementContextMap.containsKey(name)) {
      return null;
    }
    return elementContextMap.get(name);
  }

  /**
   * Gets the list of methods defined in the Page Object
   *
   * @return the list of Page Object methods
   */
  public List<PageObjectMethod> getMethods() {
    List<PageObjectMethod> allMethods = new ArrayList<>();
    // only used ones!
    allMethods.addAll(getBasicElementsGetters());
    allMethods.addAll(pageObjectMethods);
    return allMethods;
  }

  /**
   * Gets the list of fields defined in the Page Object
   *
   * @return the list of fields defined in the Page Object
   */
  public List<PageClassField> getFields() {
    Set<String> uniqueFieldNames = new HashSet<>();
    List<PageClassField> fields = new ArrayList<>();
    // return only unique fields
    // for fantom scope element same field can be reused
    for (PageClassField field : pageObjectFields.values()) {
      if (!uniqueFieldNames.contains(field.getName())) {
        fields.add(field);
      }
      uniqueFieldNames.add(field.getName());
    }
    return fields;
  }

  /**
   * Get locator field name by element name
   *
   * @param elementName name of the element
   * @return string with field name
   */
  public String getFieldName(String elementName) {
    return pageObjectFields.get(elementName).getName();
  }

  /**
   * Get a profile configuration
   *
   * @param name the key specified in the JSON for the profile
   * @return the profile configuration or null
   */
  public ProfileConfiguration getConfiguredProfile(String name) {
    return translatorConfiguration.getConfiguredProfiles().stream()
        .filter(configuration -> configuration.getPropertyKey().equals(name))
        .findAny()
        .orElse(null);
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

  /**
   * Gets a method from the Page Object
   *
   * @param name the name of the method to get
   * @return the named method on the Page Object
   */
  public PageObjectMethod getMethod(String name) {
    PageObjectMethod result =
        pageObjectMethods.stream()
            .filter(method -> method.getDeclaration().getName().equals(name))
            .findFirst()
            .orElse(null);
    if (result == null) {
      result =
          elementGetters.stream()
              .filter(method -> method.getDeclaration().getName().equals(name))
              .findFirst()
              .orElse(null);
    }
    if (result == null) {
      throw new AssertionError(String.format("method '%s' not found in JSON", name));
    }
    return result;
  }

  /**
   * used from unit test deserializer to get full list of elements <br>
   * some elements are not declared as fields
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
   * @param helper information used to generate unit test
   */
  public void setTestableElement(String elementName, ElementUnitTestHelper helper) {
    this.testableElements.put(elementName, helper);
  }

  private List<BasicElementGetterMethod> getBasicElementsGetters() {
    // only used element getter should be returned
    return elementGetters.stream()
        .filter(
            getter ->
                getter.isPublic() || usedPrivateMethods.contains(getter.getDeclaration().getName()))
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
    if (isImplementationPageObject()) {
      // if impl only PO has private basic elements - declare interface as well
      getters.forEach(
          getter -> {
            if (!getter.isPublic() && getter.getInterfaceUnionType() != null) {
              unionTypes.add(getter.getInterfaceUnionType());
            }
          });
    }
    getters.forEach(
        getter -> {
          if (getter.getClassUnionType() != null) {
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
    getters.forEach(
        getter -> {
          if (getter.getInterfaceUnionType() != null) {
            unionTypes.add(getter.getInterfaceUnionType());
          }
        });
    return unionTypes;
  }

  /**
   * get page objects version from config
   *
   * @return string
   */
  public String getConfiguredVersion() {
    return this.translatorConfiguration.getPageObjectsVersion();
  }

  /**
   * get JSON path from config
   *
   * @return string
   */
  public String getJsonPath() {
    String fullPath = translatorConfiguration.getConfiguredSource().getSourcePath(pageObjectURI);
    int index = fullPath.indexOf("resources");
    if (index > 0) { // path can be empty for mocks
      return fullPath.substring(index);
    }
    return fullPath;
  }

  /**
   * get copyright caption from config
   *
   * @return list of strings
   */
  public List<String> getCopyright() {
    return translatorConfiguration.getCopyright();
  }

  /**
   * Get subset of the page object data that is needed for linting
   *
   * @return object
   */
  public PageObjectLinting getLintingObject() {
    return contextForLinting;
  }

  /**
   * Process compiler error - depending on config either throw or report
   *
   * @param parser instance of the parser
   * @param compilationError thrown error
   */
  public void processError(JsonParser parser, Exception compilationError) {
    UtamCompilationError.ErrorSupplier errorSupplier =
        processParserError(parser, compilationError, pageObjectURI);
    if (this.translatorConfiguration.getErrorsConfig().isInterrupt()) {
      throw new UtamCompilationError(errorSupplier.getMessage(), errorSupplier.getCause());
    } else {
      this.error = new StringError(errorSupplier.getMessage());
    }
  }

  /**
   * If generation was interrupted, return the error
   *
   * @return error object or null
   */
  public CompilerErrorsContext.CompilerError getCompilerError() {
    return this.error;
  }
}
