/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.representation;

import utam.compiler.helpers.PrimitiveType;
import utam.compiler.helpers.TypeUtilities;
import utam.core.declarative.representation.*;

import java.util.*;
import java.util.stream.Collectors;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;
import static utam.compiler.helpers.TypeUtilities.VOID;

/**
 * Provides methods for validating Page Object structures. Note that this class is intended to be
 * used directly from within tests, as it performs assertions on object contents.
 *
 * @author james.evans
 */
public class PageObjectValidationTestHelper {

  /**
   * Validates a generated Page Object interface
   *
   * @param interfaceObject the PageObjectInterface object to validate
   * @param nameInfo the EntityNameInfo object describing the names of the generated interface
   * @param methods a list of MethodInfo objects describing the methods of the interface
   */
  public static void validateInterface(
      PageObjectInterface interfaceObject, EntityNameInfo nameInfo, List<MethodInfo> methods) {
    TypeProvider type = interfaceObject.getInterfaceType();
    assertThat(
        "interface " + nameInfo.fullName, type.getPackageName(), is(equalTo(nameInfo.packageName)));
    assertThat(
        "interface " + nameInfo.fullName, type.getSimpleName(), is(equalTo(nameInfo.simpleName)));
    assertThat(
        "interface " + nameInfo.fullName, type.getFullName(), is(equalTo(nameInfo.fullName)));

    List<MethodDeclaration> actualDeclarations =
            new ArrayList<>(interfaceObject.getDeclaredApi());
    assertThat(actualDeclarations, hasSize(methods.size()));
    for (int i = 0; i < actualDeclarations.size(); i++) {
      MethodInfo methodInfo = methods.get(i);
      validateDeclaration(actualDeclarations.get(i), methodInfo);
    }
  }

  /**
   * Validates a generated Page Object implementation class
   *
   * @param classObject the PageObjectClass object to validate
   * @param nameInfo the EntityNameInfo object describing the names of the generated implementation
   *     class
   * @param annotations a list of annotation values for the class
   * @param methods a list of MethodInfo objects describing the methods of the generated
   *     implementation class
   * @param fields a list of FieldInfo objects describing the fields of the generated implementation
   *     class
   */
  public static void validateImplementation(
      PageObjectClass classObject,
      EntityNameInfo nameInfo,
      List<String> annotations,
      List<MethodInfo> methods,
      List<FieldInfo> fields) {
    TypeProvider type = classObject.getClassType();
    assertThat(
        "implementation class " + nameInfo.fullName,
        type.getPackageName(),
        is(equalTo(nameInfo.packageName)));
    assertThat(
        "implementation class " + nameInfo.fullName,
        type.getSimpleName(),
        is(equalTo(nameInfo.simpleName)));
    assertThat(
        "implementation class " + nameInfo.fullName,
        type.getFullName(),
        is(equalTo(nameInfo.fullName)));

    validateAnnotationList(classObject.getClassAnnotations(), annotations);

    List<PageObjectMethod> actualMethods =
            new ArrayList<>(classObject.getMethods());
    validateMethods(
        "implementation class " + nameInfo.fullName + " methods", actualMethods, methods);

    List<PageClassField> actualFields = classObject.getFields();
    assertThat(
        "implementation class " + nameInfo.fullName + " fields",
        actualFields,
        hasSize(fields.size()));
    for (int i = 0; i < actualFields.size(); i++) {
      FieldInfo fieldInfo = fields.get(i);
      fieldInfo.validateField(actualFields.get(i));
    }
  }

  /**
   * Validates a list of generated Page Object methods
   *
   * @param methodSource the source of the list of methods to validate
   * @param actualMethods the list of PageObjectMethod objects to validate
   * @param methods the list of MethodInfo objects describing each method properties
   */
  public static void validateMethods(
      String methodSource, List<PageObjectMethod> actualMethods, List<MethodInfo> methods) {
    assertThat(methodSource, actualMethods, hasSize(methods.size()));
    for (int i = 0; i < actualMethods.size(); i++) {
      MethodInfo methodInfo = methods.get(i);
      validateMethod(actualMethods.get(i), methodInfo);
    }
  }

  /**
   * Validates a generated Page Object method
   *
   * @param method the PageObjectMethod object to validate
   * @param info the MethodInfo describing the method properties
   */
  public static void validateMethod(PageObjectMethod method, MethodInfo info) {
    validateDeclaration(method.getDeclaration(), info);
    assertThat(method.isPublic(), is(equalTo(info.getIsPublic())));
    assertThat(
        "method " + info.name + " code lines", method.getCodeLines(), is(equalTo(info.codeLines)));
    if (info.importedTypes.size() > 0 || info.impliedImportedTypes.size() > 0) {
      assertThat(
          "method " + info.name + " class imports",
          info.allImportedTypes(),
          containsInAnyOrder(method.getClassImports().stream()
                  .map(TypeProvider::getFullName).distinct().toArray()));
    }
  }

  /**
   * Validates a list of annotations in a Page Object, for either a class or a member
   *
   * @param actualAnnotations the list of AnnotationProvider objects to validate
   * @param expectedAnnotationTextList a list of strings contained in the annotations
   */
  public static void validateAnnotationList(
      List<AnnotationProvider> actualAnnotations, List<String> expectedAnnotationTextList) {
    // Important implementation note: this method filters out any AnnotationProviders
    // in the actual list that contain the empty string. This is normal and expected,
    // as such empty-string AnnotationProviders only generate white space in the
    // resulting generated Page Object code.
    List<String> actualAnnotationTextList =
        actualAnnotations.stream()
            .filter(a -> !a.getAnnotationText().isEmpty())
            .map(AnnotationProvider::getAnnotationText)
            .collect(Collectors.toList());
    assertThat(actualAnnotationTextList, is(equalTo(expectedAnnotationTextList)));
  }

  public static void validateDeclaration(MethodDeclaration actual, MethodInfo expected) {
    List<MethodParameter> actualParameters = actual.getParameters();
    assertThat(actual.getName(), is(equalTo(expected.name)));
    assertThat(
        "method " + expected.name + " return type",
        actual.getReturnType().getSimpleName(),
        is(equalTo(expected.returnType)));

    assertThat(
        "method " + expected.name + ": parameters count",
        actualParameters,
        hasSize(expected.parameterInfo.size()));
    for (int i = 0; i < actualParameters.size(); i++) {
      MethodParameter actualParameter = actualParameters.get(i);
      MethodParameterInfo expectedParameterInfo = expected.parameterInfo.get(i);
      assertThat(
          "method " + expected.name + " parameter " + i + ": name",
          actualParameter.getValue(),
          is(equalTo(expectedParameterInfo.name)));
      assertThat(
          "method " + expected.name + " parameter " + i + ": data type",
          actualParameter.getType().getSimpleName(),
          is(equalTo(expectedParameterInfo.typeName)));
    }

    // If the user has decided to validate the imports of a declaration, we will do that here.
    if (expected.importedTypes.size() > 0) {
      Set<String> actualImports =
          actual.getImports().stream()
              .map(TypeProvider::getFullName)
              .collect(Collectors.toSet());
      assertThat(actualImports, is(containsInAnyOrder(expected.importedTypes.toArray())));
    }
  }

  /**
   * Describes name information for a PageObject interface or class
   *
   * @author james.evans
   */
  public static final class EntityNameInfo {
    private final String packageName;
    private final String simpleName;
    private final String fullName;

    /**
     * Initializes a new instance of the EntityNameInfo class
     *
     * @param packageName the package name of the interface or class
     * @param simpleName the simple name of the interface or class
     * @param fullName the full name of the interface or class
     */
    public EntityNameInfo(String packageName, String simpleName, String fullName) {
      this.packageName = packageName;
      this.simpleName = simpleName;
      this.fullName = fullName;
    }
  }

  /**
   * Describes a generated PageObject method
   *
   * @author james.evans
   */
  public static final class MethodInfo {
    private final String name;
    private final String returnType;
    private final List<MethodParameterInfo> parameterInfo = new ArrayList<>();
    private final List<String> codeLines = new ArrayList<>();
    private final List<String> importedTypes = new ArrayList<>();
    private final List<String> impliedImportedTypes = new ArrayList<>();
    private boolean isPublic = true;

    /**
     * Initializes a new instance of the MethodInfo class
     *
     * @param name the name of the method
     * @param returnTypeSimpleName the simple type name of the return type of the method
     */
    public MethodInfo(String name, String returnTypeSimpleName) {
      this.name = name;
      this.returnType = returnTypeSimpleName;
    }

    public MethodInfo(String name, TypeProvider returnType) {
      this.name = name;
      this.returnType = returnType.getSimpleName();
    }

    public MethodInfo(String name) {
      this(name, VOID.getSimpleName());
    }

    /**
     * Adds a parameter descriptor for the method
     *
     * @param parameter MethodParameterInfo describing the method parameter
     */
    public void addParameter(MethodParameterInfo parameter) {
      addParameters(parameter);
    }

    /**
     * Adds parameter descriptors for the method parameters
     *
     * @param parameters MethodParameterInfo describing the method parameters
     */
    void addParameters(MethodParameterInfo... parameters) {
      parameterInfo.addAll(Arrays.asList(parameters));
    }

    /**
     * Adds string name of type to add to the class import for the given method as implied by the
     * method implementation
     *
     * @param importedTypes the full names of the types to be imported into the class by the method
     */
    public void addImpliedImportedTypes(String... importedTypes) {
      this.impliedImportedTypes.addAll(Arrays.asList(importedTypes));
    }

    /**
     * Adds string name of type to import for the given method
     *
     * @param importedTypes the full names of the types to be imported into the class by the method
     */
    public void addImportedTypes(String...importedTypes) {
      this.importedTypes.addAll(Arrays.asList(importedTypes));
    }

    /**
     * Adds a line of code of the method
     *
     * @param codeLine the line of code in the method
     */
    public void addCodeLine(String codeLine) {
      addCodeLines(codeLine);
    }

    /**
     * Adds lines of code of the method
     *
     * @param codeLines the lines of code in the method
     */
    public void addCodeLines(String... codeLines) {
      this.codeLines.addAll(Arrays.asList(codeLines));
    }

    /**
     * Gets the list of all types imported by the method, both explicitly declared in parameters and
     * return type, and implicitly required by method implementation
     *
     * @return the full list of imported types required by the method
     */
    Set<String> allImportedTypes() {
      Set<String> allImports = new HashSet<>();
      allImports.addAll(importedTypes);
      allImports.addAll(impliedImportedTypes);
      return allImports;
    }

    public boolean getIsPublic() {
      return isPublic;
    }

    public void setIsPublic(boolean isPublic) {
      this.isPublic = isPublic;
    }

    public void test(PageObjectMethod method) {
      validateDeclaration(method.getDeclaration(), this);
      assertThat(method.getCodeLines(), is(equalTo(codeLines)));
      // test against all imported types to make sure declaration is covered as well
      assertThat(allImportedTypes(), containsInAnyOrder(method.getClassImports().stream()
                      .map(TypeProvider::getFullName).distinct().toArray()));
    }
  }

  /**
   * Describes a generated Page Object field
   *
   * @author james.evans
   */
  public static final class FieldInfo {
    private final String name;
    private final String typeName;
    private final List<String> annotationValues = new ArrayList<>();

    /**
     * Initializes a new instance of the FieldInfo class
     *
     * @param name the name of the field
     * @param typeName the simple type name of the field
     */
    public FieldInfo(String name, String typeName) {
      this.name = name;
      this.typeName = typeName;
    }

    public FieldInfo(String name) {
      this.name = name;
      this.typeName = TypeUtilities.ELEMENT_FIELD.getSimpleName();
    }

    /**
     * Adds the text of annotations for the field
     *
     * @param annotations the text of the annotations on the field
     */
    public void addAnnotations(String... annotations) {
      this.annotationValues.addAll(Arrays.asList(annotations));
    }

    /**
     * Validates a Page Object field
     *
     * @param fieldObject the PageElementField object to validate
     */
    public void validateField(PageClassField fieldObject) {
      assertThat(
              "field " + name + ": name", fieldObject.getName(), is(equalTo(name)));
      assertThat(
              "field " + name + ": type name",
              fieldObject.getType().getSimpleName(),
              is(equalTo(typeName)));
      validateAnnotationList(fieldObject.getAnnotations(), annotationValues);
    }
  }

  /**
   * Describes the parameters of a Page Object method
   *
   * @author james.evans
   */
  public static final class MethodParameterInfo {
    private final String name;
    private final String typeName;

    /**
     * Initializes a new instance of the MethodParameterInfo class
     *
     * @param name the name of the parameter
     * @param typeName the simple type name of the parameter
     */
    public MethodParameterInfo(String name, String typeName) {
      this.name = name;
      this.typeName = typeName;
    }

    public MethodParameterInfo(String name, TypeProvider type) {
      this.name = name;
      this.typeName = type.getSimpleName();
    }

    public MethodParameterInfo(String name) {
      this(name, PrimitiveType.STRING);
    }
  }
}
