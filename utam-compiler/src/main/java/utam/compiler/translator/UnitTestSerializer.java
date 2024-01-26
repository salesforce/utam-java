/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.translator;

import static utam.compiler.helpers.TypeUtilities.SELECTOR;
import static utam.compiler.helpers.TypeUtilities.VOID;
import static utam.compiler.helpers.TypeUtilities.isListType;
import static utam.compiler.helpers.TypeUtilities.wrapAsList;
import static utam.compiler.translator.TranslationUtilities.*;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import utam.compiler.helpers.PrimitiveType;
import utam.compiler.helpers.TranslationContext;
import utam.compiler.types.BasicElementInterface;
import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.PageObjectClass;
import utam.core.declarative.representation.PageObjectMethod;
import utam.core.declarative.representation.TypeProvider;
import utam.core.declarative.translator.UnitTestRunner;
import utam.core.selenium.utilities.WebDriverSimulator;

/**
 * Helper to generate implementation unit test for PO representation
 *
 * @author james.evans
 * @since 228
 */
public final class UnitTestSerializer {

  private static final String ASSERT_STATEMENT_TEMPLATE = "//%stestObject.%s(%s)%s%s;";
  private static final String STRING_LIST_ASSERT_TEMPLATE =
      "//assertThat(testObject.%s(%s), containsInAnyOrder(\"replaceWithValidExpectedValueList\"));";

  private static final String TODO_MARKER = "//TODO: ";
  private static final Class WEB_DRIVER_SIMULATOR_CLASS = WebDriverSimulator.class;

  private final UnitTestRunner testRunner;
  private final List<String> elementRegistrationStatements;
  private final List<List<String>> testMethods;
  private final TypeProvider interfaceType;
  private final TypeProvider implementationType;

  /**
   * Creates a new serializer for Page Object unit tests using TestNG
   *
   * @param pageObject the PageObjectClass describing the implementation
   * @param translationContext context of the translated Page Object, holds context of declared
   *     elements and methods
   */
  public UnitTestSerializer(PageObjectClass pageObject, TranslationContext translationContext) {
    this(pageObject, translationContext, UnitTestRunner.TESTNG);
  }

  /**
   * Creates a new serializer for Page Object unit tests
   *
   * @param pageObject the PageObjectClass describing the implementation
   * @param translationContext context of the translated Page Object, holds context of declared
   *     elements and methods
   * @param testRunner the test runner to generate the unit tests to use
   */
  public UnitTestSerializer(
      PageObjectClass pageObject,
      TranslationContext translationContext,
      UnitTestRunner testRunner) {
    this.testRunner = testRunner;
    interfaceType = pageObject.getImplementedType().getInterfaceType();
    implementationType = pageObject.getClassType();
    elementRegistrationStatements =
        translationContext.getTestableElements().entrySet().stream()
            .map(entry -> entry.getValue().getElementRegistration(entry.getKey()))
            .flatMap(List::stream)
            .collect(Collectors.toList());
    testMethods =
        pageObject.getMethods().stream()
            .filter(PageObjectMethod::isPublic)
            .map(UnitTestSerializer::getTestMethod)
            .collect(Collectors.toList());
  }

  /**
   * Gets a list of the element registration statements for the generated unit test, removing any
   * comment statements from the list
   *
   * @return a list of the element registration statements for the generated unit test, removing any
   *     comment statements from the list
   */
  List<String> getElementRegistrationStatements() {
    return elementRegistrationStatements.stream()
        .filter(statement -> !statement.startsWith(TODO_MARKER))
        .collect(Collectors.toList());
  }

  /**
   * Gets a list of test methods in the generated unit test
   *
   * @return a list of test methods in the generated unit test
   */
  List<List<String>> getTestMethods() {
    return testMethods;
  }

  private static List<String> getTestMethod(PageObjectMethod method) {
    String methodName = method.getDeclaration().getName();
    String fixedMethodName = methodName.substring(0, 1).toUpperCase() + methodName.substring(1);
    TypeProvider returnType = method.getDeclaration().getReturnType();
    String methodArgs = getMethodParameters(method.getDeclaration().getParameters());
    String testStatement =
        String.format(
            ASSERT_STATEMENT_TEMPLATE,
            getTestAssertionStatementPrefix(returnType),
            methodName,
            methodArgs,
            getTestAssertionArguments(returnType),
            getTestAssertionStatementSuffix(returnType));

    List<String> methodContent = new ArrayList<>();
    methodContent.add(NEW_LINE);
    methodContent.add("/**");
    methodContent.add(
        String.format(" * The %s method should %s", methodName, getTestDescription(returnType)));
    methodContent.add(" */");
    methodContent.add("@Test");
    methodContent.add(String.format("public void test%s() {", fixedMethodName));
    methodContent.add("//TODO: implement test (sample below):");

    if (returnType.isSameType(VOID)) {
      methodContent.add("// Simply calling the method is enough. We are simply asserting");
      methodContent.add("// that the method does not throw.");
    }

    methodContent.add(testStatement);

    if (returnType.isSameType(wrapAsList(PrimitiveType.STRING))) {
      methodContent.add(String.format(STRING_LIST_ASSERT_TEMPLATE, methodName, methodArgs));
    }

    methodContent.add("assertThat(true, is(equalTo(false)));");
    methodContent.add("}");
    return methodContent;
  }

  private static String getMethodParameters(List<MethodParameter> parameters) {
    return parameters.stream()
        .map(UnitTestSerializer::getSampleParameterValue)
        .collect(Collectors.joining(", "));
  }

  private static String getSampleParameterValue(MethodParameter parameter) {
    TypeProvider typeName = parameter.getType();
    if (typeName.equals(PrimitiveType.STRING)) {
      return "\"replaceWithValidExpectedValue\"";
    }
    if (typeName.equals(PrimitiveType.NUMBER)) {
      return "-1";
    }
    return "ReplaceWithValidObjectClassName.class";
  }

  private static String getTestDescription(TypeProvider pageObjectMethodReturnType) {
    if (pageObjectMethodReturnType.isSameType(VOID)) {
      return "execute successfully";
    }
    return "return a valid value";
  }

  private static String getTestAssertionStatementPrefix(TypeProvider returnType) {
    if (returnType.isSameType(VOID)) {
      return "";
    }
    return "assertThat(";
  }

  private static String getTestAssertionStatementSuffix(TypeProvider returnType) {
    if (returnType.isSameType(VOID)) {
      return "";
    }
    return ")";
  }

  private static String getTestAssertionArguments(TypeProvider returnType) {
    if (returnType.isSameType(VOID)) {
      return "";
    }
    if (returnType.isSameType(PrimitiveType.STRING)) {
      return ", is(equalTo(\"replaceWithValidExpectedValue\"))";
    }
    if (returnType.isSameType(PrimitiveType.NUMBER)) {
      return ", is(equalTo(-1))";
    }
    if (returnType.isSameType(PrimitiveType.BOOLEAN)) {
      return ", is(equalTo(false))";
    }
    if (BasicElementInterface.isBasicType(returnType)) {
      return ".isPresent(), is(equalTo(true))";
    }
    if (isListType(returnType)) {
      return ", hasSize(-1)";
    }
    return ", is(not(nullValue()))";
  }

  @Override
  public String toString() {
    if (testRunner == UnitTestRunner.NONE) {
      return "";
    }
    List<String> content = new ArrayList<>();
    content.add(getTestClassPackageName());
    content.add(NEW_LINE);
    content.addAll(getTestClassImports());
    content.add(NEW_LINE);
    content.addAll(getTestClassDeclaration());
    content.add(NEW_LINE);
    content.addAll(getTestClassFields());

    testMethods.forEach(content::addAll);

    content.add(NEW_LINE);
    content.addAll(getTestSetupMethodStart());
    content.add(NEW_LINE);
    content.addAll(elementRegistrationStatements);
    content.add(NEW_LINE);
    content.addAll(getTestSetupMethodEnd());

    content.add("}");
    return applyJavaFormatter(content);
  }

  private String getTestClassPackageName() {
    return getPackageDeclaration(implementationType.getPackageName());
  }

  private List<String> getTestClassImports() {
    List<String> imports = new ArrayList<>();
    imports.add("import static org.hamcrest.MatcherAssert.assertThat;");
    imports.add("import static org.hamcrest.Matchers.*;");
    imports.add(NEW_LINE);
    if (testRunner == UnitTestRunner.JUNIT) {
      imports.add("import org.junit.BeforeClass;");
      imports.add("import org.junit.Test;");
    } else {
      imports.add("import org.testng.annotations.BeforeClass;");
      imports.add("import org.testng.annotations.Test;");
    }
    imports.add(NEW_LINE);
    imports.add("import " + WEB_DRIVER_SIMULATOR_CLASS.getName() + ";");
    imports.add("import " + SELECTOR.getFullName() + ";");
    imports.add(NEW_LINE);
    imports.add("import utam.consumer.SalesforceSimulatorObjectFactory;");
    imports.add("import utam.consumer.UtamLoaderTestingContext;");
    imports.add("import " + interfaceType.getFullName() + ";");
    return imports;
  }

  private List<String> getTestClassDeclaration() {
    String pageObjectClassName = implementationType.getSimpleName();
    String userName = System.getProperty("user.name");
    List<String> classDeclaration = new ArrayList<>();
    classDeclaration.add("/**");
    classDeclaration.add(" * Provides tests for the " + pageObjectClassName + " page object");
    classDeclaration.add(" * @author " + userName);
    classDeclaration.add(" *");
    classDeclaration.add(" */");
    classDeclaration.add("public class " + pageObjectClassName + "Tests {");
    return classDeclaration;
  }

  private List<String> getTestClassFields() {
    List<String> fields = new ArrayList<>();
    fields.add("//TODO: add root selector");
    fields.add(
        String.format(
            "private static final %s ROOT_SELECTOR = %s.byCss(\"\");",
            SELECTOR.getSimpleName(), SELECTOR.getSimpleName()));
    fields.add(NEW_LINE);
    fields.add(String.format("private %s simulator;", WEB_DRIVER_SIMULATOR_CLASS.getSimpleName()));
    fields.add("private " + interfaceType.getSimpleName() + " testObject;");
    return fields;
  }

  private List<String> getTestSetupMethodStart() {
    List<String> content = new ArrayList<>();
    content.add("@BeforeClass");
    content.add("public void setupSimulator() {");
    content.add(
        String.format(
            "simulator = new %s(SalesforceSimulatorObjectFactory.class);",
            WEB_DRIVER_SIMULATOR_CLASS.getSimpleName()));
    content.add("simulator.registerElement(\"rootElement\", ROOT_SELECTOR.getStringValue());");
    return content;
  }

  private List<String> getTestSetupMethodEnd() {
    String className = implementationType.getSimpleName();
    List<String> content = new ArrayList<>();
    content.add(
        "UtamLoaderTestingContext loader ="
            + " UtamLoaderTestingContext.getSimulatorLoader(simulator.getDriver());");
    content.add(String.format("testObject = loader.create(%s.class, ROOT_SELECTOR);", className));
    content.add("}");
    return content;
  }
}
