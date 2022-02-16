/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.translator;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsInRelativeOrder;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.emptyString;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.testng.Assert.expectThrows;
import static utam.compiler.helpers.TypeUtilities.VOID;
import static utam.compiler.translator.TranslationUtilities.JAVADOC_CLOSE_LINE;
import static utam.compiler.translator.TranslationUtilities.JAVADOC_LINE_PATTERN;
import static utam.compiler.translator.TranslationUtilities.JAVADOC_OPEN_LINE;
import static utam.compiler.translator.TranslationUtilities.METHOD_JAVADOC_PARAMETER_LINE;
import static utam.compiler.translator.TranslationUtilities.METHOD_JAVADOC_RETURNS_LINE;
import static utam.compiler.translator.TranslationUtilities.applyJavaFormatter;
import static utam.compiler.translator.TranslationUtilities.getClassJavadoc;
import static utam.compiler.translator.TranslationUtilities.getMethodJavadoc;
import static utam.compiler.translator.TranslationUtilities.getPackageDeclaration;
import static utam.compiler.translator.TranslationUtilities.getStatement;
import static utam.compiler.translator.TranslationUtilities.getWrappedJavadoc;
import static utam.compiler.translator.TranslationUtilities.handleSpecialChars;
import static utam.compiler.translator.TranslationUtilities.isImportableType;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.testng.annotations.Test;
import utam.compiler.grammar.TestUtilities;
import utam.compiler.helpers.ParameterUtils;
import utam.compiler.helpers.PrimitiveType;
import utam.compiler.helpers.TypeUtilities;
import utam.compiler.representation.MethodDeclarationImplTests;
import utam.core.declarative.representation.MethodDeclaration;
import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.PageObjectInterface;
import utam.core.declarative.representation.TypeProvider;
import utam.core.framework.consumer.UtamError;

/**
 * Provides tests for the InterfaceSerializer class
 *
 * @author james.evans
 */
public class TranslationUtilitiesTests {

  private static String getInterfaceCode(String json) {
    PageObjectInterface pageObject =
        TestUtilities.getJsonStringDeserializer(json).getObject().getInterface();
    return new InterfaceSerializer(pageObject).toString();
  }

  /**
   * The format static method should format a list of statements
   */
  @Test
  public void testFormat() {
    assertThat(
        applyJavaFormatter(Arrays.asList("class TestClass {", "}")),
        is(equalTo("class TestClass {}" + System.lineSeparator())));
  }

  /**
   * The format static method should throw the proper exception with invalid syntax
   */
  @Test
  public void testFormatWithInvalidSyntaxThrows() {
    UtamError e =
        expectThrows(
            UtamError.class,
            () ->
                applyJavaFormatter(
                    Stream.of("String x;").collect(Collectors.toList())));
    assertThat(e.getMessage(), containsString("error: class, interface, or enum expected"));
    assertThat(e.getMessage(), containsString("0 > String x;"));
  }

  /**
   * The format static method should return the proper value for a statement
   */
  @Test
  public void testGetStatement() {
    assertThat(getStatement("String x"), is(equalTo("String x;")));
  }

  /**
   * The format static method should return the proper value for an empty statement
   */
  @Test
  public void testGetStatementWithEmptyStatement() {
    assertThat(getStatement(""), is(emptyString()));
  }

  @Test
  public void testGetStatementWithBraces() {
    assertThat(getStatement("statement {"), is(equalTo("statement {")));
    assertThat(getStatement("}"), is(equalTo("}")));
  }

  @Test
  public void testGetPackageDeclaration() {
    assertThat(getPackageDeclaration("test"), is(equalTo("package test;")));
  }

  @Test
  public void testGetImportString() {
    TypeProvider classProvider =
        new TypeUtilities.FromString("testPageObject", "test.testPageObject");
    assertThat(isImportableType(classProvider, "current.package"), is(true));
  }

  @Test
  public void testGetImportStringWithEmptyFullName() {
    TypeProvider classProvider =
        new TypeUtilities.FromString("testPageObject", "");
    assertThat(isImportableType(classProvider, "current.package"), is(false));
  }

  @Test
  public void testGetImportStringWithEmptyPackage() {
    TypeProvider classProvider =
        new TypeUtilities.FromString("TestPageObject", "TestPageObject");
    assertThat(isImportableType(classProvider, "current.package"), is(false));
  }

  @Test
  public void testGetImportStringWithJavaPackage() {
    TypeProvider importedProvider = new TypeUtilities.FromString("String", "java.lang.String");
    assertThat(isImportableType(importedProvider, "current.package"), is(false));
  }

  @Test
  public void testGetImportStringWithSamePackage() {
    TypeProvider classProvider =
        new TypeUtilities.FromString("testPageObject", "test.testPageObject");
    assertThat(isImportableType(classProvider, "test"), is(false));
  }

  @Test
  public void testToString() {
    String json =
        "{"
            + "  \"methods\": ["
            + "    {"
            + "      \"name\": \"clickTestElement\","
            + "      \"compose\": ["
            + "        {"
            + "          \"element\": \"testElement\","
            + "          \"apply\": \"click\""
            + "        }"
            + "      ]"
            + "    }"
            + "  ],"
            + "  \"elements\": ["
            + "    {"
            + "      \"name\": \"testElement\","
            + "      \"type\": [\"clickable\"],"
            + "      \"public\": true,"
            + "      \"selector\": {"
            + "        \"css\": \".fakeSelector\""
            + "      }"
            + "    }"
            + "  ]"
            + "}";
    String code = getInterfaceCode(json);
    assertThat(code, containsString("package utam.test.pageobjects.test;"));
    assertThat(code, containsString("import utam.core.framework.base.PageObject;"));
    assertThat(code, containsString("public interface Test extends PageObject"));
    assertThat(code, containsString("void clickTestElement();"));
    assertThat(code, containsString("TestElementElement getTestElement();"));
  }

  @Test
  public void testDefaultMethodCommentsVoidNoParameters() {
    MethodDeclaration declaration = new MethodDeclarationImplTests.TestHelper("name");
    List<String> comments = getMethodJavadoc(declaration);
    assertThat(comments, containsInRelativeOrder("method name"));
    assertThat(getWrappedJavadoc(comments), containsInRelativeOrder(
        JAVADOC_OPEN_LINE,
        String.format(JAVADOC_LINE_PATTERN, ""),
        String.format(JAVADOC_LINE_PATTERN, "method name"),
        JAVADOC_CLOSE_LINE));
  }

  @Test
  public void testDefaultMethodWithCommentsVoidNoParameters() {
    MethodDeclaration declaration = new MethodDeclarationImplTests.TestHelper("name",
        "method comments");
    List<String> comments = getMethodJavadoc(declaration);
    assertThat(comments, containsInRelativeOrder("method comments"));
    assertThat(getWrappedJavadoc(comments), containsInRelativeOrder(
        JAVADOC_OPEN_LINE,
        String.format(JAVADOC_LINE_PATTERN, ""),
        String.format(JAVADOC_LINE_PATTERN, "method comments"),
        JAVADOC_CLOSE_LINE));
  }

  @Test
  public void testDefaultMethodCommentsReturnsWithParameters() {
    List<MethodParameter> parameters = new ArrayList<>();
    parameters.add(new ParameterUtils.Regular("param1", PrimitiveType.STRING));
    parameters.add(new ParameterUtils.Regular("param2", PrimitiveType.NUMBER));
    MethodDeclaration declaration = new MethodDeclarationImplTests.TestHelper("name", parameters,
        PrimitiveType.BOOLEAN);
    List<String> comments = getMethodJavadoc(declaration);
    assertThat(comments, containsInRelativeOrder(
        "method name",
        String.format(METHOD_JAVADOC_RETURNS_LINE, "Boolean"),
        String.format(METHOD_JAVADOC_PARAMETER_LINE, "param1", "String"),
        String.format(METHOD_JAVADOC_PARAMETER_LINE, "param2", "Integer")
    ));
    assertThat(getWrappedJavadoc(comments), containsInRelativeOrder(
        JAVADOC_OPEN_LINE,
        String.format(JAVADOC_LINE_PATTERN, "method name"),
        String.format(JAVADOC_LINE_PATTERN, String.format(METHOD_JAVADOC_RETURNS_LINE, "Boolean")),
        String.format(JAVADOC_LINE_PATTERN,
            String.format(METHOD_JAVADOC_PARAMETER_LINE, "param1", "String")),
        String.format(JAVADOC_LINE_PATTERN,
            String.format(METHOD_JAVADOC_PARAMETER_LINE, "param2", "Integer")),
        JAVADOC_CLOSE_LINE));
  }

  @Test
  public void testDefaultMethodCommentsVoidWithParameters() {
    List<MethodParameter> parameters = new ArrayList<>();
    parameters.add(new ParameterUtils.Regular("param1", PrimitiveType.STRING));
    parameters.add(new ParameterUtils.Regular("param2", PrimitiveType.NUMBER));
    MethodDeclaration declaration = new MethodDeclarationImplTests.TestHelper("name", parameters,
        VOID);
    List<String> comments = getMethodJavadoc(declaration);
    assertThat(comments, containsInRelativeOrder(
        "method name",
        String.format(METHOD_JAVADOC_PARAMETER_LINE, "param1", "String"),
        String.format(METHOD_JAVADOC_PARAMETER_LINE, "param2", "Integer")
    ));
    assertThat(getWrappedJavadoc(comments), containsInRelativeOrder(
        JAVADOC_OPEN_LINE,
        String.format(JAVADOC_LINE_PATTERN, "method name"),
        String.format(JAVADOC_LINE_PATTERN,
            String.format(METHOD_JAVADOC_PARAMETER_LINE, "param1", "String")),
        String.format(JAVADOC_LINE_PATTERN,
            String.format(METHOD_JAVADOC_PARAMETER_LINE, "param2", "Integer")),
        JAVADOC_CLOSE_LINE));
  }

  @Test
  public void testGetClassJavadoc() {
    List<String> javadoc = getClassJavadoc("");
    assertThat(javadoc.size(), is(equalTo(1)));
    assertThat(javadoc.get(0), containsString("@author UTAM"));
    assertThat(getWrappedJavadoc(javadoc).size(), is(equalTo(4)));
    javadoc = getClassJavadoc("class comments");
    assertThat(javadoc.size(), is(equalTo(2)));
    assertThat(javadoc.get(0), is(equalTo("class comments")));
    assertThat(javadoc.get(1), containsString("@author UTAM"));
    assertThat(getWrappedJavadoc(javadoc).size(), is(equalTo(4)));
  }

  @Test
  public void testHandleSpecialChars() {
    String splCharStr = "**/XCUIElementTypeButton[1]";
    String expSplCharStr = "\\*\\*\\/XCUIElementTypeButton[1]";
    String noSplCharStr = "//XCUIElementTypeButton[1]";
    assertThat(handleSpecialChars(splCharStr), is(equalTo(expSplCharStr)));
    assertThat(handleSpecialChars(noSplCharStr), is(equalTo(noSplCharStr)));
  }

  @Test
  public void testGetWrappedJavadoc() {
    assertThat(getWrappedJavadoc(Collections.emptyList()).isEmpty(), is(true));
  }
}
