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
import static org.hamcrest.Matchers.emptyIterable;
import static org.hamcrest.Matchers.emptyString;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static utam.compiler.translator.TranslationUtilities.JAVADOC_CLOSE_LINE;
import static utam.compiler.translator.TranslationUtilities.JAVADOC_LINE_PATTERN;
import static utam.compiler.translator.TranslationUtilities.JAVADOC_OPEN_LINE;
import static utam.compiler.translator.TranslationUtilities.applyJavaFormatter;
import static utam.compiler.translator.TranslationUtilities.formatJavadoc;
import static utam.compiler.translator.TranslationUtilities.getPackageDeclaration;
import static utam.compiler.translator.TranslationUtilities.getStatement;
import static utam.compiler.translator.TranslationUtilities.getWrappedJavadoc;
import static utam.compiler.translator.TranslationUtilities.isImportableType;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.testng.annotations.Test;
import utam.compiler.grammar.TestUtilities;
import utam.compiler.helpers.TypeUtilities;
import utam.core.declarative.representation.PageObjectInterface;
import utam.core.declarative.representation.TypeProvider;

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

  /** The format static method should format a list of statements */
  @Test
  public void testFormat() {
    assertThat(
        applyJavaFormatter(Arrays.asList("class TestClass {", "}")),
        is(equalTo("class TestClass {}" + System.lineSeparator())));
  }

  /** The format static method should return the proper value for a statement */
  @Test
  public void testGetStatement() {
    assertThat(getStatement("String x"), is(equalTo("String x;")));
  }

  /** The format static method should return the proper value for an empty statement */
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
    TypeProvider classProvider = new TypeUtilities.FromString("test.testPageObject");
    assertThat(isImportableType(classProvider, "current.package"), is(true));
  }

  @Test
  public void testGetImportStringWithEmptyPackage() {
    TypeProvider classProvider = new TypeUtilities.UnimportableType("PageObject");
    assertThat(isImportableType(classProvider, "current.package"), is(false));
  }

  @Test
  public void testGetImportStringWithJavaPackage() {
    TypeProvider importedProvider = new TypeUtilities.FromString("java.lang.String");
    assertThat(isImportableType(importedProvider, "current.package"), is(false));
  }

  @Test
  public void testGetImportStringWithSamePackage() {
    TypeProvider classProvider = new TypeUtilities.FromString("test.testPageObject");
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
  public void testGetWrappedJavadoc() {
    List<String> comments = Stream.of("one", "two").collect(Collectors.toList());
    List<String> wrapped = getWrappedJavadoc(comments);
    assertThat(
        wrapped,
        containsInRelativeOrder(
            JAVADOC_OPEN_LINE,
            String.format(JAVADOC_LINE_PATTERN, "one"),
            String.format(JAVADOC_LINE_PATTERN, "two"),
            JAVADOC_CLOSE_LINE));
  }

  @Test
  public void testGetWrappedJavadocEmptyList() {
    assertThat(getWrappedJavadoc(Collections.emptyList()), is(emptyIterable()));
  }

  @Test
  public void testFormatJavadoc() {
    final String input = "<text> & */";
    assertThat(formatJavadoc(input), is(equalTo("&lt;text&gt; &amp; *&#47")));
  }
}
