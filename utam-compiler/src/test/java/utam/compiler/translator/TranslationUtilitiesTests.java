package utam.compiler.translator;

import utam.compiler.grammar.TestUtilities;
import utam.compiler.helpers.ParameterUtils;
import utam.compiler.helpers.PrimitiveType;
import utam.compiler.helpers.TypeUtilities;
import utam.core.framework.consumer.UtamError;
import org.testng.annotations.Test;
import utam.compiler.representation.MethodDeclarationImplTests;
import utam.core.declarative.representation.*;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static utam.compiler.helpers.TypeUtilities.VOID;
import static utam.compiler.translator.TranslationUtilities.*;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.testng.Assert.assertThrows;
import static org.testng.Assert.expectThrows;

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
        is(equalTo("class TestClass {}\n")));
  }

  /** The format static method should throw the proper exception with invalid syntax */
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
    TypeProvider classProvider =
        new TypeUtilities.FromString("testPageObject", "test.testPageObject");
    assertThat(getImportString(classProvider, "current.package"),
        is(equalTo("import test.testPageObject;")));
  }

  @Test
  public void testGetImportStringWithEmptyFullName() {
    TypeProvider classProvider =
        new TypeUtilities.FromString("testPageObject", "");
    assertThat(getImportString(classProvider, "current.package"), is(emptyString()));
  }

  @Test
  public void testGetImportStringWithEmptyPackage() {
    TypeProvider classProvider =
        new TypeUtilities.FromString("TestPageObject", "TestPageObject");
    assertThat(getImportString(classProvider, "current.package"), is(emptyString()));
  }

  @Test
  public void testGetImportStringWithJavaPackage() {
    TypeProvider importedProvider = new TypeUtilities.FromString("String", "java.lang.String");
    assertThat(getImportString(importedProvider, "current.package"), is(emptyString()));
  }

  @Test
  public void testGetImportStringWithSamePackage() {
    TypeProvider classProvider =
        new TypeUtilities.FromString("testPageObject", "test.testPageObject");
    assertThat(getImportString(classProvider, "test"), is(emptyString()));
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
            + "      \"type\": \"clickable\","
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
    assertThat(code, containsString("import utam.core.selenium.element.Clickable;"));
    assertThat(code, containsString("public interface Test extends PageObject"));
    assertThat(code, containsString("void clickTestElement();"));
    assertThat(code, containsString("Clickable getTestElement();"));
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
    MethodDeclaration declaration = new MethodDeclarationImplTests.TestHelper("name", "method comments");
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
    MethodDeclaration declaration = new MethodDeclarationImplTests.TestHelper("name", parameters, PrimitiveType.BOOLEAN);
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
            String.format(JAVADOC_LINE_PATTERN, String.format(METHOD_JAVADOC_PARAMETER_LINE, "param1", "String")),
            String.format(JAVADOC_LINE_PATTERN, String.format(METHOD_JAVADOC_PARAMETER_LINE, "param2", "Integer")),
            JAVADOC_CLOSE_LINE));
  }

  @Test
  public void testDefaultMethodCommentsVoidWithParameters() {
    List<MethodParameter> parameters = new ArrayList<>();
    parameters.add(new ParameterUtils.Regular("param1", PrimitiveType.STRING));
    parameters.add(new ParameterUtils.Regular("param2", PrimitiveType.NUMBER));
    MethodDeclaration declaration = new MethodDeclarationImplTests.TestHelper("name", parameters, VOID);
    List<String> comments = getMethodJavadoc(declaration);
    assertThat(comments, containsInRelativeOrder(
            "method name",
            String.format(METHOD_JAVADOC_PARAMETER_LINE, "param1", "String"),
            String.format(METHOD_JAVADOC_PARAMETER_LINE, "param2", "Integer")
    ));
    assertThat(getWrappedJavadoc(comments), containsInRelativeOrder(
            JAVADOC_OPEN_LINE,
            String.format(JAVADOC_LINE_PATTERN, "method name"),
            String.format(JAVADOC_LINE_PATTERN, String.format(METHOD_JAVADOC_PARAMETER_LINE, "param1", "String")),
            String.format(JAVADOC_LINE_PATTERN, String.format(METHOD_JAVADOC_PARAMETER_LINE, "param2", "Integer")),
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
   assertThat(handleSpecialChars(splCharStr),is (equalTo(expSplCharStr)));
   assertThat(handleSpecialChars(noSplCharStr),is (equalTo(noSplCharStr)));
  }
  @Test
  public void testGetLastStatement() {
    PageObjectMethod mock = mock(PageObjectMethod.class);
    MethodDeclaration declarationMock = mock(MethodDeclaration.class);
    when(mock.getDeclaration()).thenReturn(declarationMock);
    when(mock.getCodeLines()).thenReturn(null);
    assertThrows(UtamError.class, () -> getLastStatement(mock));
    when(mock.getCodeLines()).thenReturn(Collections.emptyList());
    assertThrows(UtamError.class, () -> getLastStatement(mock));
    when(mock.getCodeLines()).thenReturn(Collections.singletonList("last statement"));
    when(declarationMock.getReturnType()).thenReturn(VOID);
    assertThat(getLastStatement(mock), is(equalTo("last statement;")));
    when(declarationMock.getReturnType()).thenReturn(PrimitiveType.STRING);
    assertThat(getLastStatement(mock), is(equalTo("return last statement;")));
  }

  @Test
  public void testGetWrappedJavadoc() {
    assertThat(getWrappedJavadoc(Collections.emptyList()).isEmpty(), is(true));
  }
}
