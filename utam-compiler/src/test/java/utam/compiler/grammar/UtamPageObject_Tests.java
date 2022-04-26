/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.contains;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.empty;
import static org.testng.Assert.expectThrows;
import static utam.compiler.grammar.DeserializerUtilities.expectCompilerError;
import static utam.compiler.helpers.TypeUtilities.BASE_PAGE_OBJECT_CLASS;
import static utam.compiler.helpers.TypeUtilities.BASE_ROOT_PAGE_OBJECT_CLASS;
import static utam.compiler.helpers.TypeUtilities.PAGE_OBJECT;
import static utam.compiler.helpers.TypeUtilities.ROOT_PAGE_OBJECT;

import java.util.List;
import java.util.stream.Collectors;
import org.testng.annotations.Test;
import utam.compiler.UtamCompilationError;
import utam.core.declarative.representation.AnnotationProvider;
import utam.core.declarative.representation.PageObjectDeclaration;
import utam.core.framework.consumer.UtamError;

/**
 * Page Object grammar tests
 *
 * @author james.evans
 */
public class UtamPageObject_Tests {

  private final String PARSER_ERROR_PREFIX = "error 900: incorrect format of the page object: \n";

  @Test
  public void testRootPageObjectHasCorrectBaseClasses() {
    PageObjectDeclaration declaration = new DeserializerUtilities()
        .getResultFromFile("pageobjects/root").getPageObject();
    assertThat(
        declaration.getImplementation().getBaseClassType().isSameType(BASE_ROOT_PAGE_OBJECT_CLASS),
        is(true));
    assertThat(declaration.getInterface().getBaseInterfaceType().isSameType(ROOT_PAGE_OBJECT),
        is(true));
    List<String> annotations = declaration.getImplementation()
        .getClassAnnotations().stream()
        .map(AnnotationProvider::getAnnotationText)
        .collect(Collectors.toList());
    assertThat(annotations, contains("@PageMarker.Find(css = \".css\")"));
  }

  @Test
  public void testNonRootPageObjectHasCorrectBaseClasses() {
    PageObjectDeclaration declaration = new DeserializerUtilities()
        .getResultFromFile("pageobjects/non_root").getPageObject();
    assertThat(
        declaration.getImplementation().getBaseClassType().isSameType(BASE_PAGE_OBJECT_CLASS),
        is(true));
    assertThat(declaration.getInterface().getBaseInterfaceType().isSameType(PAGE_OBJECT), is(true));
    List<String> annotations = declaration.getImplementation()
        .getClassAnnotations().stream()
        .map(AnnotationProvider::getAnnotationText)
        .collect(Collectors.toList());
    assertThat(annotations, is(empty()));
  }

  @Test
  public void testGetProfilesWithNullImplementedTypeThrows() {
    String json = "{ \"profile\" :  [{\"profile\" : \"test\" }] }";
    UtamError e = expectThrows(UtamError.class,
        () -> new DeserializerUtilities().getResultFromString(json));
    assertThat(e.getMessage(), containsString(
        "error 805: \"profile\" can only be set for a page object that implements an interface"));
  }

  @Test
  public void testImplementationWithoutProfileThrows() {
    String json = "{ \"implements\" : \"type/to/implement\" }";
    UtamError e = expectThrows(UtamError.class,
        () -> new DeserializerUtilities().getResultFromString(json));
    assertThat(e.getMessage(),
        containsString("error 804: page object with \"implements\" should have \"profile\" property"));
  }

  @Test
  public void testAbstractWithNonNullElementsThrows() {
    String json = "{ \"elements\" : [{ \"name\" : \"test\", \"selector\": {\"css\": \"css\"} }], \"interface\" : true }";
    Exception e = expectThrows(UtamError.class,
        () -> new DeserializerUtilities().getResultFromString(json));
    assertThat(e.getMessage(),
        containsString("error 904: interface declaration can only have properties"));
  }

  @Test
  public void testElementWithRootElementNameThrows() {
    String json = "{ \"elements\" : [ { \"name\" : \"root\", \"selector\" : { \"css\" : \"css\" } } ]}";
    Exception e = expectThrows(UtamError.class,
        () -> new DeserializerUtilities().getResultFromString(json));
    assertThat(e.getMessage(), containsString(
        "error 202: element \"root\": element with same name was already declared"));
  }

  @Test
  public void testRootCreationWithoutRootPropertyErr() {
    String json = "{\"selector\": {\"css\": \"rootSelector\"}}";
    UtamError e = expectThrows(UtamCompilationError.class,
        () -> new DeserializerUtilities().getResultFromString(json));
    assertThat(e.getMessage(),
        containsString("error 901: non root page object can't have selector"));
  }

  /**
   * Tests that an element marked as a root element, but without a selector throws the proper
   * exception
   */
  @Test
  public void testRootNodeWithRootPropertyWithoutSelectorThrows() {
    String json = "{\"root\": true }";
    UtamError e = expectThrows(UtamError.class,
        () -> new DeserializerUtilities().getResultFromString(json));
    assertThat(e.getMessage(),
        containsString("error 902: root page object requires default selector property"));
  }

  @Test
  public void testIncorrectFieldType() {
    String json = "{\"root\": \"invalid\"}";
    UtamError e = expectThrows(UtamError.class,
        () -> new DeserializerUtilities().getResultFromString(json));
    assertThat(
        e.getMessage(),
        containsString(
            PARSER_ERROR_PREFIX + "Cannot deserialize value of type `boolean` from String"));
  }

  @Test
  public void testUnrecognizedField() {
    String json = "{\"public\": true}";
    UtamError e = expectThrows(UtamError.class,
        () -> new DeserializerUtilities().getResultFromString(json));
    assertThat(e.getMessage(),
        containsString(PARSER_ERROR_PREFIX + "Unrecognized field \"public\""));
  }

  @Test
  public void testDuplicateKeyThrows() {
    String json = "{ \"elements\" : [], \"elements\" : [] }";
    UtamError e = expectThrows(UtamError.class,
        () -> new DeserializerUtilities().getResultFromString(json));
    assertThat(e.getMessage(), containsString(PARSER_ERROR_PREFIX + "Duplicate field 'elements'"));
  }

  @Test
  public void testPlatformNotStringThrows() {
    String json = "{ \"platform\": [] }";
    Exception e = expectThrows(UtamError.class,
        () -> new DeserializerUtilities().getResultFromString(json));
    assertThat(e.getMessage(),
        containsString(PARSER_ERROR_PREFIX + "Cannot deserialize instance of `java.lang.String`"));
  }

  @Test
  public void testIncorrectPlatformThrows() {
    String json = "{ \"platform\": \"webview\" }";
    Exception e = expectThrows(UtamError.class,
        () -> new DeserializerUtilities().getResultFromString(json));
    assertThat(e.getMessage(),
        containsString(
            "error 903: \"platform\" property: "
                + "platform should be a string with \"web\" or \"native\" value, instead found \"webview\""));
  }

  @Test
  public void testNotAllowedPropertyThrows() {
    String json = "{\"elements\": [\n"
        + "    {\n"
        + "      \"name\": \"test\",\n"
        + "      \"wrong\": \"test\",\n"
        + "      \"type\": \"container\""
        + "    }\n"
        + "  ]}";
    Exception e = expectCompilerError(json);
    assertThat(e.getMessage(),
        containsString("error 200: root elements: incorrect format of elements: \n"
            + "Unrecognized field \"wrong\""));
  }
}
