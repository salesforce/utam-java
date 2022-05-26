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
import static org.hamcrest.Matchers.containsString;
import static org.testng.Assert.expectThrows;
import static utam.compiler.helpers.TypeUtilities.PAGE_OBJECT;
import static utam.compiler.helpers.TypeUtilities.ROOT_PAGE_OBJECT;

import org.testng.annotations.Test;
import utam.compiler.JsonBuilderTestUtility;
import utam.compiler.UtamCompilationError;
import utam.compiler.representation.PageObjectValidationTestHelper;
import utam.compiler.representation.PageObjectValidationTestHelper.MethodInfo;
import utam.compiler.representation.PageObjectValidationTestHelper.MethodParameterInfo;
import utam.core.declarative.representation.PageObjectMethod;
import utam.core.framework.consumer.UtamError;

/**
 * Provides tests for the ArgumentsProcessor type
 *
 * @author james.evans
 */
public class UtamArgumentTests {

  private static void testArgs(String argJson) {
    JsonBuilderTestUtility test = new JsonBuilderTestUtility();
    test.addRawString("methods",
        String.format(
            "[{ \"name\" : \"test\", \"compose\" : [ {\"apply\": \"self\", \"args\": [%s]} ]}]",
            argJson));
    test.getDeserializedJson();
  }

  @Test
  public void testDuplicateNamesThrows() {
    String json = "{\"name\":\"name\", \"type\":\"string\"}, {\"name\":\"name\", \"type\":\"string\"}";
    Exception e = expectThrows(UtamCompilationError.class, () -> testArgs(json));
    assertThat(e.getMessage(), containsString(
        "error 107: method \"test\": parameter with name \"name\" is already declared"));

  }

  @Test
  public void testUnknownTypeThrows() {
    String json = "{  \"name\" :  \"name\",  \"type\" : \"type\" }";
    Exception e = expectThrows(UtamCompilationError.class, () -> testArgs(json));
    assertThat(e.getMessage(), containsString(
        "error 103: method \"test\": unsupported argument type \"type\", supported are"));
  }

  @Test
  public void testDeserializationByNameType() {
    String json = "{  \"name\" :  \"name\",  \"type\" : \"string\" }";
    testArgs(json);

    json = "{  \"name\" :  \"name\",  \"type\" : \"number\" }";
    testArgs(json);

    json = "{  \"name\" :  \"name\",  \"type\" : \"boolean\" }";
    testArgs(json);

    json = "{  \"name\" :  \"name\",  \"type\" : \"locator\" }";
    testArgs(json);
  }

  @Test
  public void testDeserializationByValue() {
    String json = "{  \"value\" :  \"string\" }";
    testArgs(json);

    json = "{  \"value\" :  1 }";
    testArgs(json);

    json = "{  \"value\": true }";
    testArgs(json);

    json = "{  \"value\" : \"my/po/type\", \"type\" : \"pageObject\" }";
    testArgs(json);

    json = "{  \"value\" : { \"css\" : \"css\" }, \"type\" : \"locator\" }";
    testArgs(json);
  }

  @Test
  public void testSelectorArgByValueWithArgs() {
    String json = "{  \"type\" : \"locator\", \"value\" : { \"css\" : \"css[%s]\" , \"args\" : [{ \"value\" : \"1\"}] } }";
    testArgs(json);
  }

  @Test
  public void testUnknownElementThrows() {
    String json = "{  \"value\" :  \"argName\",  \"type\" : \"elementReference\" }";
    UtamError e = expectThrows(UtamError.class, () -> testArgs(json));
    assertThat(e.getMessage(),
        containsString(
            "error 101: method \"test\", element \"argName\" reference: "
                + "unknown element with name \"argName\" is referenced"));
  }

  @Test
  public void testNameMissingThrows() {
    String json = "{ \"type\" : \"string\" }";
    UtamError e = expectThrows(UtamError.class, () -> testArgs(json));
    assertThat(e.getMessage(),
        containsString("error 100: method \"test\": incorrect argument format: \n"
            + "Missing required creator property 'name'"));
  }

  @Test
  public void testTypeMissingThrows() {
    String json = "{ \"name\" : \"name\" }";
    UtamError e = expectThrows(UtamError.class, () -> testArgs(json));
    assertThat(e.getMessage(),
        containsString("error 100: method \"test\": incorrect argument format: \n"
            + "Missing required creator property 'type'"));
  }

  @Test
  public void testUnsupportedValueThrows() {
    String json = "{ \"value\": 1.024 }";
    UtamError e = expectThrows(UtamError.class, () -> testArgs(json));
    assertThat(e.getMessage(),
        containsString(
            "error 102: method \"test\": unsupported literal argument type \"1.024\""));
  }

  @Test
  public void testRedundantValueWithTypeThrows() {
    String json = "{ \"value\" : true, \"type\" : \"string\" }";
    UtamError e = expectThrows(UtamError.class, () -> testArgs(json));
    assertThat(e.getMessage(),
        containsString("error 100: method \"test\": incorrect argument format: \n"
            + "Unrecognized field \"type\""));
  }

  @Test
  public void testRedundantValueWithNameThrows() {
    String json = "{ \"value\" : true, \"name\" : \"name\" }";
    UtamError e = expectThrows(UtamError.class, () -> testArgs(json));
    assertThat(e.getMessage(),
        containsString("error 100: method \"test\": incorrect argument format: \n"
            + "Unrecognized field \"name\""));
  }

  @Test
  public void testMissingPredicate() {
    String json = "{  \"name\" :  \"name\",  \"type\" : \"function\" }";
    UtamError e = expectThrows(UtamError.class, () -> testArgs(json));
    assertThat(e.getMessage(),
        containsString("error 104: method \"test\": incorrect predicate format: \n"
            + "Missing required creator property 'predicate'"));
  }

  @Test
  public void testArgByValueUnknownObjectThrows() {
    String json = "{  \"value\" : { \"extra\" : true } }";
    UtamError e = expectThrows(UtamError.class, () -> testArgs(json));
    assertThat(e.getMessage(),
        containsString("error 102: method \"test\": unsupported literal argument type"));
  }

  @Test
  public void testElementReferenceRedundantPredicate() {
    String json = "{  \"value\" :  \"element\",  \"type\" : \"elementReference\", \"predicate\" : [] }";
    UtamError e = expectThrows(UtamError.class, () -> testArgs(json));
    assertThat(e.getMessage(), containsString(
        "error 106: method \"test\": incorrect format of elementReference argument: \n"
            + "Unrecognized field \"predicate\""));
  }

  @Test
  public void testElementReferenceNameMandatoryThrows() {
    String json = "{  \"type\" : \"elementReference\" }";
    UtamError e = expectThrows(UtamError.class, () -> testArgs(json));
    assertThat(e.getMessage(),
        containsString(
            "error 103: method \"test\": unsupported argument type \"elementReference\""));
  }

  @Test
  public void testUnsupportedPropertyThrows() {
    String json = "{  \"error\" : \"text\" }";
    UtamError e = expectThrows(UtamError.class, () -> testArgs(json));
    assertThat(e.getMessage(),
        containsString("error 100: method \"test\": incorrect argument format: \n"
            + "Missing required creator property 'name'"));
  }

  @Test
  public void testNonStringTypeThrows() {
    String json = "{  \"value\" :  \"string\",  \"type\" : {} }";
    Exception e = expectThrows(UtamCompilationError.class, () -> testArgs(json));
    assertThat(e.getMessage(),
        containsString(
            "error 10: method \"test\": property \"type\" should be a non empty string, instead found object"));
  }

  @Test
  public void testEmptyNestedArgsThrows() {
    String json = "{  \"value\" :  \"root\",  \"type\" : \"elementReference\", \"args\": [] }";
    //testArgs(json);
    Exception e = expectThrows(UtamCompilationError.class, () -> testArgs(json));
    assertThat(e.getMessage(),
        containsString(
            "error 12: method \"test\", element \"root\" reference: property \"args\" should be a not empty array"));
  }

  @Test
  public void testArgNotObjectThrows() {
    String json = "[]";
    Exception e = expectThrows(UtamCompilationError.class, () -> testArgs(json));
    assertThat(e.getMessage(),
        containsString(
            "error 13: method \"test\": argument should be a non empty object"));
  }

  @Test
  public void testPageObjectNonLiteralParameterWhenReturningVoid() {
    PageObjectMethod method = new DeserializerUtilities()
        .getContext("compose/args/nonLiteralPageObject").getMethod("test");
    MethodInfo expected = new MethodInfo("test");
    // if return type is not page object itself, use Class with wildcards
    // void test(Class<? extends PageObject> param), but T test(Class<T extends PageObject> param)
    assertThat(method.getDeclaration().getCodeLine(), is("void test(Class<? extends PageObject> param)"));
    expected.addParameter(new MethodParameterInfo("param", "Class<? extends PageObject>"));
    expected.addImpliedImportedTypes(PAGE_OBJECT.getFullName());
    expected.addImportedTypes(PAGE_OBJECT.getFullName());
    expected.addCodeLine("this.something(param)");
    PageObjectValidationTestHelper.validateMethod(method, expected);
  }

  @Test
  public void testRootPageObjectNonLiteralParameterWhenReturningVoid() {
    PageObjectMethod method = new DeserializerUtilities()
        .getContext("compose/args/nonLiteralRootPageObject").getMethod("test");
    MethodInfo expected = new MethodInfo("test");
    // if return type is not page object itself, use Class with wildcards
    // void test(Class<? extends RootPageObject> param), but T test(Class<T extends RootPageObject> param)
    assertThat(method.getDeclaration().getCodeLine(), is("void test(Class<? extends RootPageObject> param)"));
    expected.addParameter(new MethodParameterInfo("param", "Class<? extends RootPageObject>"));
    expected.addImpliedImportedTypes(ROOT_PAGE_OBJECT.getFullName());
    expected.addImportedTypes(ROOT_PAGE_OBJECT.getFullName());
    expected.addCodeLine("this.something(param)");
    PageObjectValidationTestHelper.validateMethod(method, expected);
  }

  @Test
  public void testRootPageObjectNonLiteralParameterWhenReturningNonVoid() {
    PageObjectMethod method = new DeserializerUtilities()
        .getContext("compose/args/nonLiteralRootPageObjectReturns").getMethod("test");
    MethodInfo expected = new MethodInfo("test", "T");
    // if return type is not page object itself, use Class with wildcards
    // void test(Class<? extends RootPageObject> param), but T test(Class<T extends RootPageObject> param)
    assertThat(method.getDeclaration().getCodeLine(), is("<T extends RootPageObject> T test(Class<T> param)"));
    expected.addParameter(new MethodParameterInfo("param", "Class<T>"));
    expected.addImpliedImportedTypes(ROOT_PAGE_OBJECT.getFullName());
    expected.addImportedTypes(ROOT_PAGE_OBJECT.getFullName());
    expected.addCodeLine("T statement0 = this.something(param)");
    expected.addCodeLine("return statement0");
    PageObjectValidationTestHelper.validateMethod(method, expected);
  }
}
