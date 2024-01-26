/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.emptyIterable;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.core.StringContains.containsString;
import static org.testng.Assert.expectThrows;
import static utam.compiler.helpers.TypeUtilities.SELECTOR;

import org.testng.annotations.Test;
import utam.compiler.JsonBuilderTestUtility;
import utam.compiler.grammar.DeserializerUtilities.Result;
import utam.compiler.representation.PageObjectValidationTestHelper;
import utam.compiler.representation.PageObjectValidationTestHelper.MethodInfo;
import utam.compiler.representation.PageObjectValidationTestHelper.MethodParameterInfo;
import utam.core.declarative.representation.PageObjectMethod;
import utam.core.element.BasicElement;
import utam.core.framework.consumer.UtamError;
import utam.core.framework.element.BasePageElement;

/**
 * Tests for composing root element actions and defining root element type
 *
 * @author elizaveta.ivanova
 * @since 236
 */
public class UtamMethodActionApplyRootTests {

  private static final String TEST_METHOD_NAME = "test";
  private static final String ROOT_METHOD_NAME = "getRoot";

  private static Result getResult(String json) {
    return new DeserializerUtilities().getResultFromFile("compose/root/" + json);
  }

  private static PageObjectMethod getRootMethod(Result result) {
    return result.getContext().getMethod(ROOT_METHOD_NAME);
  }

  private static PageObjectMethod getMethod(Result result) {
    return result.getContext().getMethod(TEST_METHOD_NAME);
  }

  @Test
  public void testPrivateRootNoTypeBasicAction() {
    Result result = getResult("rootContains");
    PageObjectMethod actualMethod = getMethod(result);
    MethodInfo expected = new MethodInfo(TEST_METHOD_NAME, "Boolean");
    expected.addImpliedImportedTypes(SELECTOR.getFullName());
    expected.addImpliedImportedTypes(BasePageElement.class.getName());
    expected.addCodeLine("BasePageElement root0 = this.getRootElement()");
    expected.addCodeLine(
        "Boolean statement0 ="
            + " root0.containsElement(LocatorBy.byCss(String.format(\".foo[title='%s']\","
            + " title)))");
    expected.addCodeLine("return statement0");
    expected.addParameter(new MethodParameterInfo("title"));
    PageObjectValidationTestHelper.validateMethod(actualMethod, expected);
  }

  @Test
  public void testPublicRootNoTypeGetText() {
    Result result = getResult("publicRootNoType");
    PageObjectMethod actualMethod = getMethod(result);
    MethodInfo expected = new MethodInfo(TEST_METHOD_NAME, "String");
    Class returnType = BasicElement.class;
    expected.addImpliedImportedTypes(returnType.getName());
    expected.addCodeLine("BasicElement root0 = this.getRoot()");
    expected.addCodeLine("String statement0 = root0.getText()");
    expected.addCodeLine("return statement0");
    PageObjectValidationTestHelper.validateMethod(actualMethod, expected);

    PageObjectMethod rootMethod = getRootMethod(result);
    MethodInfo expectedRootMethod = new MethodInfo(ROOT_METHOD_NAME, returnType.getSimpleName());
    expectedRootMethod.addImpliedImportedTypes(returnType.getName());
    expectedRootMethod.addCodeLine("return this.getRootElement()");
    PageObjectValidationTestHelper.validateMethod(rootMethod, expectedRootMethod);
  }

  @Test
  public void testPublicRootNoTypeClickThrows() {
    UtamError e = expectThrows(UtamError.class, () -> getResult("publicRootWrongAction"));
    assertThat(
        e.getMessage(),
        containsString(
            "error 612: method \"test\" statement: unknown method \"click\" for basic element"));
  }

  @Test
  public void testPrivateRootWrongTypeClickThrows() {
    UtamError e = expectThrows(UtamError.class, () -> getResult("privateRootWrongAction"));
    assertThat(
        e.getMessage(),
        containsString(
            "error 617: method \"test\" statement: unsupported action \"click\" for type"
                + " \"Editable\""));
  }

  @Test
  public void testPrivateRootUnknownActionThrows() {
    UtamError e = expectThrows(UtamError.class, () -> getResult("privateRootUnknownAction"));
    assertThat(
        e.getMessage(),
        containsString(
            "error 612: method \"test\" statement: unknown method \"test\" for basic element"));
  }

  @Test
  public void testPublicRootWithTypeClick() {
    Result result = getResult("publicRootWithType");
    PageObjectMethod actualMethod = getMethod(result);
    MethodInfo expected = new MethodInfo(TEST_METHOD_NAME);
    expected.addCodeLine("RootElement root0 = this.getRoot()");
    expected.addCodeLine("root0.click()");
    PageObjectValidationTestHelper.validateMethod(actualMethod, expected);

    PageObjectMethod rootMethod = getRootMethod(result);
    MethodInfo expectedRootMethod = new MethodInfo(ROOT_METHOD_NAME, "RootElement");
    expectedRootMethod.addCodeLine("return getProxy(this.getRootElement(), RootElement.class)");
    PageObjectValidationTestHelper.validateMethod(rootMethod, expectedRootMethod);
  }

  @Test
  public void testPrivateRootWithTypeClick() {
    Result result = getResult("privateRootWithType");
    PageObjectMethod actualMethod = getMethod(result);
    MethodInfo expected = new MethodInfo(TEST_METHOD_NAME);
    expected.addCodeLine("RootElement root0 = this.getRoot()");
    expected.addCodeLine("root0.click()");
    PageObjectValidationTestHelper.validateMethod(actualMethod, expected);

    PageObjectMethod rootMethod = getRootMethod(result);
    assertThat(rootMethod.isPublic(), is(false));
    MethodInfo expectedRootMethod = new MethodInfo(ROOT_METHOD_NAME, "RootElement");
    expectedRootMethod.setNotPublic();
    assertThat(rootMethod.getClassImports(), is(emptyIterable()));
    expectedRootMethod.addCodeLine("return getProxy(this.getRootElement(), RootElement.class)");
    PageObjectValidationTestHelper.validateMethod(rootMethod, expectedRootMethod);

    assertThat(result.getContext().getInterfaceUnionTypes().size(), is(equalTo(1)));
    assertThat(
        result.getContext().getInterfaceUnionTypes().get(0).getDeclarationCode().get(0),
        is(equalTo("interface RootElement extends Clickable {}")));
    assertThat(result.getContext().getClassUnionTypes().size(), is(equalTo(0)));
  }

  @Test
  public void incorrectComposeFormatThrows() {
    JsonBuilderTestUtility test = new JsonBuilderTestUtility();
    test.addRawString(
        "methods",
        "[{ \"name\" : \"test\", \"compose\" : [ {\"element\": \"root\", \"apply\": []} ]}]");
    Exception e = test.expectCompilerError();
    assertThat(
        e.getMessage(),
        containsString("error 600: method \"test\" statement: incorrect compose statement format"));
  }

  @Test
  public void incorrectComposeNotObjectFormatThrows() {
    JsonBuilderTestUtility test = new JsonBuilderTestUtility();
    test.addRawString("methods", "[{ \"name\" : \"test\", \"compose\" : [ true ]}]");
    Exception e = test.expectCompilerError();
    assertThat(
        e.getMessage(),
        containsString(
            "error 13: method \"test\": compose statement should be a non-empty object"));
  }
}
