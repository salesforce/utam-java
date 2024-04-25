/*
 * Copyright (c) 2023, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.testng.Assert.expectThrows;
import static utam.compiler.grammar.UtamPageObject.BEFORE_LOAD_METHOD_NAME;

import org.testng.annotations.Test;
import utam.compiler.UtamCompilationError;
import utam.compiler.representation.PageObjectValidationTestHelper;
import utam.compiler.representation.PageObjectValidationTestHelper.MethodInfo;
import utam.core.declarative.representation.PageObjectMethod;

/**
 * Tests for "load" inside UtamElement
 *
 * @author kperumal
 * @since 250
 */
public class UtamElementLoadTests {

  static PageObjectMethod getMethod(String jsonFile, String methodName) {
    return new DeserializerUtilities().getContext(jsonFile).getMethod(methodName);
  }

  @Test
  public void testLoadInPrivateBasicElement() {
    // verify private wait method is created for private element
    String privateElementWaitMethodName = "waitForBasicPrivateElement";
    PageObjectMethod privateElementWaitMethod =
        getMethod("generated/load/loadBasicPrivateElement.utam", privateElementWaitMethodName);
    MethodInfo expectedPrivateWait =
        new MethodInfo(privateElementWaitMethodName, "BasicPrivateElementElement");
    expectedPrivateWait.setNotPublic();
    expectedPrivateWait.addCodeLine(
        "BasicPrivateElementElement statement0 = this.waitFor(() -> {\n"
            + "BasicPrivateElementElement pstatement0 = this.getBasicPrivateElementElement();\n"
            + "return pstatement0;\n"
            + "})");
    expectedPrivateWait.addCodeLine("return statement0");
    PageObjectValidationTestHelper.validateMethod(privateElementWaitMethod, expectedPrivateWait);

    // verify beforeLoad method has call to both private wait methods
    PageObjectMethod beforeLoadMethod =
        getMethod("generated/load/loadBasicPrivateElement.utam", BEFORE_LOAD_METHOD_NAME);
    MethodInfo expectedBeforeLoad = new MethodInfo(BEFORE_LOAD_METHOD_NAME, "Object");
    expectedBeforeLoad.addCodeLine("this.waitForBasicPrivateElement()");
    expectedBeforeLoad.addCodeLine("return this");
    PageObjectValidationTestHelper.validateMethod(beforeLoadMethod, expectedBeforeLoad);
  }

  @Test
  public void testLoadInPublicBasicElement() {
    // verify private wait method is created for private element
    String publicElementWaitMethodName = "waitForBasicPublicElement";
    PageObjectMethod publicElementWaitMethod =
        getMethod("generated/load/loadBasicPublicElement.utam", publicElementWaitMethodName);
    MethodInfo expectedPrivateWait =
        new MethodInfo(publicElementWaitMethodName, "BasicPublicElementElement");
    expectedPrivateWait.setNotPublic();
    expectedPrivateWait.addCodeLine(
        "BasicPublicElementElement statement0 = this.waitFor(() -> {\n"
            + "BasicPublicElementElement pstatement0 = this.getBasicPublicElement();\n"
            + "return pstatement0;\n"
            + "})");
    expectedPrivateWait.addCodeLine("return statement0");
    PageObjectValidationTestHelper.validateMethod(publicElementWaitMethod, expectedPrivateWait);

    // verify beforeLoad method has call to both private wait methods
    PageObjectMethod beforeLoadMethod =
        getMethod("generated/load/loadBasicPublicElement.utam", BEFORE_LOAD_METHOD_NAME);
    MethodInfo expectedBeforeLoad = new MethodInfo(BEFORE_LOAD_METHOD_NAME, "Object");
    expectedBeforeLoad.addCodeLine("this.waitForBasicPublicElement()");
    expectedBeforeLoad.addCodeLine("return this");
    PageObjectValidationTestHelper.validateMethod(beforeLoadMethod, expectedBeforeLoad);
  }

  @Test
  public void testLoadAndWaitInPublicBasicElement() {
    // verify private wait method is created for private element
    String publicElementWaitMethodName = "waitForBasicPublicElement";
    PageObjectMethod publicElementWaitMethod =
        getMethod("generated/load/loadAndWaitBasicElement.utam", publicElementWaitMethodName);
    MethodInfo expectedPrivateWait =
        new MethodInfo(publicElementWaitMethodName, "BasicPublicElementElement");
    expectedPrivateWait.addCodeLine(
        "BasicPublicElementElement statement0 = this.waitFor(() -> {\n"
            + "BasicPublicElementElement pstatement0 = this.getBasicPublicElement();\n"
            + "return pstatement0;\n"
            + "})");
    expectedPrivateWait.addCodeLine("return statement0");
    PageObjectValidationTestHelper.validateMethod(publicElementWaitMethod, expectedPrivateWait);

    // verify beforeLoad method has call to both private wait methods
    PageObjectMethod beforeLoadMethod =
        getMethod("generated/load/loadAndWaitBasicElement.utam", BEFORE_LOAD_METHOD_NAME);
    MethodInfo expectedBeforeLoad = new MethodInfo(BEFORE_LOAD_METHOD_NAME, "Object");
    expectedBeforeLoad.addCodeLine("this.waitForBasicPublicElement()");
    expectedBeforeLoad.addCodeLine("return this");
    PageObjectValidationTestHelper.validateMethod(beforeLoadMethod, expectedBeforeLoad);
  }

  @Test
  public void testLoadInCustomElement() {
    // verify private wait method is created for private element
    String privateElementWaitMethodName = "waitForCustomElement";
    PageObjectMethod privateElementWaitMethod =
        getMethod("generated/load/loadCustomElement.utam", privateElementWaitMethodName);
    MethodInfo expectedPrivateWait =
        new MethodInfo(privateElementWaitMethodName, "LoadBasicPrivateElement");
    expectedPrivateWait.setNotPublic();
    expectedPrivateWait.addCodeLine(
        "LoadBasicPrivateElement statement0 = this.waitFor(() -> {\n"
            + "LoadBasicPrivateElement pstatement0 = this.getCustomElementElement();\n"
            + "return pstatement0;\n"
            + "})");
    expectedPrivateWait.addCodeLine("return statement0");
    PageObjectValidationTestHelper.validateMethod(privateElementWaitMethod, expectedPrivateWait);

    // verify beforeLoad method has call to both private wait methods
    PageObjectMethod beforeLoadMethod =
        getMethod("generated/load/loadCustomElement.utam", BEFORE_LOAD_METHOD_NAME);
    MethodInfo expectedBeforeLoad = new MethodInfo(BEFORE_LOAD_METHOD_NAME, "Object");
    expectedBeforeLoad.addCodeLine("this.waitForCustomElement()");
    expectedBeforeLoad.addCodeLine("return this");
    PageObjectValidationTestHelper.validateMethod(beforeLoadMethod, expectedBeforeLoad);
  }

  @Test
  public void testLoadInFrame() {
    String methodName = "waitForFrameElement";
    PageObjectMethod method = getMethod("generated/load/loadFrameElement.utam", methodName);
    MethodInfo expected = new MethodInfo(methodName, "FrameElement");
    expected.setNotPublic();
    expected.addCodeLine(
        "FrameElement statement0 = this.waitFor(() -> {\n"
            + "FrameElement pstatement0 = this.getFrameElementElement();\n"
            + "return pstatement0;\n"
            + "})");
    expected.addCodeLine("return statement0");
    PageObjectValidationTestHelper.validateMethod(method, expected);

    // verify beforeLoad method has call to both private wait methods
    PageObjectMethod beforeLoadMethod =
        getMethod("generated/load/loadFrameElement.utam", BEFORE_LOAD_METHOD_NAME);
    MethodInfo expectedBeforeLoad = new MethodInfo(BEFORE_LOAD_METHOD_NAME, "Object");
    expectedBeforeLoad.addCodeLine("this.waitForFrameElement()");
    expectedBeforeLoad.addCodeLine("return this");
    PageObjectValidationTestHelper.validateMethod(beforeLoadMethod, expectedBeforeLoad);
  }

  @Test
  public void testLoadElementWithFilterNoArguments() {
    String methodName = "waitForBasicElement";
    PageObjectMethod method = getMethod("generated/load/loadFilterElement.utam", methodName);
    MethodInfo expected = new MethodInfo(methodName, "BasicElement");
    expected.setNotPublic();
    expected.addCodeLine(
        "BasicElement statement0 = this.waitFor(() -> {\n"
            + "BasicElement pstatement0 = this.getBasicElement();\n"
            + "return pstatement0;\n"
            + "})");
    expected.addCodeLine("return statement0");
    PageObjectValidationTestHelper.validateMethod(method, expected);

    // verify beforeLoad method has call to both private wait methods
    PageObjectMethod beforeLoadMethod =
        getMethod("generated/load/loadFilterElement.utam", BEFORE_LOAD_METHOD_NAME);
    MethodInfo expectedBeforeLoad = new MethodInfo(BEFORE_LOAD_METHOD_NAME, "Object");
    expectedBeforeLoad.addCodeLine("this.waitForBasicElement()");
    expectedBeforeLoad.addCodeLine("return this");
    PageObjectValidationTestHelper.validateMethod(beforeLoadMethod, expectedBeforeLoad);
  }

  @Test
  public void testLoadNestedFilters() {
    // first basic element inside scope element with filter
    String methodName = "waitForBasicElement";
    PageObjectMethod method = getMethod("generated/load/loadNestedFilter.utam", methodName);
    MethodInfo expected = new MethodInfo(methodName, "BasicElement");
    expected.setNotPublic();
    expected.addCodeLine(
        "BasicElement statement0 = this.waitFor(() -> {\n"
            + "BasicElement pstatement0 = this.getBasicElementElement();\n"
            + "return pstatement0;\n"
            + "})");
    expected.addCodeLine("return statement0");
    PageObjectValidationTestHelper.validateMethod(method, expected);

    // second basic element with filter inside second scope element
    String methodTwoName = "waitForSecondElement";
    PageObjectMethod methodTwo = getMethod("generated/load/loadNestedFilter.utam", methodTwoName);
    MethodInfo expectedTwo = new MethodInfo(methodTwoName, "BasicElement");
    expectedTwo.setNotPublic();
    expectedTwo.addCodeLine(
        "BasicElement statement0 = this.waitFor(() -> {\n"
            + "BasicElement pstatement0 = this.getSecondElementElement();\n"
            + "return pstatement0;\n"
            + "})");
    expectedTwo.addCodeLine("return statement0");
    PageObjectValidationTestHelper.validateMethod(methodTwo, expectedTwo);

    // verify beforeLoad method has call to both private wait methods
    PageObjectMethod beforeLoadMethod =
        getMethod("generated/load/loadFilterElement.utam", BEFORE_LOAD_METHOD_NAME);
    MethodInfo expectedBeforeLoad = new MethodInfo(BEFORE_LOAD_METHOD_NAME, "Object");
    expectedBeforeLoad.addCodeLine("this.waitForBasicElement()");
    expectedBeforeLoad.addCodeLine("this.waitForSecondElement()");
    expectedBeforeLoad.addCodeLine("return this");
  }

  @Test
  public void testElementWithArguments() {
    Exception e =
        expectThrows(
            UtamCompilationError.class,
            () -> getMethod("validate/basic_element/loadElementWithArguments", "test"));
    assertThat(
        e.getMessage(),
        containsString(
            "error 206: element \"basicElement\": property \"load\" is not supported for element"
                + " with arguments, filter or for container element"));
  }

  @Test
  public void testElementWithFilterArguments() {
    Exception e =
        expectThrows(
            UtamCompilationError.class,
            () -> getMethod("validate/basic_element/loadFilterElementWithArgs", "getTest"));
    assertThat(
        e.getMessage(),
        containsString(
            "error 206: element \"test\": property \"load\" is not supported for element with"
                + " arguments, filter or for container element"));
  }

  @Test
  public void testLoadInNestedBasicElement() {
    // verify private wait method is created for private element
    String privateElementWaitMethodName = "waitForBasicElement";
    PageObjectMethod privateElementWaitMethod =
        getMethod("generated/load/loadNested.utam", privateElementWaitMethodName);
    MethodInfo expectedPrivateWait = new MethodInfo(privateElementWaitMethodName, "BasicElement");
    expectedPrivateWait.setNotPublic();
    expectedPrivateWait.addCodeLine(
        "BasicElement statement0 = this.waitFor(() -> {\n"
            + "BasicElement pstatement0 = this.getBasicElementElement();\n"
            + "return pstatement0;\n"
            + "})");
    expectedPrivateWait.addCodeLine("return statement0");
    PageObjectValidationTestHelper.validateMethod(privateElementWaitMethod, expectedPrivateWait);

    // verify beforeLoad method has call to both private wait methods
    PageObjectMethod beforeLoadMethod =
        getMethod("generated/load/loadNested.utam", BEFORE_LOAD_METHOD_NAME);
    MethodInfo expectedBeforeLoad = new MethodInfo(BEFORE_LOAD_METHOD_NAME, "Object");
    expectedBeforeLoad.addCodeLine("this.waitForBasicElement()");
    expectedBeforeLoad.addCodeLine("return this");
    PageObjectValidationTestHelper.validateMethod(beforeLoadMethod, expectedBeforeLoad);
  }

  @Test
  public void testLoadInNestedBasicElementWithArgs() {
    Exception e =
        expectThrows(
            UtamCompilationError.class,
            () -> getMethod("validate/basic_element/loadNestedElementWithArgs", "getTest"));
    assertThat(
        e.getMessage(),
        containsString(
            "error 206: element \"basicElement\": property \"load\" is not supported for element"
                + " with arguments, filter or for container element"));
  }

  @Test
  public void testNestedFilterElementWithScopeArguments() {
    Exception e =
        expectThrows(
            UtamCompilationError.class,
            () -> getMethod("validate/basic_element/loadNestedFilterWithArgs", "getTest"));
    assertThat(
        e.getMessage(),
        containsString(
            "error 206: element \"test\": property \"load\" is not supported for element with"
                + " arguments, filter or for container element"));
  }
}
