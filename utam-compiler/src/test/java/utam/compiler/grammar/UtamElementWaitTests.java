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
import static utam.compiler.helpers.TypeUtilities.PAGE_OBJECT;

import org.testng.annotations.Test;
import utam.compiler.UtamCompilationError;
import utam.compiler.representation.PageObjectValidationTestHelper;
import utam.compiler.representation.PageObjectValidationTestHelper.MethodInfo;
import utam.compiler.representation.PageObjectValidationTestHelper.MethodParameterInfo;
import utam.core.declarative.representation.PageObjectMethod;

/**
 * Tests for "wait" inside UtamElement
 *
 * @author elizaveta.ivanova
 * @since 248
 */
public class UtamElementWaitTests {

  PageObjectMethod getMethod(String jsonFile, String methodName) {
    return new DeserializerUtilities().getContext(jsonFile).getMethod(methodName);
  }

  @Test
  public void testWaitInPrivateBasicElement() {
    String methodName = "waitForBasicPrivateElement";
    PageObjectMethod method = getMethod("generated/wait/waitForBasicElement.utam", methodName);
    MethodInfo expected = new MethodInfo(methodName, "BasicPrivateElementElement");
    expected.addCodeLine(
        "BasicPrivateElementElement statement0 = this.waitFor(() -> {\n"
            + "BasicPrivateElementElement pstatement0 = this.getBasicPrivateElementElement();\n"
            + "return pstatement0;\n"
            + "})");
    expected.addCodeLine("return statement0");
    PageObjectValidationTestHelper.validateMethod(method, expected);
  }

  @Test
  public void testWaitInPublicBasicElement() {
    String methodName = "waitForBasicPublicElement";
    PageObjectMethod method = getMethod("generated/wait/waitForBasicElement.utam", methodName);
    MethodInfo expected = new MethodInfo(methodName, "BasicElement");
    expected.addCodeLine(
        "BasicElement statement0 = this.waitFor(() -> {\n"
            + "BasicElement pstatement0 = this.getBasicPublicElement(selectorArg);\n"
            + "return pstatement0;\n"
            + "})");
    expected.addCodeLine("return statement0");
    expected.addParameter(new MethodParameterInfo("selectorArg"));
    PageObjectValidationTestHelper.validateMethod(method, expected);
  }

  @Test
  public void testWaitInCustomElementWithFilter() {
    String methodName = "waitForCustomElementWithFilter";
    PageObjectMethod method = getMethod("generated/wait/waitForElement.utam", methodName);
    MethodInfo expected = new MethodInfo(methodName, "List<WaitForBasicElement>");
    expected.addCodeLine(
        "List<WaitForBasicElement> statement0 = this.waitFor(() -> {\n"
            + "List<WaitForBasicElement> pstatement0 ="
            + " this.getCustomElementWithFilterElement(matcherArg);\n"
            + "return pstatement0;\n"
            + "})");
    expected.addCodeLine("return statement0");
    expected.addParameter(new MethodParameterInfo("matcherArg"));
    PageObjectValidationTestHelper.validateMethod(method, expected);
  }

  @Test
  public void testWaitInFrame() {
    String methodName = "waitForFrameElement";
    PageObjectMethod method = getMethod("generated/wait/waitForElement.utam", methodName);
    MethodInfo expected = new MethodInfo(methodName, "FrameElement");
    expected.addCodeLine(
        "FrameElement statement0 = this.waitFor(() -> {\n"
            + "FrameElement pstatement0 = this.getFrameElementElement();\n"
            + "return pstatement0;\n"
            + "})");
    expected.addCodeLine("return statement0");
    PageObjectValidationTestHelper.validateMethod(method, expected);
  }

  @Test
  public void testWaitInContainer() {
    String methodName = "waitForContainer";
    PageObjectMethod method = getMethod("generated/wait/waitForContainer.utam", methodName);
    MethodInfo expected = new MethodInfo(methodName, "T");
    expected.addParameter(new MethodParameterInfo("pageObjectType", "Class<T>"));
    expected.addImportedTypes(PAGE_OBJECT.getFullName());
    expected.addImpliedImportedTypes(PAGE_OBJECT.getFullName());
    expected.addCodeLine(
        "T statement0 = this.waitFor(() -> {\n"
            + "T pstatement0 = this.getContainerElement(pageObjectType);\n"
            + "return pstatement0;\n"
            + "})");
    expected.addCodeLine("return statement0");
    PageObjectValidationTestHelper.validateMethod(method, expected);
  }

  @Test
  public void testWaitNotBoolean() {
    Exception e =
        expectThrows(
            UtamCompilationError.class,
            () -> getMethod("validate/basic_element/waitWrongType", "test"));
    assertThat(
        e.getMessage(),
        containsString(
            "error 200: root elements: incorrect format of elements \n"
                + "Cannot deserialize value of type `java.lang.Boolean` from String \"string\""));
  }
}
