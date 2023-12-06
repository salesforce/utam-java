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
import static utam.compiler.grammar.UtamPageObject.BEFORE_LOAD_METHOD_NAME;

import org.testng.annotations.Test;
import utam.compiler.UtamCompilationError;
import utam.compiler.representation.PageObjectValidationTestHelper;
import utam.compiler.representation.PageObjectValidationTestHelper.MethodInfo;
import utam.compiler.representation.PageObjectValidationTestHelper.MethodParameterInfo;
import utam.core.declarative.representation.PageObjectMethod;

/**
 * Tests for "load" inside UtamElement
 *
 * @author kperumal
 * @since 250
 */
public class UtamElementLoadTests {

    PageObjectMethod getMethod(String jsonFile, String methodName) {
        return new DeserializerUtilities().getContext(jsonFile)
                .getMethod(methodName);
    }

    @Test
    public void testLoadInPrivateBasicElement() {
        //verify private wait method is created for private element
        String privateElementWaitMethodName = "waitForBasicPrivateElement";
        PageObjectMethod privateElementWaitMethod = getMethod("generated/load/loadBasicPrivateElement.utam", privateElementWaitMethodName);
        MethodInfo expectedPrivateWait = new MethodInfo(privateElementWaitMethodName, "BasicPrivateElementElement");
        expectedPrivateWait.setNotPublic();
        expectedPrivateWait.addCodeLine("BasicPrivateElementElement statement0 = this.waitFor(() -> {\n"
                + "BasicPrivateElementElement pstatement0 = this.getBasicPrivateElementElement();\n"
                + "return pstatement0;\n"
                + "})");
        expectedPrivateWait.addCodeLine("return statement0");
        PageObjectValidationTestHelper.validateMethod(privateElementWaitMethod, expectedPrivateWait);

        //verify beforeLoad method has call to both private wait methods
        PageObjectMethod beforeLoadMethod = getMethod("generated/load/loadBasicPrivateElement.utam", BEFORE_LOAD_METHOD_NAME);
        MethodInfo expectedBeforeLoad = new MethodInfo(BEFORE_LOAD_METHOD_NAME, "Object");
        expectedBeforeLoad.addCodeLine("this.waitForBasicPrivateElement()");
        expectedBeforeLoad.addCodeLine("return this");
        PageObjectValidationTestHelper.validateMethod(beforeLoadMethod, expectedBeforeLoad);
    }

    @Test
    public void testLoadInPublicBasicElement() {
        //verify private wait method is created for private element
        String publicElementWaitMethodName = "waitForBasicPublicElement";
        PageObjectMethod publicElementWaitMethod = getMethod("generated/load/loadBasicPublicElement.utam", publicElementWaitMethodName);
        MethodInfo expectedPrivateWait = new MethodInfo(publicElementWaitMethodName, "BasicPublicElementElement");
        expectedPrivateWait.setNotPublic();
        expectedPrivateWait.addCodeLine("BasicPublicElementElement statement0 = this.waitFor(() -> {\n"
                + "BasicPublicElementElement pstatement0 = this.getBasicPublicElement();\n"
                + "return pstatement0;\n"
                + "})");
        expectedPrivateWait.addCodeLine("return statement0");
        PageObjectValidationTestHelper.validateMethod(publicElementWaitMethod, expectedPrivateWait);

        //verify beforeLoad method has call to both private wait methods
        PageObjectMethod beforeLoadMethod = getMethod("generated/load/loadBasicPublicElement.utam", BEFORE_LOAD_METHOD_NAME);
        MethodInfo expectedBeforeLoad = new MethodInfo(BEFORE_LOAD_METHOD_NAME, "Object");
        expectedBeforeLoad.addCodeLine("this.waitForBasicPublicElement()");
        expectedBeforeLoad.addCodeLine("return this");
        PageObjectValidationTestHelper.validateMethod(beforeLoadMethod, expectedBeforeLoad);
    }

    @Test
    public void testLoadAndWaitInPublicBasicElement() {
        //verify private wait method is created for private element
        String publicElementWaitMethodName = "waitForBasicPublicElement";
        PageObjectMethod publicElementWaitMethod = getMethod("generated/load/loadAndWaitBasicElement.utam", publicElementWaitMethodName);
        MethodInfo expectedPrivateWait = new MethodInfo(publicElementWaitMethodName, "BasicPublicElementElement");
        expectedPrivateWait.addCodeLine("BasicPublicElementElement statement0 = this.waitFor(() -> {\n"
                + "BasicPublicElementElement pstatement0 = this.getBasicPublicElement();\n"
                + "return pstatement0;\n"
                + "})");
        expectedPrivateWait.addCodeLine("return statement0");
        PageObjectValidationTestHelper.validateMethod(publicElementWaitMethod, expectedPrivateWait);

        //verify beforeLoad method has call to both private wait methods
        PageObjectMethod beforeLoadMethod = getMethod("generated/load/loadAndWaitBasicElement.utam", BEFORE_LOAD_METHOD_NAME);
        MethodInfo expectedBeforeLoad = new MethodInfo(BEFORE_LOAD_METHOD_NAME, "Object");
        expectedBeforeLoad.addCodeLine("this.waitForBasicPublicElement()");
        expectedBeforeLoad.addCodeLine("return this");
        PageObjectValidationTestHelper.validateMethod(beforeLoadMethod, expectedBeforeLoad);
    }

    @Test
    public void testLoadInFrame() {
        String methodName = "waitForFrameElement";
        PageObjectMethod method = getMethod("generated/load/loadFrameElement.utam", methodName);
        MethodInfo expected = new MethodInfo(methodName, "FrameElement");
        expected.setNotPublic();
        expected.addCodeLine("FrameElement statement0 = this.waitFor(() -> {\n"
                + "FrameElement pstatement0 = this.getFrameElementElement();\n"
                + "return pstatement0;\n"
                + "})");
        expected.addCodeLine("return statement0");
        PageObjectValidationTestHelper.validateMethod(method, expected);

        //verify beforeLoad method has call to both private wait methods
        PageObjectMethod beforeLoadMethod = getMethod("generated/load/loadFrameElement.utam", BEFORE_LOAD_METHOD_NAME);
        MethodInfo expectedBeforeLoad = new MethodInfo(BEFORE_LOAD_METHOD_NAME, "Object");
        expectedBeforeLoad.addCodeLine("this.waitForFrameElement()");
        expectedBeforeLoad.addCodeLine("return this");
    }

}
