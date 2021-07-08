/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import org.testng.annotations.Test;
import utam.compiler.helpers.*;
import utam.compiler.representation.PageObjectValidationTestHelper;
import utam.compiler.representation.PageObjectValidationTestHelper.MethodInfo;
import utam.core.declarative.representation.PageObjectMethod;
import utam.core.framework.consumer.UtamError;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;
import static org.testng.Assert.expectThrows;
import static utam.compiler.grammar.TestUtilities.getElementPrivateMethodCalled;
import static utam.compiler.grammar.UtamMethod.ERR_BEFORE_LOAD_HAS_NO_ARGS;
import static utam.compiler.grammar.UtamMethod.ERR_METHOD_EMPTY_STATEMENTS;
import static utam.compiler.helpers.MethodContext.BEFORE_LOAD_METHOD_MANE;

/**
 * Provides tests for UtamMethod class with beforeLoad method.
 *
 * @author igor.khorev
 */
public class UtamMethod_BeforeLoadTests {

    private static final String METHOD_NAME = "testMethod";
    private static final String ELEMENT_NAME_1 = "testElement1";
    private static final String ELEMENT_NAME_2 = "testElement2";

    private static void setClickableElementContext(TranslationContext context, String elementName) {
        TestUtilities.UtamEntityCreator.createUtamElement(
            elementName, new String[] {"clickable"}, new UtamSelector(".fakeSelector"))
                .testTraverse(context);
    }

    /**
     * The getBeforeLoadMethod method should return a valid value for a beforeLoad method.
     */
    @Test
    public void testGetBeforeLoadMethod() {
        TranslationContext context = TestUtilities.getTestTranslationContext();
        // traverses
        setClickableElementContext(context, ELEMENT_NAME_1);
        setClickableElementContext(context, ELEMENT_NAME_2);
        UtamMethod method =
            TestUtilities.UtamEntityCreator.createUtamMethod(
                    BEFORE_LOAD_METHOD_MANE,
                        new UtamMethodAction[] {
                                new UtamMethodAction(ELEMENT_NAME_1, "isPresent"),
                                new UtamMethodAction(ELEMENT_NAME_2, "isVisible")
                        });
        PageObjectValidationTestHelper.MethodInfo methodInfo =
                new PageObjectValidationTestHelper.MethodInfo(BEFORE_LOAD_METHOD_MANE, "void");
        methodInfo.addCodeLine("this.getTestElement1Element().isPresent()");
        methodInfo.addCodeLine(getElementPrivateMethodCalled(ELEMENT_NAME_2) + "().isVisible()");

        PageObjectMethod methodObject = method.getBeforeLoadMethod(context);
        PageObjectValidationTestHelper.validateMethod(methodObject, methodInfo);
    }

    @Test
    public void testBeforeLoadArgsThrows() {
        TranslationContext context = TestUtilities.getTestTranslationContext();
        UtamMethod method = TestUtilities.UtamEntityCreator.createUtamMethod(
            METHOD_NAME, new UtamMethodAction[] {
                new UtamMethodAction("self", "isPresent")
            });
        method.compose[0].args = new UtamArgument[] {
            new UtamArgument("str", "string")
        };
        UtamError e = expectThrows(UtamError.class, () -> method.getBeforeLoadMethod(context));
        assertThat(e.getMessage(), containsString(ERR_BEFORE_LOAD_HAS_NO_ARGS));
    }

    /**
     * The getBeforeLoadMethod should throw a proper UtamError if UtamMethodAction list has zero statements.
     */
    @Test
    public void testGetBeforeLoadMethodWithEmptyStatementListThrows() {
        TranslationContext context = TestUtilities.getTestTranslationContext();
        UtamMethod method = TestUtilities.UtamEntityCreator.createUtamMethod(
            BEFORE_LOAD_METHOD_MANE, new UtamMethodAction[] {});
        UtamError e = expectThrows(UtamError.class, () -> method.getBeforeLoadMethod(context));
        assertThat(e.getMessage(), containsString(String.format(ERR_METHOD_EMPTY_STATEMENTS, BEFORE_LOAD_METHOD_MANE)));
    }

    @Test
    public void testBeforeLoad() {
        MethodInfo methodInfo = new MethodInfo(BEFORE_LOAD_METHOD_MANE, "void");
        methodInfo.addCodeLine("this.getRootElement().isPresent()");
        methodInfo.addCodeLine("this.getRootElement().getText()");
        TranslationContext context = new DeserializerUtilities().getContext("beforeLoadMethod");
        PageObjectValidationTestHelper.validateMethod(context.getMethod(BEFORE_LOAD_METHOD_MANE), methodInfo);
    }
}
