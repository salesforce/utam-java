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
import utam.core.declarative.representation.PageObjectMethod;
import utam.core.framework.consumer.UtamError;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;
import static org.testng.Assert.expectThrows;
import static utam.compiler.grammar.TestUtilities.getElementPrivateMethodCalled;
import static utam.compiler.grammar.UtamMethod.ERR_ARGS_NOT_ALLOWED;
import static utam.compiler.grammar.UtamMethod.ERR_METHOD_EMPTY_STATEMENTS;
import static utam.compiler.grammar.UtamMethod.ERR_DUPLICATED_STATEMENT;

/**
 * Provides tests for UtamMethod class with beforeLoad method.
 *
 * @author igor.khorev
 */
public class UtamMethod_BeforeLoadTests {

    private static final String METHOD_NAME = "testMethod";
    private static final String ELEMENT_NAME_1 = "testElement1";
    private static final String ELEMENT_NAME_2 = "testElement2";
    private static String getErr(String message) {
        return String.format(message, METHOD_NAME);
    }

    private static void setClickableElementContext(TranslationContext context, String elementName) {
        new UtamElement(elementName, "clickable", new UtamSelector(".fakeSelector"))
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
                new UtamMethod(
                        "load",
                        new UtamMethodAction[] {
                                new UtamMethodAction(ELEMENT_NAME_1, "isPresent"),
                                new UtamMethodAction(ELEMENT_NAME_2, "isVisible")
                        });
        PageObjectValidationTestHelper.MethodInfo methodInfo =
                new PageObjectValidationTestHelper.MethodInfo("load", "void");
        methodInfo.addCodeLine(getElementPrivateMethodCalled(ELEMENT_NAME_1) + "().isPresent()");
        methodInfo.addCodeLine(getElementPrivateMethodCalled(ELEMENT_NAME_2) + "().isVisible()");

        PageObjectMethod methodObject = method.getBeforeLoadMethod(context);
        PageObjectValidationTestHelper.validateMethod(methodObject, methodInfo);
    }

    /**
     * The beforeLoad method should throw proper UtamError if UtamMethodAction list contains duplicated statements.
     */
    @Test
    public void testBeforeLoadMethodWithDuplicatedStatementsThrows() {
        TranslationContext context = TestUtilities.getTestTranslationContext();
        // traverses
        setClickableElementContext(context, ELEMENT_NAME_1);
        UtamMethod method =
                new UtamMethod(
                        "load",
                        new UtamMethodAction[] {
                                new UtamMethodAction(ELEMENT_NAME_1, "isPresent"),
                                new UtamMethodAction(ELEMENT_NAME_1, "isPresent")
                        });
        PageObjectValidationTestHelper.MethodInfo methodInfo =
                new PageObjectValidationTestHelper.MethodInfo("load", "void");
        methodInfo.addCodeLine(getElementPrivateMethodCalled(ELEMENT_NAME_1) + "().isPresent()");
        methodInfo.addCodeLine(getElementPrivateMethodCalled(ELEMENT_NAME_1) + "().isPresent()");

        UtamError e = expectThrows(UtamError.class, () -> method.getBeforeLoadMethod(context));
        assertThat(e.getMessage(), containsString(String.format(ERR_DUPLICATED_STATEMENT, ELEMENT_NAME_1, "isPresent")));
    }

    /**
     * The beforeLoad method should throw proper UtamError if UtamArgumets list is not null.
     */
    @Test
    public void testBeforeLoadArgsRedundant() {
        TranslationContext context = TestUtilities.getTestTranslationContext();
        UtamMethod method = new UtamMethod(METHOD_NAME, new UtamMethodAction[] {});
        method.args = new UtamArgument[0];
        UtamError e = expectThrows(UtamError.class, () -> method.getBeforeLoadMethod(context));
        assertThat(e.getMessage(), containsString(getErr(ERR_ARGS_NOT_ALLOWED)));
    }

    /**
     * The getBeforeLoadMethod should throw a proper UtamError if UtamMethodAction list has zero statements.
     */
    @Test
    public void testGetBeforeLoadMethodWithEmptyStatementListThrows() {
        TranslationContext context = TestUtilities.getTestTranslationContext();
        UtamMethod method = new UtamMethod("load", new UtamMethodAction[] {});

        UtamError e = expectThrows(UtamError.class, () -> method.getBeforeLoadMethod(context));
        assertThat(e.getMessage(), containsString(String.format(ERR_METHOD_EMPTY_STATEMENTS, "load")));
    }
}
