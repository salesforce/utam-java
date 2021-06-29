/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.representation;

import org.testng.annotations.Test;
import utam.compiler.helpers.MethodContext;

import java.util.Collections;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.collection.IsCollectionWithSize.hasSize;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static utam.compiler.helpers.MethodContext.BEFORE_LOAD_METHOD_MANE;
import static utam.compiler.helpers.ParameterUtils.EMPTY_PARAMETERS;
import static utam.compiler.translator.TranslationUtilities.EMPTY_COMMENTS;
import static utam.compiler.helpers.TypeUtilities.VOID;

/**
 * Provides tests for BeforeLoadMethod class.
 *
 * @author igor.khorev
 */
public class BeforeLoadMethodTests {

     @Test
    public void testBeforeLoadMethodCreation() {
        PageObjectValidationTestHelper.MethodInfo info =
                new PageObjectValidationTestHelper.MethodInfo(BEFORE_LOAD_METHOD_MANE, VOID.getSimpleName());
        info.addCodeLine("this.getFakeElement().isPresent()");
        ComposeMethodStatement methodAction = mock(ComposeMethodStatement.class);
        when(methodAction.getCodeLines())
                .thenReturn(Collections.singletonList("this.getFakeElement().isPresent()"));
        when(methodAction.getReturnType()).thenReturn(VOID);
        when(methodAction.getImports()).thenReturn(Collections.singletonList(VOID));

        BeforeLoadMethod method = new BeforeLoadMethod(
            new MethodContext(BEFORE_LOAD_METHOD_MANE, VOID, false),
            Collections.singletonList(methodAction),
            EMPTY_PARAMETERS,
            EMPTY_COMMENTS);
        PageObjectValidationTestHelper.validateMethod(method, info);
        assertThat(method.getClassImports(), hasSize(0));
    }
}
