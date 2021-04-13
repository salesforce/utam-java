/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import org.testng.annotations.Test;

import static utam.compiler.grammar.TestUtilities.getDeserializedObject;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;
import static org.testng.Assert.assertThrows;

/**
 * @author elizaveta.ivanova
 * @since 228
 */
public class UtamMethodAction_DeserializeTests {

    /** A UtamMethodAction object should be able to be created through deserialization */
    /** For built-in actions on basic & custom elements */
    @Test
    public void testDeserializationDefaultValues() {
        String json = "{  \"element\": \"element\",  \"apply\": \"click\" }";
        UtamMethodAction method = getDeserializedObject(json, UtamMethodAction.class);
        assertThat(method, is(not(nullValue())));
        assertThat(method.apply, is(equalTo("click")));
        assertThat(method.args, is(nullValue()));
        assertThat(method.elementName, is(equalTo("element")));
    }

    @Test
    /** For imperative extensions */
    public void testDeserializationDefaultValuesForUtilityStatement() {
        String json = "{"
                + " \"applyExternal\": {"
                + "      \"type\": \"utam-test/utils/test/testUtilClass\","
                + "      \"invoke\": \"testUtilityMethod\""
                + "  }"
                + "}";
        UtamMethodAction method = getDeserializedObject(json, UtamMethodAction.class);
        assertThat(method, is(not(nullValue())));
        assertThat(method.apply, is(nullValue()));
        assertThat(method.args, is(nullValue()));
        assertThat(method.elementName, is(nullValue()));
        assertThat(method.applyExternal, is(not(nullValue())));
        UtamUtilityMethodAction utility = method.applyExternal;
        assertThat(utility.externalClassPath, is(equalTo("utam-test/utils/test/testUtilClass")));
        assertThat(utility.methodName, is(equalTo("testUtilityMethod")));
        assertThat(utility.args, is(nullValue()));
    }
}
