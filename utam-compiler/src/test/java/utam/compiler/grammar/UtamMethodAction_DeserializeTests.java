/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import utam.core.framework.consumer.UtamError;
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

    // does not really need to be tested as it's part of Jackson library
    @Test
    public void testMandatoryFields() {
        String json1 = "{" + "  \"apply\": \"click\"" + "}";
        assertThrows(
                UtamError.class,
                () -> getDeserializedObject(json1, UtamMethodAction.class));
        String json2 = "{" + "  \"element\": \"element\"," + "}";
        assertThrows(
                UtamError.class,
                () -> getDeserializedObject(json2, UtamMethodAction.class));
        String json3 = "{"
                + "  \"compose\": ["
                + "    {"
                + "      \"element\": \"element\","
                + "      \"apply\": \"click\""
                + "    }"
                + "  ]"
                + "}";
        assertThrows(
                UtamError.class,
                () -> getDeserializedObject(json3, UtamMethodAction.class));
    }

    /** A UtamMethodAction object should be able to be created through deserialization */
    @Test
    public void testDeserializationDefaultValues() {
        String json = "{  \"element\": \"element\",  \"apply\": \"click\" }";
        UtamMethodAction method = getDeserializedObject(json, UtamMethodAction.class);
        assertThat(method, is(not(nullValue())));
        assertThat(method.apply, is(equalTo("click")));
        assertThat(method.args, is(nullValue()));
        assertThat(method.elementName, is(equalTo("element")));
    }
}
