/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.MatcherAssert.assertThat;
import static utam.compiler.grammar.DeserializerUtilities.expectCompilerError;

import org.testng.annotations.Test;

/**
 * test validation of JSON files with container elements
 *
 * @author elizaveta.ivanova
 * @since 228
 */
public class UtamElement_ContainerTests {

  @Test
  public void testDuplicatePropertyThrows() {
    String json = "{\"elements\": [\n"
        + "    {\n"
        + "      \"name\": \"test\",\n"
        + "      \"name\": \"test\",\n"
        + "      \"type\": \"container\""
        + "    }\n"
        + "  ]}";
    Exception e = expectCompilerError(json);
    assertThat(e.getMessage(),
        containsString("error UPO000: incorrect format of the page object: \n"
            + "Duplicate field 'name'"));
  }

  @Test
  public void testNotAllowedPropertyThrows() {
    String json = "{\"elements\": [\n"
        + "    {\n"
        + "      \"name\": \"test\",\n"
        + "      \"wrong\": \"test\",\n"
        + "      \"type\": \"container\""
        + "    }\n"
        + "  ]}";
    Exception e = expectCompilerError(json);
    assertThat(e.getMessage(),
        containsString("error UE000: root elements: incorrect format of elements: \n"
            + "Unrecognized field \"wrong\""));
  }
}
