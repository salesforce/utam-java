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
import utam.compiler.grammar.UtamElement.Type;

/**
 * test reads JSON file with declared frames
 *
 * @since 236
 */
public class UtamElement_FrameTests {

  @Test
  public void testFrameElementWithReturnAllSelectorThrows() {
    String json = "{\"elements\": [\n"
        + "    {\n"
        + "      \"name\": \"test\",\n"
        + "      \"selector\": {\"css\" : \"css\", \"returnAll\": true},\n"
        + "      \"type\": \"frame\""
        + "    }\n"
        + "  ]}";
    Exception e = expectCompilerError(json);
    assertThat(e.getMessage(), containsString("error 206: element \"test\": frame selector cannot have \"returnAll\" set to true"));
  }

  @Test
  public void testFrameElementWithNoSelectorThrows() {
    String json = "{\"elements\": [\n"
        + "    {\n"
        + "      \"name\": \"test\",\n"
        + "      \"type\": \"frame\""
        + "    }\n"
        + "  ]}";
    Exception e = expectCompilerError(json);
    assertThat(e.getMessage(),
        containsString("error 204: element \"test\": property \"selector\" is mandatory"));
  }

  @Test
  public void testFrameElementWithNullableThrows() {
    String json = "{\"elements\": [\n"
        + "    {\n"
        + "      \"name\": \"test\",\n"
        + "      \"selector\": {\"css\" : \"css\"},\n"
        + "      \"type\": \"frame\","
        + "      \"nullable\": true"
        + "    }\n"
        + "  ]}";
    Exception e = expectCompilerError(json);
    assertThat(e.getMessage(), containsString(Type.FRAME.getSupportedPropertiesErr("test")));
  }

  @Test
  public void testFrameElementWithElementsThrows() {
    String json = "{\"elements\": [\n"
        + "    {\n"
        + "      \"name\": \"test\",\n"
        + "      \"selector\": {\"css\" : \"css\"},\n"
        + "      \"type\": \"frame\","
        + "      \"elements\": [{ \"name\": \"nested\", \"type\": \"container\" }]"
        + "    }\n"
        + "  ]}";
    Exception e = expectCompilerError(json);
    assertThat(e.getMessage(),
        containsString(Type.FRAME.getSupportedPropertiesErr("test")));
  }

  @Test
  public void testFrameElementWithShadowThrows() {
    String json = "{\"elements\": [\n"
        + "    {\n"
        + "      \"name\": \"test\",\n"
        + "      \"selector\": {\"css\" : \"css\"},\n"
        + "      \"type\": \"frame\","
        + "      \"shadow\": { \"elements\": []}"
        + "    }\n"
        + "  ]}";
    Exception e = expectCompilerError(json);
    assertThat(e.getMessage(),
        containsString(
            "error 12: element \"test\" shadow: property \"elements\" should be a not empty array"));
  }
}
