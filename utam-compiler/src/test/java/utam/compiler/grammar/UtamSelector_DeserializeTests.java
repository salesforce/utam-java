/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import utam.compiler.helpers.PrimitiveType;
import utam.core.declarative.representation.MethodParameter;
import org.testng.annotations.Test;

import java.util.List;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;

/**
 * Provides deserialization tests for the UtamSelector class
 * @author elizaveta.ivanova
 * @since 228
 */
public class UtamSelector_DeserializeTests {

  /**
   * A UtamSelector object should be able to be created through deserialization
   */
  @Test
  public void testValidSimpleSelector() {
    String json = "{" + "  \"css\": \"simpleSelector\"" + "}";
    UtamSelector instance = TestUtilities.getDeserializedObject(json, UtamSelector.class);
    assertThat(instance.isReturnAll, is(equalTo(false)));
    UtamSelector.Context context = instance.getContext();
    assertThat(context.getLocator().getStringValue(), is(equalTo("simpleSelector")));
    assertThat(instance.args, is(nullValue()));
    assertThat(context.getParameters(), is(empty()));
  }

  @Test
  public void testDeserializationArgs() {
    String json =
        "{"
            + "  \"css\": \"stringArgSelector[%s]\","
            + "    \"args\": [ {\"name\": \"text\", \"type\":\"string\" }]"
            + "}";
    UtamSelector instance = TestUtilities.getDeserializedObject(json, UtamSelector.class);
    assertThat(instance, is(not(nullValue())));
  }

  @Test
  public void testValidSimpleListSelector() {
    String json = "{"
            + "  \"css\": \"simpleListSelector\","
            + "  \"returnAll\": true"
            + "}";
    UtamSelector node = TestUtilities.getDeserializedObject(json, UtamSelector.class);
    UtamSelector.Context context = node.getContext();
    assertThat(node.isReturnAll, is(equalTo(true)));
    assertThat(context.getLocator().getStringValue(), is(equalTo("simpleListSelector")));
    assertThat(context.getParameters(), is(empty()));
  }

  @Test
  public void testStringArgSelector() {
    String json =
        "{"
            + "  \"css\": \"stringArgSelector[%s]\","
            + "    \"args\": [ {\"name\": \"text\", \"type\":\"string\" }]"
            + "}";
    UtamSelector node = TestUtilities.getDeserializedObject(json, UtamSelector.class);
    UtamSelector.Context context = node.getContext();
    assertThat(node.isReturnAll, is(equalTo(false)));
    assertThat(context.getLocator().getStringValue(), is(equalTo("stringArgSelector[%s]")));
    assertThat(context.getParameters(), hasSize(1));
    assertThat(context.getParameters().get(0).getType(), is(equalTo(PrimitiveType.STRING)));
  }

  @Test
  public void testIntegerArgSelector() {
    String json =
        "{"
            + "  \"css\": \"integerArgSelector[%d]\","
            + "    \"args\": [ {\"name\": \"intArg\", \"type\":\"number\" }]"
            + "}";
    UtamSelector node = TestUtilities.getDeserializedObject(json, UtamSelector.class);
    UtamSelector.Context context = node.getContext();
    List<MethodParameter> parameters = context.getParameters();
    assertThat(node.isReturnAll, is(equalTo(false)));
    assertThat(context.getLocator().getStringValue(), is(equalTo("integerArgSelector[%d]")));
    assertThat(parameters, hasSize(1));
    assertThat(parameters.get(0).getType(), is(equalTo(PrimitiveType.NUMBER)));
  }
}
