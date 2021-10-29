/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.instanceOf;
import static org.hamcrest.Matchers.is;
import static org.testng.Assert.expectThrows;
import static utam.compiler.grammar.TestUtilities.getDeserializedObject;
import static utam.compiler.grammar.UtamArgument.UtamArgumentLiteralPrimitive.ERR_ARGS_UNKNOWN;
import static utam.compiler.grammar.UtamArgumentDeserializer.getUnsupportedTypeErr;

import java.util.HashMap;
import java.util.Map;
import java.util.stream.Collectors;
import org.testng.annotations.Test;
import utam.compiler.grammar.UtamArgument.UtamArgumentLiteralPageObjectType;
import utam.compiler.grammar.UtamArgument.UtamArgumentLiteralSelector;
import utam.core.framework.consumer.UtamError;

/**
 * Provides deserialization tests for the UtamArgument class
 *
 * @author james.evans
 */
public class UtamArgumentDeserializerTests {

  private static UtamArgument deserialize(String json) {
    return getDeserializedObject(json, UtamArgument.class);
  }

  @Test
  public void testDeserializationByNameType() {
    String json = "{  \"name\" :  \"name\",  \"type\" : \"string\" }";
    deserialize(json);

    json = "{  \"name\" :  \"name\",  \"type\" : \"number\" }";
    deserialize(json);

    json = "{  \"name\" :  \"name\",  \"type\" : \"boolean\" }";
    deserialize(json);

    json = "{  \"name\" :  \"name\",  \"type\" : \"locator\" }";
    deserialize(json);

  }

  @Test
  public void testDeserializationByValue() {
    String json = "{  \"value\" :  \"string\" }";
    deserialize(json);

    json = "{  \"value\" :  1 }";
    deserialize(json);

    json = "{  \"value\": true }";
    deserialize(json);

    json = "{  \"value\" : \"my/po/type\", \"type\" : \"pageObject\" }";
    UtamArgument argument = deserialize(json);
    assertThat(argument, is(instanceOf(UtamArgumentLiteralPageObjectType.class)));

    json = "{  \"value\" : { \"css\" : \"css\" }, \"type\" : \"locator\" }";
    argument = deserialize(json);
    assertThat(argument, is(instanceOf(UtamArgumentLiteralSelector.class)));

    json = "{  \"value\" :  \"argName\",  \"type\" : \"elementReference\" }";
    deserialize(json);
  }

  @Test
  public void testSelectorArgByValueWithArgs() {
    String json = "{  \"type\" : \"locator\", \"value\" : { \"css\" : \"css[%s]\" , \"args\" : [{ \"value\" : \"1\"}] } }";
    UtamArgument argument = deserialize(json);
    assertThat(argument.value, is(instanceOf(UtamSelector.class)));
  }

  @Test
  public void testNameMissingThrows() {
    String json = "{ \"type\" : \"string\" }";
    UtamError e = expectThrows(UtamError.class, () -> deserialize(json));
    assertThat(e.getMessage(),
        containsString("Missing required creator property 'name'"));
  }

  @Test
  public void testTypeMissingThrows() {
    String json = "{ \"name\" : \"name\" }";
    UtamError e = expectThrows(UtamError.class, () -> deserialize(json));
    assertThat(e.getMessage(), containsString("Missing required creator property 'type'"));
  }

  @Test
  public void testUnsupportedTypeThrows() {
    String json = "{ \"name\" :  \"name\",  \"type\" : \"xxx\" }";
    UtamError e = expectThrows(UtamError.class, () -> deserialize(json));
    assertThat(e.getMessage(), containsString(getUnsupportedTypeErr("xxx")));
  }

  @Test
  public void testUnsupportedValueThrows() {
    String json = "{ \"value\": 1.024 }";
    UtamError e = expectThrows(UtamError.class, () -> deserialize(json));
    assertThat(e.getCause().getMessage(), containsString(ERR_ARGS_UNKNOWN));
  }

  @Test
  public void testRedundantValueWithTypeThrows() {
    String json = "{ \"value\" : true, \"type\" : \"string\" }";
    UtamError e = expectThrows(UtamError.class, () -> deserialize(json));
    assertThat(e.getMessage(), containsString("Unrecognized field \"type\""));
  }

  @Test
  public void testRedundantValueWithNameThrows() {
    String json = "{ \"value\" : true, \"name\" : \"name\" }";
    UtamError e = expectThrows(UtamError.class, () -> deserialize(json));
    assertThat(e.getMessage(), containsString("Unrecognized field \"name\""));
  }

  @Test
  public void testMissingPredicate() {
    String json = "{  \"name\" :  \"name\",  \"type\" : \"function\" }";
    UtamError e = expectThrows(UtamError.class, () -> deserialize(json));
    assertThat(e.getMessage(),
        containsString("Missing required creator property 'predicate'"));
  }

  @Test
  public void testArgByValueUnknownObjectThrows() {
    String json = "{  \"value\" : { \"extra\" : true } }";
    UtamError e = expectThrows(UtamError.class, () -> deserialize(json));
    assertThat(e.getMessage(), containsString(ERR_ARGS_UNKNOWN));
  }

  @Test
  public void testElementReferenceRedundantPredicate() {
    JsonStringBuilder helper = new JsonStringBuilder().setType("elementReference")
        .setValue("value", "\"element\"").setValue("predicate", "[]");
    String json = helper.asString();
    UtamError e = expectThrows(UtamError.class, () -> deserialize(json));
    assertThat(e.getMessage(), containsString("Unrecognized field \"predicate\""));
  }

  @Test
  public void testElementReferenceNameMandatory() {
    JsonStringBuilder helper = new JsonStringBuilder().setType("elementReference");
    String json = helper.asString();
    UtamError e = expectThrows(UtamError.class, () -> deserialize(json));
    assertThat(e.getMessage(),
        containsString("Missing required creator property 'value'"));
  }

  @Test
  public void testUnsupportedProperty() {
    JsonStringBuilder helper = new JsonStringBuilder().setValue("error", "text");
    String json = helper.asString();
    UtamError e = expectThrows(UtamError.class, () -> deserialize(json));
    assertThat(e.getCause().getMessage(),
        containsString("Unrecognized token 'text'"));
  }

  private static class JsonStringBuilder {

    private final Map<String, String> keys = new HashMap<>();

    private JsonStringBuilder() {
      keys.put("\"name\"", null);
      keys.put("\"type\"", null);
    }

    JsonStringBuilder setType(String value) {
      setValue("type", String.format("\"%s\"", value));
      return this;
    }

    JsonStringBuilder setValue(String key, String value) {
      keys.put(String.format("\"%s\"", key), value);
      return this;
    }

    String asString() {
      String content = keys
          .entrySet()
          .stream()
          .filter(entry -> entry.getValue() != null)
          .map(entry -> String.format("%s : %s", entry.getKey(), entry.getValue()))
          .collect(Collectors.joining(", "));
      return String.format("{ %s }", content);
    }
  }
}
