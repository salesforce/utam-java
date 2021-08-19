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
import static utam.compiler.grammar.TestUtilities.JSON_MAPPING_ERROR;
import static utam.compiler.grammar.TestUtilities.getDeserializedObject;
import static utam.compiler.grammar.UtamArgumentDeserializer.ERR_ARGS_LITERAL_TYPE_NOT_SUPPORTED;
import static utam.compiler.grammar.UtamArgumentDeserializer.ERR_ARGS_NAME_MANDATORY;
import static utam.compiler.grammar.UtamArgumentDeserializer.ERR_ARGS_TYPE_MANDATORY;
import static utam.compiler.grammar.UtamArgumentDeserializer.ERR_ARGS_UNKNOWN_LITERAL_OBJECT;
import static utam.compiler.grammar.UtamArgumentDeserializer.ERR_PREDICATE_MANDATORY;
import static utam.compiler.grammar.UtamArgumentDeserializer.ERR_PREDICATE_REDUNDANT;
import static utam.compiler.grammar.UtamArgumentDeserializer.getRedundantForValueErr;
import static utam.compiler.grammar.UtamArgumentDeserializer.getUnsupportedTypeErr;

import org.testng.annotations.Test;
import utam.compiler.grammar.UtamArgumentDeserializer.ElementReference;
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

    json = "{  \"name\" :  \"argName\",  \"type\" : \"function\", \"predicate\" : [] }";
    deserialize(json);

    json = "{  \"name\" :  \"argName\",  \"type\" : \"element\" }";
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

    json = "{  \"value\" : { \"element\" : \"elementRef\" } }";
    UtamArgument argument = deserialize(json);
    assertThat(argument.value, is(instanceOf(ElementReference.class)));

    json = "{  \"value\" : { \"css\" : \"css\" } }";
    argument = deserialize(json);
    assertThat(argument.value, is(instanceOf(UtamSelector.class)));
  }

  @Test
  public void testSelectorArgByValueWithArgs() {
    String json = "{  \"value\" : { \"css\" : \"css[%s]\" , \"args\" : [{ \"value\" : \"1\"}] } }";
    UtamArgument argument = deserialize(json);
    assertThat(argument.value, is(instanceOf(UtamSelector.class)));
  }

  @Test
  public void testNameMissingThrows() {
    String json = "{ \"type\" : \"string\" }";
    UtamError e = expectThrows(UtamError.class, () -> deserialize(json));
    assertThat(e.getMessage(), containsString(ERR_ARGS_NAME_MANDATORY));
  }

  @Test
  public void testTypeMissingThrows() {
    String json = "{ \"name\" : \"name\" }";
    UtamError e = expectThrows(UtamError.class, () -> deserialize(json));
    assertThat(e.getMessage(), containsString(ERR_ARGS_TYPE_MANDATORY));
  }

  @Test
  public void testNameMissingForFunctionAllowed() {
    String json = "{ \"type\" : \"function\", \"predicate\" : [] }";
    deserialize(json);
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
    assertThat(e.getMessage(),
        containsString(String.format(ERR_ARGS_LITERAL_TYPE_NOT_SUPPORTED, "1.024")));
  }

  @Test
  public void testRedundantValueWithTypeThrows() {
    String json = "{ \"value\" : true, \"type\" : \"string\" }";
    UtamError e = expectThrows(UtamError.class, () -> deserialize(json));
    assertThat(e.getMessage(), containsString(getRedundantForValueErr("true")));
  }

  @Test
  public void testRedundantValueWithNameThrows() {
    String json = "{ \"value\" : true, \"name\" : \"name\" }";
    UtamError e = expectThrows(UtamError.class, () -> deserialize(json));
    assertThat(e.getMessage(), containsString(getRedundantForValueErr("true")));
  }

  @Test
  public void testRedundantValueWithPredicateThrows() {
    String json = "{ \"value\" : true, \"predicate\" : [] }";
    UtamError e = expectThrows(UtamError.class, () -> deserialize(json));
    assertThat(e.getMessage(), containsString(getRedundantForValueErr("true")));
  }

  @Test
  public void testRedundantNotLiteralPredicate() {
    String json = "{  \"name\" :  \"name\",  \"type\" : \"string\", \"predicate\" : [] }";
    UtamError e = expectThrows(UtamError.class, () -> deserialize(json));
    assertThat(e.getMessage(), containsString(ERR_PREDICATE_REDUNDANT));
  }

  @Test
  public void testMissingPredicate() {
    String json = "{  \"name\" :  \"name\",  \"type\" : \"function\" }";
    UtamError e = expectThrows(UtamError.class, () -> deserialize(json));
    assertThat(e.getMessage(), containsString(ERR_PREDICATE_MANDATORY));
  }

  @Test
  public void testElementArgByValueWithErrorThrows() {
    String json = "{  \"value\" : { \"element\" : \"elementRef\", \"extra\" : true } }";
    UtamError e = expectThrows(UtamError.class, () -> deserialize(json));
    assertThat(e.getMessage(), containsString(JSON_MAPPING_ERROR));
  }

  @Test
  public void testArgByValueUnknownObjectThrows() {
    String json = "{  \"value\" : { \"extra\" : true } }";
    UtamError e = expectThrows(UtamError.class, () -> deserialize(json));
    assertThat(e.getMessage(), containsString(ERR_ARGS_UNKNOWN_LITERAL_OBJECT));
  }

  @Test
  public void testIncorrectPredicateThrows() {
    String json = "{ \"type\" : \"function\", \"predicate\" : [{ \"error\": true }] }";
    UtamError e = expectThrows(UtamError.class, () -> deserialize(json));
    assertThat(e.getMessage(), containsString(JSON_MAPPING_ERROR));
  }
}
