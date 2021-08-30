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
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.instanceOf;
import static org.hamcrest.Matchers.is;
import static org.testng.Assert.expectThrows;
import static utam.compiler.grammar.DeserializerUtilities.getDeserializedObjects;
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
import static utam.compiler.helpers.TypeUtilities.BASIC_ELEMENT;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import org.testng.annotations.Test;
import utam.compiler.grammar.UtamArgument.ArgsProcessor;
import utam.compiler.grammar.UtamArgument.ArgsProcessorWithExpectedTypes;
import utam.compiler.grammar.UtamArgumentDeserializer.PageObjectType;
import utam.compiler.helpers.ElementContext;
import utam.compiler.helpers.ElementContext.Basic;
import utam.compiler.helpers.ParameterUtils.Regular;
import utam.compiler.helpers.PrimitiveType;
import utam.compiler.helpers.TranslationContext;
import utam.compiler.representation.ElementMethod.Single;
import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.PageObjectMethod;
import utam.core.declarative.representation.TypeProvider;
import utam.core.framework.consumer.UtamError;

/**
 * Provides deserialization tests for the UtamArgument class
 *
 * @author james.evans
 */
public class UtamArgumentDeserializerTests {

  private final TranslationContext containers = new DeserializerUtilities()
      .getContext("element/testContainer");
  private final List<UtamArgument> elementReferences = getDeserializedObjects(UtamArgument.class, "element/testElementReference");

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

    json = "{  \"name\" :  \"argName\",  \"type\" : \"elementReference\" }";
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

    json = "{  \"value\" : { \"type\" : \"my/po/type\" } }";
    UtamArgument argument = deserialize(json);
    assertThat(argument.value, is(instanceOf(PageObjectType.class)));

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

  @Test
  public void testElementReferencePublicNoArgs() {
    ArgsProcessor argsProcessor = new ArgsProcessor(containers, "test");
    ElementContext elementContext = new Basic("testNoArgs");
    containers.setElement(elementContext);
    PageObjectMethod getter = new Single(elementContext, true);
    elementContext.setElementMethod(getter);
    UtamArgument argument = elementReferences.get(0);
    MethodParameter parameter = argument.asParameter(containers);
    assertThat(parameter.isLiteral(), is(true));
    assertThat(parameter.getValue(), is(equalTo("this.getTestNoArgs()")));
    assertThat(parameter.getType().isSameType(BASIC_ELEMENT), is(true));
    argsProcessor.getParameters(new UtamArgument[]{argument});
  }

  @Test
  public void testElementReferencePrivateWithArgsAndExpectedTypes() {
    List<TypeProvider> expectedTypes = Collections.singletonList(BASIC_ELEMENT);
    List<MethodParameter> selectorParameters = Collections
        .singletonList(new Regular("par", PrimitiveType.STRING));
    ArgsProcessor argsProcessor = new ArgsProcessorWithExpectedTypes(containers, "test",
        expectedTypes);
    ElementContext elementContext = new Basic("testWithArgs", selectorParameters);
    containers.setElement(elementContext);
    PageObjectMethod getter = new Single(elementContext, false);
    elementContext.setElementMethod(getter);
    UtamArgument argument = elementReferences.get(1);
    MethodParameter parameter = argument.asParameter(containers);
    assertThat(parameter.isLiteral(), is(true));
    assertThat(parameter.getValue(),
        is(equalTo("this.getTestWithArgsElement(\"elementArgValue\")")));
    assertThat(parameter.getType().isSameType(BASIC_ELEMENT), is(true));
    argsProcessor.getParameters(new UtamArgument[]{argument});
  }

  @Test
  public void testElementReferenceRedundantPredicate() {
    JsonStringBuilder helper = new JsonStringBuilder().setType("elementReference").setName("name").setValue("predicate", "[]");
    String json = helper.asString();
    UtamError e = expectThrows(UtamError.class, () -> deserialize(json));
    assertThat(e.getMessage(), is(equalTo(ERR_PREDICATE_REDUNDANT)));
  }

  @Test
  public void testElementReferenceNameMandatory() {
    JsonStringBuilder helper = new JsonStringBuilder().setType("elementReference");
    String json = helper.asString();
    UtamError e = expectThrows(UtamError.class, () -> deserialize(json));
    assertThat(e.getMessage(), is(equalTo(ERR_ARGS_NAME_MANDATORY)));
  }

  @Test
  public void testOnlyLiteralsAllowedInsideReference() {
    UtamError e = expectThrows(UtamError.class, () ->
        getDeserializedObjects(UtamArgument.class, "element/elementReferenceNonLiteralArgsThrows"));
    assertThat(e.getCause().getMessage(), containsString(JSON_MAPPING_ERROR));
  }

  private static class JsonStringBuilder {

    private final Map<String, String> keys = new HashMap<>();

    private JsonStringBuilder() {
      keys.put("\"name\"", null);
      keys.put("\"type\"", null);
    }

    JsonStringBuilder setName(String value) {
      setValue("name", String.format("\"%s\"", value));
      return this;
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
