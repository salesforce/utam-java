package utam.compiler.grammar;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.instanceOf;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.nullValue;
import static org.testng.Assert.assertThrows;
import static org.testng.Assert.expectThrows;
import static utam.compiler.grammar.TestUtilities.getDeserializedObject;
import static utam.compiler.grammar.UtamArgument.Processor.ERR_ARGS_DUPLICATE_NAMES;
import static utam.compiler.grammar.UtamArgument.Processor.ERR_ARGS_WRONG_COUNT;
import static utam.compiler.grammar.UtamArgument.getArgsProcessor;
import static utam.compiler.helpers.TypeUtilities.FUNCTION;
import static utam.compiler.helpers.TypeUtilities.SELECTOR;

import com.fasterxml.jackson.databind.JsonMappingException;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.testng.annotations.Test;
import utam.compiler.helpers.PrimitiveType;
import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.TypeProvider;
import utam.core.framework.consumer.UtamError;

/**
 * Provides deserialization tests for the UtamArgument class
 *
 * @author james.evans
 */
public class UtamArgument_DeserializeTests {

  private static List<MethodParameter> getParameters(String json) {
    UtamMethod utamMethod = getDeserializedObject(json, UtamMethod.class);
    return getArgsProcessor(utamMethod.args, utamMethod.name).getOrdered();
  }

  private static List<MethodParameter> getParameters(String json, List<TypeProvider> expected) {
    UtamMethod utamMethod = getDeserializedObject(json, UtamMethod.class);
    return getArgsProcessor(utamMethod.args, expected, utamMethod.name)
        .getOrdered();
  }

  @Test
  public void testDeserializationNameType() {
    String json = "{  \"name\" :  \"attrName\",  \"type\" : \"string\" }";
    UtamArgument argument = getDeserializedObject(json, UtamArgument.class);
    assertThat(argument, is(not(nullValue())));
  }

  @Test
  public void testUnsupportedTypeThrows() {
    String json = "{ \"type\" : \"xxx\" }";
    assertThrows(() -> getDeserializedObject(json, UtamArgument.class));
  }

  @Test
  public void testUnsupportedValueThrows() {
    String json = "{ \"value\": 1.024 }";
    assertThrows(() -> getDeserializedObject(json, UtamArgument.class));
  }

  @Test
  public void testRedundantValueWithTypeThrows() {
    String json = "{ \"value\" : true, \"type\" : \"string\" }";
    assertThrows(() -> getDeserializedObject(json, UtamArgument.class));
  }

  @Test
  public void testRedundantValueWithNameThrows() {
    String json = "{ \"value\" : true, \"name\" : \"name\" }";
    assertThrows(() -> getDeserializedObject(json, UtamArgument.class));
  }

  @Test
  public void testRedundantValueWithPredicateThrows() {
    String json = "{ \"value\" : true, \"predicate\" : [] }";
    assertThrows(() -> getDeserializedObject(json, UtamArgument.class));
  }

  /**
   * Creating a UtamArgument object with duplicate parameter names throws the proper exception
   */
  @Test
  public void testCreationWithDuplicateNamesThrows() {
    String json =
        "{"
            + "  \"name\" : \"testParameterMethod\",\n"
            + "  \"args\" : [\n"
            + "    {\n"
            + "      \"name\" :  \"attrName\",\n"
            + "      \"type\" : \"string\"\n"
            + "    },\n"
            + "    {\n"
            + "      \"name\" :  \"attrName\",\n"
            + "      \"type\" : \"number\"\n"
            + "    }\n"
            + "  ],\n"
            + "  \"compose\" : [\n"
            + "    {\n"
            + "      \"apply\": \"getAttribute\",\n"
            + "      \"element\": \"rootElement\"\n"
            + "    }\n"
            + "  ]\n"
            + "}\n";

    UtamError e = expectThrows(UtamError.class, () -> getParameters(json));
    assertThat(
        e.getMessage(),
        containsString(String.format(ERR_ARGS_DUPLICATE_NAMES, "testParameterMethod", "attrName")));
  }

  @Test
  public void testExpectedCountErr() {
    String json = "{ \"name\" : \"test\" }";
    UtamError e =
        expectThrows(
            UtamError.class,
            () -> getParameters(json, Collections.singletonList(PrimitiveType.NUMBER)));
    assertThat(
        e.getMessage(), containsString(String.format(ERR_ARGS_WRONG_COUNT, "test", 1, 0)));
  }

  /**
   * A valid UtamArgument object with literal values should be able to be created
   */
  @Test
  public void testPrimitiveValuesWithLiterals() {
    String json =
        "{"
            + "  \"name\" : \"testParameterMethod\",\n"
            + "  \"args\" : [\n"
            + "    {\n"
            + "      \"value\" : \"nameValue\"\n"
            + "    },\n"
            + "    {\n"
            + "      \"value\" : 1\n"
            + "    },\n"
            + "    {\n"
            + "      \"value\" : true\n"
            + "    }\n"
            + "  ],\n"
            + "  \"compose\" : [\n"
            + "    {\n"
            + "      \"apply\": \"getAttribute\",\n"
            + "      \"element\": \"rootElement\"\n"
            + "    }\n"
            + "  ]\n"
            + "}\n";

    List<MethodParameter> parameters =
        UtamArgument.getArgsProcessor(
            getDeserializedObject(json, UtamMethod.class).args,
            Arrays.asList(PrimitiveType.STRING, PrimitiveType.NUMBER, PrimitiveType.BOOLEAN),
            "test")
            .getOrdered();
    assertThat(parameters, hasSize(3));
    assertThat(parameters.get(0).getValue(), is(equalTo("\"nameValue\"")));
    assertThat(parameters.get(0).getType().getSimpleName(), is(equalTo("String")));
    assertThat(parameters.get(1).getValue(), is(equalTo("1")));
    assertThat(parameters.get(1).getType().getSimpleName(), is(equalTo("Integer")));
    assertThat(parameters.get(2).getValue(), is(equalTo("true")));
    assertThat(parameters.get(2).getType().getSimpleName(), is(equalTo("Boolean")));
  }

  @Test
  public void testPrimitiveTypesWithoutValue() {
    String json =
        "{"
            + "  \"name\" : \"testParameterMethod\",\n"
            + "  \"args\" : [\n"
            + "    {\n"
            + "      \"name\" :  \"string\",\n"
            + "      \"type\" : \"string\""
            + "    },\n"
            + "    {\n"
            + "      \"name\" :  \"number\",\n"
            + "      \"type\" : \"number\""
            + "    },\n"
            + "    {\n"
            + "      \"name\" :  \"boolean\",\n"
            + "      \"type\" : \"boolean\""
            + "    }\n"
            + "  ],\n"
            + "  \"compose\" : [\n"
            + "    {\n"
            + "      \"apply\": \"getAttribute\",\n"
            + "      \"element\": \"rootElement\"\n"
            + "    }\n"
            + "  ]\n"
            + "}\n";

    List<MethodParameter> parameters =
        getParameters(json,
            Stream.of(PrimitiveType.STRING, PrimitiveType.NUMBER, PrimitiveType.BOOLEAN).collect(
                Collectors.toList()));
    assertThat(parameters, hasSize(3));
    assertThat(parameters.get(0).getValue(), is(equalTo("string")));
    assertThat(parameters.get(0).getType().getSimpleName(), is(equalTo("String")));
    assertThat(parameters.get(1).getValue(), is(equalTo("number")));
    assertThat(parameters.get(1).getType().getSimpleName(), is(equalTo("Integer")));
    assertThat(parameters.get(2).getValue(), is(equalTo("boolean")));
    assertThat(parameters.get(2).getType().getSimpleName(), is(equalTo("Boolean")));
  }

  @Test
  public void testCreationWithMismatchedTypeThrows() {
    String json = "{  \"name\" :  \"name\",  \"type\" : \"locator\" }";
    UtamArgument byType = getDeserializedObject(json, UtamArgument.class);
    assertThrows(() -> byType.getParameterOrValue("test", PrimitiveType.STRING));

    json = "{  \"value\" :  true }";
    UtamArgument byValue = getDeserializedObject(json, UtamArgument.class);
    assertThrows(() -> byValue.getParameterOrValue("test", PrimitiveType.STRING));
  }

  /**
   * A UtamArgument object should throw the appropriate exception when the argument is not an
   * object
   */
  @Test
  public void testArgsStringArrayThrows() {
    String json =
        "{"
            + "  \"name\" : \"testParameterMethod\",\n"
            + "  \"args\" : [\n"
            + "    \"attrName\"\n"
            + "  ],\n"
            + "  \"compose\" : [\n"
            + "    {\n"
            + "      \"apply\": \"getAttribute\",\n"
            + "      \"element\": \"rootElement\"\n"
            + "    }\n"
            + "  ]\n"
            + "}\n";
    UtamError e = expectThrows(UtamError.class, () -> getParameters(json));
    assertThat(e.getCause(), is(instanceOf(JsonMappingException.class)));
  }

  @Test
  public void testSelectorArgByTypeOrValue() {
    String byType = "{  \"name\" :  \"name\",  \"type\" : \"locator\" }";
    UtamArgument argument = getDeserializedObject(byType, UtamArgument.class);
    argument.getParameterOrValue("test", SELECTOR);
    String byValue = "{  \"value\" : { \"css\" : \".css\" } }";
    argument = getDeserializedObject(byValue, UtamArgument.class);
    argument.getParameterOrValue("test", SELECTOR);
  }

  @Test
  public void testFunctionArgByType() {
    String byType = "{  \"name\" :  \"name\",  \"type\" : \"function\" }";
    UtamArgument argument = getDeserializedObject(byType, UtamArgument.class);
    assertThrows(() -> argument.getParameterOrValue("test", SELECTOR));
    assertThat(argument.getParameterOrValue("test", FUNCTION), is(nullValue()));
  }
}
