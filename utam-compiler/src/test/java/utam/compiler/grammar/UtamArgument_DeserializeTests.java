package utam.compiler.grammar;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.nullValue;
import static org.testng.Assert.assertThrows;
import static org.testng.Assert.expectThrows;
import static utam.compiler.grammar.TestUtilities.JACKSON_CONSTRUCTOR_ERROR;
import static utam.compiler.grammar.TestUtilities.getDeserializedObject;
import static utam.compiler.grammar.UtamArgument.ERR_ARGS_TYPE_NOT_SUPPORTED;
import static utam.compiler.grammar.UtamArgument.ERR_ARGS_WRONG_TYPE;
import static utam.compiler.grammar.UtamArgument.Processor.ERR_ARGS_DUPLICATE_NAMES;
import static utam.compiler.grammar.UtamArgument.Processor.ERR_ARGS_WRONG_COUNT;
import static utam.compiler.grammar.UtamArgument.getArgsProcessor;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
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

  /**
   * A UtamArgument object should be able to be created through deserialization with the proper
   * default values
   */
  @Test
  public void testDeserializationDefaultValues() {
    String json = "{\n" + "  \"name\" :  \"attrName\",\n" + "  \"type\" : \"string\"\n" + "}\n";
    UtamArgument argument = getDeserializedObject(json, UtamArgument.class);
    assertThat(argument, is(not(nullValue())));
  }

  /**
   * A valid UtamArgument object should be able to be created
   */
  @Test
  public void testCreation() {
    String json =
        "{"
            + "  \"name\" : \"testParameterMethod\",\n"
            + "  \"args\" : [\n"
            + "    {\n"
            + "      \"name\" :  \"attrName\",\n"
            + "      \"type\" : \"string\"\n"
            + "    },\n"
            + "    {\n"
            + "      \"name\" :  \"attrValue\",\n"
            + "      \"type\" : \"string\"\n"
            + "    }\n"
            + "  ],\n"
            + "  \"compose\" : [\n"
            + "    {\n"
            + "      \"apply\": \"getAttribute\",\n"
            + "      \"element\": \"rootElement\"\n"
            + "    }\n"
            + "  ]\n"
            + "}\n";

    List<MethodParameter> parameters = getParameters(json);
    assertThat(parameters, hasSize(2));
    assertThat(parameters.get(0).getValue(), is(equalTo("attrName")));
    assertThat(parameters.get(0).getType().getSimpleName(), is(equalTo("String")));
    assertThat(parameters.get(1).getValue(), is(equalTo("attrValue")));
    assertThat(parameters.get(1).getType().getSimpleName(), is(equalTo("String")));
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
    e =
        expectThrows(
            UtamError.class,
            () -> getParameters(json, Arrays.asList(PrimitiveType.STRING, PrimitiveType.NUMBER)));
    assertThat(
        e.getMessage(),
        containsString(String.format(ERR_ARGS_DUPLICATE_NAMES, "testParameterMethod", "attrName")));
  }

  /**
   * Creating a UtamArgument object with an invalid parameter type throws the proper exception
   */
  @Test
  public void testCreationWithInvalidParameterTypeThrows() {
    String json =
        "{"
            + "  \"name\" : \"test\",\n"
            + "  \"args\" : [\n"
            + "    {\n"
            + "      \"name\" :  \"attrName\",\n"
            + "      \"type\" : \"int\"\n"
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
        e.getMessage(), containsString(String.format(ERR_ARGS_TYPE_NOT_SUPPORTED, "test", "int")));
  }

  /**
   * The isPrimitiveType static method should return true for a primitive type
   */
  @Test
  public void testIsPrimitiveType() {
    assertThat(PrimitiveType.fromString("string"), is(equalTo(PrimitiveType.STRING)));
  }

  /**
   * The isPrimitiveType static method should return false for an unrecognized type
   */
  @Test
  public void testIsPrimitiveTypeWithInvalidValue() {
    assertThat(PrimitiveType.fromString("int") == null, is(equalTo(true)));
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
   * Creating a UtamArgument object should throw the proper exception when custom types are
   * mismatched
   */
  @Test
  public void testCreationWithMismatchedPrimitiveTypesThrows() {
    String json =
        "{"
            + "  \"name\" : \"test\",\n"
            + "  \"args\" : [\n"
            + "    {\n"
            + "      \"name\" :  \"attrName\",\n"
            + "      \"type\" : \"string\"\n"
            + "    }\n"
            + "  ],\n"
            + "  \"compose\" : [\n"
            + "    {\n"
            + "      \"apply\": \"getAttribute\",\n"
            + "      \"element\": \"rootElement\"\n"
            + "    }\n"
            + "  ]\n"
            + "}\n";

    UtamError e =
        expectThrows(
            UtamError.class,
            () -> getParameters(json, Collections.singletonList(PrimitiveType.NUMBER)));
    assertThat(
        e.getMessage(),
        containsString(
            String.format(ERR_ARGS_WRONG_TYPE, "test", "attrName", "Integer", "String")));
  }

  /**
   * A valid UtamArgument object with literal values should be able to be created
   */
  @Test
  public void testCreationWithLiterals() {
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
    assertThat(parameters.get(0).getValue(), is(equalTo("nameValue")));
    assertThat(parameters.get(0).getType().getSimpleName(), is(equalTo("String")));
    assertThat(parameters.get(1).getValue(), is(equalTo("1")));
    assertThat(parameters.get(1).getType().getSimpleName(), is(equalTo("Integer")));
    assertThat(parameters.get(2).getValue(), is(equalTo("true")));
    assertThat(parameters.get(2).getType().getSimpleName(), is(equalTo("Boolean")));
  }

  /**
   * A valid UtamArgument object with literal values should be able to be created
   */
  @Test
  public void testCreationWithLiteralsWithoutValue() {
    String json =
        "{"
            + "  \"name\" : \"testParameterMethod\",\n"
            + "  \"args\" : [\n"
            + "    {\n"
            + "      \"name\" :  \"attrName\",\n"
            + "      \"type\" : \"string\""
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
        getParameters(json, Collections.singletonList(PrimitiveType.STRING));
    assertThat(parameters, hasSize(1));
    assertThat(parameters.get(0).getValue(), is(equalTo("attrName")));
    assertThat(parameters.get(0).getType().getSimpleName(), is(equalTo("String")));
  }

  /**
   * Creating a UtamArgument object with literals and mismatched types should throw the proper
   * exception
   */
  @Test
  public void testCreationWithLiteralsWithMismatchedTypesThrows() {
    String json =
        "{"
            + "  \"name\" : \"test\",\n"
            + "  \"args\" : [\n"
            + "    {\n"
            + "      \"value\" : \"invalid string value\"\n"
            + "    }\n"
            + "  ],\n"
            + "  \"compose\" : [\n"
            + "    {\n"
            + "      \"apply\": \"getAttribute\",\n"
            + "      \"element\": \"rootElement\"\n"
            + "    }\n"
            + "  ]\n"
            + "}\n";

    UtamError e =
        expectThrows(
            UtamError.class,
            () ->
                UtamArgument.getArgsProcessor(
                    getDeserializedObject(json, UtamMethod.class).args,
                    Collections.singletonList(PrimitiveType.NUMBER),
                    "test")
                    .getOrdered());
    assertThat(
        e.getMessage(),
        containsString(String
            .format(ERR_ARGS_WRONG_TYPE, "test", "invalid string value", "Integer", "String")));
  }

  /**
   * Creating a UtamArgument object with declared primitive type but non-primitive value should
   * throw the proper exception
   */
  @Test
  public void testCreationWithLiteralsMismatchedNonPrimitiveTypeThrows() {
    String json =
        "{"
            + "  \"name\" : \"test\",\n"
            + "  \"args\" : [\n"
            + "    {\n"
            + "      \"value\" : true"
            + "    }\n"
            + "  ],\n"
            + "  \"compose\" : [\n"
            + "    {\n"
            + "      \"apply\": \"getAttribute\",\n"
            + "      \"element\": \"rootElement\"\n"
            + "    }\n"
            + "  ]\n"
            + "}\n";

    assertThrows(() -> getParameters(json, Collections.singletonList(PrimitiveType.STRING)));
  }

  /**
   * Creating a UtamArgument object with non-primitive types should be able to be created
   */
  @Test
  public void testCreationWithLiteralsNonPrimitiveTypeThrows() {
    String json =
        "{"
            + "  \"name\" : \"test\",\n"
            + "  \"args\" : [\n"
            + "    {\n"
            + "      \"name\" :  \"attrValue\",\n"
            + "      \"type\" : \"Object\"\n"
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
        containsString(String.format(ERR_ARGS_TYPE_NOT_SUPPORTED, "test", "Object")));
  }

  /**
   * A valid UtamArgument object should be able to be created
   */
  @Test
  public void testCreationAbstract() {
    String json =
        "{"
            + "  \"name\" : \"testParameterMethod\",\n"
            + "  \"args\" : [\n"
            + "    {\n"
            + "      \"name\" :  \"attrName\",\n"
            + "      \"type\" : \"string\""
            + "    }\n"
            + "  ],\n"
            + "  \"compose\" : [\n"
            + "    {\n"
            + "      \"apply\": \"getAttribute\",\n"
            + "      \"element\": \"rootElement\"\n"
            + "    }\n"
            + "  ]\n"
            + "}\n";

    List<MethodParameter> parameters = getParameters(json);
    assertThat(parameters, hasSize(1));
    assertThat(parameters.get(0).getValue(), is(equalTo("attrName")));
    assertThat(parameters.get(0).getType().getSimpleName(), is(equalTo("String")));
  }

  /**
   * A UtamArgument object should throw the appropriate exception when the argument is not an
   * object
   */
  @Test
  public void testCreationAbstractWithNonObjectArgumentThrows() {
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
    assertThat(e.getCause().getMessage(),
        containsString(String.format(JACKSON_CONSTRUCTOR_ERROR, UtamArgument.class.getName())));
  }
}
