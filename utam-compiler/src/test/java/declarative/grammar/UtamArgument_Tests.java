package declarative.grammar;

import declarative.helpers.PrimitiveType;
import declarative.helpers.TypeUtilities;
import declarative.representation.MethodParameter;
import declarative.representation.TypeProvider;
import framework.consumer.UtamError;
import org.testng.annotations.Test;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static declarative.grammar.UtamArgument.*;
import static declarative.grammar.UtamArgument.Processor.ERR_ARGS_DUPLICATE_NAMES;
import static declarative.helpers.ParameterUtils.EMPTY_PARAMETERS;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;
import static org.testng.Assert.expectThrows;

/**
 * Provides tests for the ArgumentsProcessor type
 *
 * @author james.evans
 */
public class UtamArgument_Tests {

  private static final String NODE_NAME = "test";

  private static void testRedundancy(String name, String type) {
    UtamError e =
        expectThrows(
            UtamError.class,
            () ->
                literalParameters(
                        new UtamArgument[] {new UtamArgument("value", name, type)},
                        Collections.singletonList(new TypeUtilities.FromString("String", "String")),
                        NODE_NAME)
                    .getOrdered());
    assertThat(e.getMessage(), containsString(String.format(ERR_LITERAL_REDUNDANT, NODE_NAME)));
  }

  @Test
  public void testRedundancy() {
    testRedundancy("argName", "string");
    testRedundancy(null, "string");
    testRedundancy("argName", null);
  }

  /** The getParameters static method should return valid values */
  @Test
  public void testGetParameters() {
    List<MethodParameter> parameters =
        unknownTypesParameters(
                new UtamArgument[] {
                  new UtamArgument("name1", "string"), new UtamArgument("name2", "string")
                },
                NODE_NAME)
            .getOrdered();
    assertThat(parameters, hasSize(2));
    assertThat(parameters.get(0).getValue(), is(equalTo("name1")));
    assertThat(parameters.get(0).getType().getSimpleName(), is(equalTo("String")));
    assertThat(parameters.get(1).getValue(), is(equalTo("name2")));
    assertThat(parameters.get(1).getType().getSimpleName(), is(equalTo("String")));
  }

  /**
   * The getParameters static method with duplicate argument names should throw the proper exception
   */
  @Test
  public void testGetParametersWithDuplicateNamesThrows() {
    final String ARG_NAME = "name";
    UtamError e =
        expectThrows(
            UtamError.class,
            () ->
                unknownTypesParameters(
                        new UtamArgument[] {
                          new UtamArgument(ARG_NAME, "string"), new UtamArgument(ARG_NAME, "number")
                        },
                        NODE_NAME)
                    .getOrdered());
    assertThat(
        e.getMessage(),
        containsString(String.format(ERR_ARGS_DUPLICATE_NAMES, NODE_NAME, ARG_NAME)));
  }

  /**
   * The getParameters static method should throw the proper exception when the duplicate argument
   * names are specified
   */
  @Test
  public void testDuplicateNamesErr() {
    UtamError e =
        expectThrows(
            UtamError.class,
            () ->
                nonLiteralParameters(
                        new UtamArgument[] {
                          new UtamArgument("name", "string"), new UtamArgument("name", "string")
                        },
                        Arrays.asList(PrimitiveType.STRING, PrimitiveType.STRING),
                        NODE_NAME)
                    .getOrdered());
    assertThat(
        e.getMessage(), containsString(String.format(ERR_ARGS_DUPLICATE_NAMES, NODE_NAME, "name")));
  }

  @Test
  public void duplicateLiterals() {
    List<MethodParameter> parameters =
        literalParameters(
                new UtamArgument[] {
                  new UtamArgument(true),
                  new UtamArgument(true),
                  new UtamArgument("true"),
                  new UtamArgument("true")
                },
                Stream.of(
                        PrimitiveType.BOOLEAN,
                        PrimitiveType.BOOLEAN,
                        PrimitiveType.STRING,
                        PrimitiveType.STRING)
                    .collect(Collectors.toList()),
                NODE_NAME)
            .getOrdered();
    assertThat(parameters, hasSize(4));
    assertThat(parameters.get(0).isLiteral(), is(true));
    assertThat(parameters.get(0).getValue(), is(equalTo("true")));
    assertThat(parameters.get(0).getType(), is(equalTo(PrimitiveType.BOOLEAN)));
    assertThat(parameters.get(3).getValue(), is(equalTo("true")));
    assertThat(parameters.get(3).getType(), is(equalTo(PrimitiveType.STRING)));
  }

  /**
   * The getParameters static method with an invalid argument type should throw the proper exception
   */
  @Test
  public void testGetParametersWithInvalidParameterTypeThrows() {
    UtamError e =
        expectThrows(
            UtamError.class,
            () ->
                unknownTypesParameters(
                        new UtamArgument[] {new UtamArgument("name", "int")}, NODE_NAME)
                    .getOrdered());
    assertThat(
        e.getMessage(),
        containsString(String.format(ERR_ARGS_TYPE_NOT_SUPPORTED, NODE_NAME, "int")));
  }

  /** If an argument has a value of non-primitive type, the proper exception should be thrown */
  @Test
  public void testInvalidLiteralParameterValueTypeThrows() {
    Object object = new Object();
    UtamArgument arg = new UtamArgument(object, null, null);
    UtamError e =
        expectThrows(
            UtamError.class,
            () -> literalParameters(new UtamArgument[] {arg}, null, NODE_NAME).getOrdered());
    assertThat(
        e.getMessage(),
        containsString(
            String.format(ERR_ARGS_TYPE_NOT_SUPPORTED, NODE_NAME, object.getClass().getName())));
  }

  /**
   * The getParameters static method should throw the proper exception when the number of parameters
   * does not match
   */
  @Test
  public void testExpectedCountErr() {
    UtamError e =
        expectThrows(
            UtamError.class,
            () ->
                nonLiteralParameters(
                        new UtamArgument[] {},
                        Collections.singletonList(PrimitiveType.NUMBER),
                        NODE_NAME)
                    .getOrdered());
    assertThat(
        e.getMessage(), containsString(String.format(ERR_ARGS_NUMBER_MISMATCH, NODE_NAME, 1, 0)));
  }

  /** The getParameters static method with a custom argument type should return the proper value */
  @Test
  public void testGetParametersWithCustomType() {
    List<MethodParameter> parameters =
        UtamArgument.nonLiteralParameters(
                new UtamArgument[] {new UtamArgument("attrName", "CustomType")},
                Collections.singletonList(new TypeUtilities.FromString("CustomType", "")),
                NODE_NAME)
            .getOrdered();
    assertThat(parameters, hasSize(1));
    assertThat(parameters.get(0).getType().getSimpleName(), is(equalTo("CustomType")));
  }

  /**
   * The getParameters static method with a mismatched argument type should throw the proper
   * exception
   */
  @Test
  public void testCreationWithMismatchedPrimitiveTypesThrows() {
    UtamArgument arg = new UtamArgument("attrName", "string");
    UtamError e =
        expectThrows(
            UtamError.class,
            () ->
                UtamArgument.nonLiteralParameters(
                        new UtamArgument[] {arg},
                        Collections.singletonList(PrimitiveType.NUMBER),
                        NODE_NAME)
                    .getOrdered());
    assertThat(
        e.getMessage(),
        containsString(
            String.format(ERR_ARGS_WRONG_TYPE, NODE_NAME, "attrName", "Integer", "String")));
  }

  @Test
  public void testLiteralParameters() {
    List<MethodParameter> parameters =
        literalParameters(
                new UtamArgument[] {
                  new UtamArgument("nameValue", null, null),
                  new UtamArgument(1, null, null),
                  new UtamArgument(true, null, null)
                },
                Arrays.asList(PrimitiveType.STRING, PrimitiveType.NUMBER, PrimitiveType.BOOLEAN),
                "foo")
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
   * The getParameters static method with mismatched primitive data types should throw the proper
   * exception
   */
  @Test
  public void testCreationWithLiteralsWithMismatchedTypesThrows() {
    UtamArgument stringArg = new UtamArgument("nameValue", null, null);
    UtamError e =
        expectThrows(
            UtamError.class,
            () ->
                literalParameters(
                        new UtamArgument[] {stringArg},
                        Collections.singletonList(PrimitiveType.NUMBER),
                        NODE_NAME)
                    .getOrdered());
    assertThat(
        e.getMessage(),
        containsString(
            String.format(ERR_ARGS_WRONG_TYPE, "test", "nameValue", "Integer", "String")));
  }

  /**
   * The getParameters static method with a null value for the args argument should return an empty
   * parameter list
   */
  @Test
  public void testGetParametersWithNullArgumentArray() {
    assertThat(
        nonLiteralParameters(null, TypeProvider.EMPTY_LIST, NODE_NAME).getOrdered(),
        is(equalTo(EMPTY_PARAMETERS)));
    assertThat(unknownTypesParameters(null, NODE_NAME).getOrdered(), is(equalTo(EMPTY_PARAMETERS)));
  }

  /**
   * The getParameters static method with a null value for the args argument with expected type list
   * should throw the proper exception
   */
  @Test
  public void testExpectedMissmatch() {
    UtamError e =
        expectThrows(
            UtamError.class,
            () ->
                UtamArgument.nonLiteralParameters(
                        null,
                        Collections.singletonList(new TypeUtilities.FromString("String", "String")),
                        NODE_NAME)
                    .getOrdered());
    assertThat(
        e.getMessage(), containsString(String.format(ERR_ARGS_NUMBER_MISMATCH, NODE_NAME, 1, 0)));
  }

  /**
   * The getParameters static method with a null value for the expected type in the expected type
   * list should throw the proper exception
   */
  @Test
  public void testNullExpected() {
    literalParameters(
            new UtamArgument[] {new UtamArgument("value", null, null)},
            Collections.singletonList(null),
            NODE_NAME)
        .getOrdered();
  }

  /**
   * The getParameters static method for a set of arguments that allows values with an invalid value
   * type for the argument value should throw the proper exception
   */
  @Test
  public void testGetParametersAllowingValueWithInvalidValueTypeThrows() {
    UtamError e =
        expectThrows(
            UtamError.class,
            () ->
                nonLiteralParameters(
                        new UtamArgument[] {new UtamArgument('c', null, null)},
                        Collections.singletonList(new TypeUtilities.FromString("String", "String")),
                        NODE_NAME)
                    .getOrdered());
    assertThat(e.getMessage(), containsString(String.format(ERR_LITERAL_NOT_SUPPORTED, NODE_NAME)));
  }

  /**
   * The getParameters static method for a set of arguments that does not allow values with a null
   * value for the argument name should throw the proper exception
   */
  @Test
  public void testWithoutNameType() {
    UtamError e =
        expectThrows(
            UtamError.class,
            () ->
                nonLiteralParameters(
                        new UtamArgument[] {new UtamArgument(null, "String")},
                        Collections.singletonList(new TypeUtilities.FromString("String", "String")),
                        NODE_NAME)
                    .getOrdered());
    assertThat(
        e.getMessage(), containsString(String.format(ERR_ARGS_NAME_TYPE_MANDATORY, NODE_NAME)));
    e =
        expectThrows(
            UtamError.class,
            () ->
                nonLiteralParameters(
                        new UtamArgument[] {new UtamArgument("name", null)},
                        Collections.singletonList(new TypeUtilities.FromString("String", "String")),
                        NODE_NAME)
                    .getOrdered());
    assertThat(
        e.getMessage(), containsString(String.format(ERR_ARGS_NAME_TYPE_MANDATORY, NODE_NAME)));
  }
}
