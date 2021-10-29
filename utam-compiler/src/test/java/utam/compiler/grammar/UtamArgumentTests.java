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
import static org.hamcrest.Matchers.empty;
import static org.hamcrest.Matchers.emptyString;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static org.testng.Assert.expectThrows;
import static utam.compiler.grammar.TestUtilities.getTestTranslationContext;
import static utam.compiler.grammar.UtamArgument.ERR_ARGS_DUPLICATE_NAMES;
import static utam.compiler.grammar.UtamArgument.ERR_ARGS_WRONG_COUNT;
import static utam.compiler.grammar.UtamArgument.ERR_ARGS_WRONG_TYPE;
import static utam.compiler.grammar.UtamArgument.ERR_GET_PREDICATE_NEEDS_PREDICATE_ARG;
import static utam.compiler.grammar.UtamArgument.SELECTOR_TYPE_PROPERTY;
import static utam.compiler.grammar.UtamArgumentDeserializer.getUnsupportedTypeErr;
import static utam.compiler.helpers.TypeUtilities.FUNCTION;
import static utam.compiler.helpers.TypeUtilities.SELECTOR;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.testng.annotations.Test;
import utam.compiler.grammar.ArgsProcessor.ArgsProcessorWithExpectedTypes;
import utam.compiler.grammar.UtamArgument.UtamArgumentLiteralPrimitive;
import utam.compiler.grammar.UtamArgument.UtamArgumentLiteralSelector;
import utam.compiler.helpers.ElementContext;
import utam.compiler.helpers.ElementContext.Basic;
import utam.compiler.helpers.PrimitiveType;
import utam.compiler.helpers.TranslationContext;
import utam.compiler.representation.ElementMethod;
import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.TypeProvider;
import utam.core.framework.consumer.UtamError;

/**
 * Provides tests for the ArgumentsProcessor type
 *
 * @author james.evans
 */
public class UtamArgumentTests {

  private static final String ARGS_CONTEXT = "test";

  private static List<MethodParameter> getOrderedArgs(UtamArgument[] args) {
    ArgsProcessor argsProcessor = new ArgsProcessor(getTestTranslationContext(), ARGS_CONTEXT);
    return argsProcessor.getParameters(args);
  }

  private static List<MethodParameter> getOrderedArgs(UtamArgument[] args,
      List<TypeProvider> expectedTypes) {
    ArgsProcessor argsProcessor = new ArgsProcessorWithExpectedTypes(getTestTranslationContext(),
        ARGS_CONTEXT,
        expectedTypes);
    return argsProcessor.getParameters(args);
  }

  static UtamArgument getNonLiteralArg(String name, String type) {
    return new UtamArgument.UtamArgumentNonLiteral(name, type);
  }

  static UtamArgument getLiteralArg() {
    return new UtamArgumentLiteralPrimitive(true);
  }

  @Test
  public void testArgsWithoutExpectedTypes() {
    List<MethodParameter> parameters =
        getOrderedArgs(
            new UtamArgument[]{
                getNonLiteralArg("name1", "string"),
                getNonLiteralArg("name2", "number"),
                getNonLiteralArg("name3", "boolean"),
                getNonLiteralArg("name4", SELECTOR_TYPE_PROPERTY)
            });
    assertThat(parameters, hasSize(4));
    assertThat(parameters.get(0).getValue(), is(equalTo("name1")));
    assertThat(parameters.get(0).getType(), is(equalTo(PrimitiveType.STRING)));
    assertThat(parameters.get(1).getValue(), is(equalTo("name2")));
    assertThat(parameters.get(1).getType(), is(equalTo(PrimitiveType.NUMBER)));
    assertThat(parameters.get(2).getValue(), is(equalTo("name3")));
    assertThat(parameters.get(2).getType(), is(equalTo(PrimitiveType.BOOLEAN)));
    assertThat(parameters.get(3).getValue(), is(equalTo("name4")));
    assertThat(parameters.get(3).getType(), is(equalTo(SELECTOR)));
  }

  @Test
  public void testArgsWithExpectedTypes() {
    List<TypeProvider> expectedTypes = Stream
        .of(PrimitiveType.STRING, PrimitiveType.NUMBER, PrimitiveType.BOOLEAN, SELECTOR)
        .collect(Collectors.toList());
    List<MethodParameter> parameters = getOrderedArgs(
        new UtamArgument[]{
            getNonLiteralArg("name1", "string"),
            getNonLiteralArg("name2", "number"),
            getNonLiteralArg("name3", "boolean"),
            getNonLiteralArg("name4", SELECTOR_TYPE_PROPERTY),
        }, expectedTypes);
    assertThat(parameters, hasSize(4));
    assertThat(parameters.get(0).getValue(), is(equalTo("name1")));
    assertThat(parameters.get(0).getType(), is(equalTo(PrimitiveType.STRING)));
    assertThat(parameters.get(1).getValue(), is(equalTo("name2")));
    assertThat(parameters.get(1).getType(), is(equalTo(PrimitiveType.NUMBER)));
    assertThat(parameters.get(2).getValue(), is(equalTo("name3")));
    assertThat(parameters.get(2).getType(), is(equalTo(PrimitiveType.BOOLEAN)));
    assertThat(parameters.get(3).getValue(), is(equalTo("name4")));
    assertThat(parameters.get(3).getType(), is(equalTo(SELECTOR)));
  }

  @Test
  public void testLiteralParametersWithExpectedTypes() {
    TranslationContext context = getTestTranslationContext();
    ElementContext elementContext = new Basic("element");
    context.setElement(elementContext);
    elementContext.setElementMethod(new ElementMethod.Single(elementContext, true));
    ArgsProcessor processor = new ArgsProcessorWithExpectedTypes(context, ARGS_CONTEXT,
        Arrays.asList(PrimitiveType.STRING,
            PrimitiveType.NUMBER,
            PrimitiveType.BOOLEAN,
            SELECTOR));
    List<MethodParameter> parameters =
        processor.getParameters(
            new UtamArgument[]{
                new UtamArgumentLiteralPrimitive("nameValue"),
                new UtamArgumentLiteralPrimitive(1),
                getLiteralArg(),
                new UtamArgumentLiteralSelector(new UtamSelector("css"), "locator")
            });
    assertThat(parameters, hasSize(4));
    assertThat(parameters.get(0).getValue(), is(equalTo("\"nameValue\"")));
    assertThat(parameters.get(0).getType(), is(equalTo(PrimitiveType.STRING)));
    assertThat(parameters.get(1).getValue(), is(equalTo("1")));
    assertThat(parameters.get(1).getType(), is(equalTo(PrimitiveType.NUMBER)));
    assertThat(parameters.get(2).getValue(), is(equalTo("true")));
    assertThat(parameters.get(2).getType(), is(equalTo(PrimitiveType.BOOLEAN)));
    assertThat(parameters.get(3).getValue(), is(equalTo("LocatorBy.byCss(\"css\")")));
    assertThat(parameters.get(3).getType(), is(equalTo(SELECTOR)));
  }

  @Test
  public void testLiteralParameters() {
    TranslationContext context = getTestTranslationContext();
    ElementContext elementContext = new Basic("element");
    context.setElement(elementContext);
    elementContext.setElementMethod(new ElementMethod.Single(elementContext, true));
    ArgsProcessor processor = new ArgsProcessor(context, ARGS_CONTEXT);
    List<MethodParameter> parameters =
        processor.getParameters(
            new UtamArgument[]{
                new UtamArgumentLiteralPrimitive("nameValue"),
                new UtamArgumentLiteralPrimitive(1),
                getLiteralArg(),
                new UtamArgumentLiteralSelector(new UtamSelector("css"), "locator")
            });
    assertThat(parameters, hasSize(4));
    assertThat(parameters.get(0).getValue(), is(equalTo("\"nameValue\"")));
    assertThat(parameters.get(0).getType(), is(equalTo(PrimitiveType.STRING)));
    assertThat(parameters.get(1).getValue(), is(equalTo("1")));
    assertThat(parameters.get(1).getType(), is(equalTo(PrimitiveType.NUMBER)));
    assertThat(parameters.get(2).getValue(), is(equalTo("true")));
    assertThat(parameters.get(2).getType(), is(equalTo(PrimitiveType.BOOLEAN)));
    assertThat(parameters.get(3).getValue(), is(equalTo("LocatorBy.byCss(\"css\")")));
    assertThat(parameters.get(3).getType(), is(equalTo(SELECTOR)));
  }

  /**
   * method with duplicate argument names should throw the proper exception
   */
  @Test
  public void testDuplicateNamesThrows() {
    final String ARG_NAME = "name";
    UtamError e =
        expectThrows(
            UtamError.class,
            () ->
                getOrderedArgs(
                    new UtamArgument[]{
                        getNonLiteralArg(ARG_NAME, "string"),
                        getNonLiteralArg(ARG_NAME, "number")
                    }));
    assertThat(
        e.getMessage(),
        containsString(String.format(ERR_ARGS_DUPLICATE_NAMES, ARGS_CONTEXT, ARG_NAME)));
  }

  @Test
  public void duplicateLiteralsAreAllowed() {
    List<MethodParameter> parameters =
        getOrderedArgs(
            new UtamArgument[]{
                new UtamArgumentLiteralPrimitive("str"),
                new UtamArgumentLiteralPrimitive("str")
            },
            Stream.of(
                PrimitiveType.STRING,
                PrimitiveType.STRING)
                .collect(Collectors.toList()));
    assertThat(parameters, hasSize(2));
    assertThat(parameters.get(0).getValue(), is(equalTo("\"str\"")));
    assertThat(parameters.get(0).getType(), is(equalTo(PrimitiveType.STRING)));
    assertThat(parameters.get(1).getValue(), is(equalTo("\"str\"")));
    assertThat(parameters.get(1).getType(), is(equalTo(PrimitiveType.STRING)));
  }

  /**
   * The getParameters static method should throw the proper exception when the number of parameters
   * does not match
   */
  @Test
  public void testExpectedCountErr() {
    UtamError e = expectThrows(UtamError.class,
        () ->
            getOrderedArgs(
                new UtamArgument[]{},
                Collections.singletonList(PrimitiveType.NUMBER)));
    assertThat(
        e.getMessage(), containsString(String.format(ERR_ARGS_WRONG_COUNT, ARGS_CONTEXT, 1, 0)));
  }

  /**
   * The getParameters static method with a mismatched argument type should throw the proper
   * exception
   */
  @Test
  public void testMismatchedPrimitiveTypesThrows() {
    UtamError e =
        expectThrows(
            UtamError.class,
            () ->
                getOrderedArgs(
                    new UtamArgument[]{
                        getNonLiteralArg("attrName", "string")
                    },
                    Collections.singletonList(PrimitiveType.NUMBER)));
    assertThat(
        e.getMessage(),
        containsString(
            String.format(ERR_ARGS_WRONG_TYPE, ARGS_CONTEXT, "attrName", "Integer", "String")));
  }

  /**
   * The getParameters static method with mismatched primitive data types should throw the proper
   * exception
   */
  @Test
  public void testLiteralsWithMismatchedTypesThrows() {
    UtamError e =
        expectThrows(
            UtamError.class,
            () ->
                getOrderedArgs(
                    new UtamArgument[]{
                        new UtamArgumentLiteralPrimitive("nameValue")
                    },
                    Collections.singletonList(PrimitiveType.NUMBER)));
    assertThat(
        e.getMessage(),
        containsString(
            String.format(ERR_ARGS_WRONG_TYPE, "test", "\"nameValue\"", "Integer", "String")));
  }

  /**
   * The getParameters static method with a null value for the args argument should return an empty
   * parameter list
   */
  @Test
  public void testGetParametersWithNullArgumentArray() {
    assertThat(
        getOrderedArgs(null, Collections.emptyList()),
        is(empty()));
    assertThat(getOrderedArgs(null), is(empty()));
  }

  /**
   * The getParameters static method with a null value for the args argument with expected type list
   * should throw the proper exception
   */
  @Test
  public void testArgsCountError() {
    UtamError e = expectThrows(UtamError.class, () ->
        getOrderedArgs(
            null,
            Collections.singletonList(PrimitiveType.STRING)));
    assertThat(
        e.getMessage(), containsString(String.format(ERR_ARGS_WRONG_COUNT, ARGS_CONTEXT, 1, 0)));
  }

  @Test
  public void testSelectorArgWithParameter() {
    UtamSelector selector = new UtamSelector(".selector[%s]",
        new UtamArgument[]{
            getNonLiteralArg("title", "string")
        });
    UtamArgument arg = new UtamArgumentLiteralSelector(selector, "locator");
    List<MethodParameter> parameters = getOrderedArgs(
        new UtamArgument[]{arg}, Collections.singletonList(SELECTOR));
    assertThat(parameters, hasSize(1));
    assertThat(parameters.get(0).getValue(),
        is(equalTo("LocatorBy.byCss(String.format(\".selector[%s]\", title))")));
    assertThat(parameters.get(0).getType().isSameType(SELECTOR), is(equalTo(true)));
    assertThat(parameters.get(0).getDeclaration(), is(emptyString()));
    assertThat(parameters.get(0).isLiteral(), is(equalTo(true)));
  }

  @Test
  public void testFunctionArgType() {
    UtamArgument utamArgument = new UtamArgument.UtamArgumentPredicate(new UtamMethodAction[0]);
    TranslationContext translationContext = getTestTranslationContext();
    ArgsProcessor argsProcessor = new ArgsProcessor(translationContext, ARGS_CONTEXT);
    MethodParameter parameter = utamArgument.asParameter(translationContext);
    argsProcessor.checkParameter(parameter, FUNCTION);
    assertThat(parameter, is(nullValue()));
  }

  @Test
  public void testGetPredicateMethodOnNonPredicateThrows() {
    UtamArgument utamArgument = getLiteralArg();
    Exception e = expectThrows(IllegalStateException.class,
        () -> utamArgument.getPredicate(null, null));
    assertThat(e.getMessage(), is(equalTo(ERR_GET_PREDICATE_NEEDS_PREDICATE_ARG)));
  }

  @Test
  public void testUnknownTypeThrows() {
    UtamError e = expectThrows(UtamError.class,
        () -> getNonLiteralArg("name", "type").asParameter(getTestTranslationContext()));
    assertThat(e.getMessage(), containsString(getUnsupportedTypeErr("type")));
  }
}
