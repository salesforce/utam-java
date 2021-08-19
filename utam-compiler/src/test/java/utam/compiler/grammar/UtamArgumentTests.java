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
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.testng.Assert.expectThrows;
import static utam.compiler.grammar.TestUtilities.getTestTranslationContext;
import static utam.compiler.grammar.UtamArgument.ELEMENT_TYPE_PROPERTY;
import static utam.compiler.grammar.UtamArgument.ERR_ARGS_DUPLICATE_NAMES;
import static utam.compiler.grammar.UtamArgument.ERR_ARGS_WRONG_COUNT;
import static utam.compiler.grammar.UtamArgument.ERR_ARGS_WRONG_TYPE;
import static utam.compiler.grammar.UtamArgument.ERR_WHILE_PARSING;
import static utam.compiler.grammar.UtamArgument.FUNCTION_TYPE_PROPERTY;
import static utam.compiler.grammar.UtamArgument.SELECTOR_TYPE_PROPERTY;
import static utam.compiler.helpers.TypeUtilities.BASIC_ELEMENT;
import static utam.compiler.helpers.TypeUtilities.FUNCTION;
import static utam.compiler.helpers.TypeUtilities.SELECTOR;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.testng.annotations.Test;
import utam.compiler.grammar.UtamArgument.ArgsProcessor;
import utam.compiler.grammar.UtamArgument.ArgsProcessorWithExpectedTypes;
import utam.compiler.grammar.UtamArgumentDeserializer.ElementReference;
import utam.compiler.helpers.ElementContext;
import utam.compiler.helpers.ElementContext.Basic;
import utam.compiler.helpers.MethodContext;
import utam.compiler.helpers.PrimitiveType;
import utam.compiler.helpers.TranslationContext;
import utam.compiler.representation.ComposeMethodStatement;
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
    ArgsProcessor argsProcessor = new ArgsProcessorWithExpectedTypes(getTestTranslationContext(), ARGS_CONTEXT,
        expectedTypes);
    return argsProcessor.getParameters(args);
  }

  private static MethodParameter getParameter(UtamArgument utamArgument,
      TypeProvider expectedType) {
    TranslationContext translationContext = getTestTranslationContext();
    ArgsProcessor argsProcessor = new ArgsProcessor(translationContext, ARGS_CONTEXT);
    MethodParameter parameter = argsProcessor.getParameter(utamArgument);
    if(parameter != null) {
      argsProcessor.checkExpectedType(expectedType, parameter.getType());
    }
    return parameter;
  }

  @Test
  public void testArgsWithoutExpectedTypes() {
    List<MethodParameter> parameters =
        getOrderedArgs(
            new UtamArgument[]{
                new UtamArgument("name1", "string"),
                new UtamArgument("name2", "number"),
                new UtamArgument("name3", "boolean"),
                new UtamArgument("name4", SELECTOR_TYPE_PROPERTY),
                new UtamArgument("name5", ELEMENT_TYPE_PROPERTY),
            });
    assertThat(parameters, hasSize(5));
    assertThat(parameters.get(0).getValue(), is(equalTo("name1")));
    assertThat(parameters.get(0).getType(), is(equalTo(PrimitiveType.STRING)));
    assertThat(parameters.get(1).getValue(), is(equalTo("name2")));
    assertThat(parameters.get(1).getType(), is(equalTo(PrimitiveType.NUMBER)));
    assertThat(parameters.get(2).getValue(), is(equalTo("name3")));
    assertThat(parameters.get(2).getType(), is(equalTo(PrimitiveType.BOOLEAN)));
    assertThat(parameters.get(3).getValue(), is(equalTo("name4")));
    assertThat(parameters.get(3).getType(), is(equalTo(SELECTOR)));
    assertThat(parameters.get(4).getValue(), is(equalTo("name5")));
    assertThat(parameters.get(4).getType(), is(equalTo(BASIC_ELEMENT)));
  }

  @Test
  public void testArgsWithExpectedTypes() {
    List<TypeProvider> expectedTypes = Stream
        .of(PrimitiveType.STRING, PrimitiveType.NUMBER, PrimitiveType.BOOLEAN, SELECTOR,
            BASIC_ELEMENT)
        .collect(Collectors.toList());
    List<MethodParameter> parameters = getOrderedArgs(
        new UtamArgument[]{
            new UtamArgument("name1", "string"),
            new UtamArgument("name2", "number"),
            new UtamArgument("name3", "boolean"),
            new UtamArgument("name4", SELECTOR_TYPE_PROPERTY),
            new UtamArgument("name5", ELEMENT_TYPE_PROPERTY),
        }, expectedTypes);
    assertThat(parameters, hasSize(5));
    assertThat(parameters.get(0).getValue(), is(equalTo("name1")));
    assertThat(parameters.get(0).getType(), is(equalTo(PrimitiveType.STRING)));
    assertThat(parameters.get(1).getValue(), is(equalTo("name2")));
    assertThat(parameters.get(1).getType(), is(equalTo(PrimitiveType.NUMBER)));
    assertThat(parameters.get(2).getValue(), is(equalTo("name3")));
    assertThat(parameters.get(2).getType(), is(equalTo(PrimitiveType.BOOLEAN)));
    assertThat(parameters.get(3).getValue(), is(equalTo("name4")));
    assertThat(parameters.get(3).getType(), is(equalTo(SELECTOR)));
    assertThat(parameters.get(4).getValue(), is(equalTo("name5")));
    assertThat(parameters.get(4).getType(), is(equalTo(BASIC_ELEMENT)));
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
            PrimitiveType.BOOLEAN, BASIC_ELEMENT,
            SELECTOR));
    List<MethodParameter> parameters =
        processor.getParameters(
            new UtamArgument[]{
                new UtamArgument("nameValue"),
                new UtamArgument(1),
                new UtamArgument(true),
                new UtamArgument(new ElementReference("element")),
                new UtamArgument(new UtamSelector("css"))
            });
    assertThat(parameters, hasSize(5));
    assertThat(parameters.get(0).getValue(), is(equalTo("\"nameValue\"")));
    assertThat(parameters.get(0).getType(), is(equalTo(PrimitiveType.STRING)));
    assertThat(parameters.get(1).getValue(), is(equalTo("1")));
    assertThat(parameters.get(1).getType(), is(equalTo(PrimitiveType.NUMBER)));
    assertThat(parameters.get(2).getValue(), is(equalTo("true")));
    assertThat(parameters.get(2).getType(), is(equalTo(PrimitiveType.BOOLEAN)));
    assertThat(parameters.get(3).getValue(), is(equalTo("this.getElement()")));
    assertThat(parameters.get(3).getType(), is(equalTo(BASIC_ELEMENT)));
    assertThat(parameters.get(4).getValue(), is(equalTo("LocatorBy.byCss(\"css\")")));
    assertThat(parameters.get(4).getType(), is(equalTo(SELECTOR)));
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
                new UtamArgument("nameValue"),
                new UtamArgument(1),
                new UtamArgument(true),
                new UtamArgument(new ElementReference("element")),
                new UtamArgument(new UtamSelector("css"))
            });
    assertThat(parameters, hasSize(5));
    assertThat(parameters.get(0).getValue(), is(equalTo("\"nameValue\"")));
    assertThat(parameters.get(0).getType(), is(equalTo(PrimitiveType.STRING)));
    assertThat(parameters.get(1).getValue(), is(equalTo("1")));
    assertThat(parameters.get(1).getType(), is(equalTo(PrimitiveType.NUMBER)));
    assertThat(parameters.get(2).getValue(), is(equalTo("true")));
    assertThat(parameters.get(2).getType(), is(equalTo(PrimitiveType.BOOLEAN)));
    assertThat(parameters.get(3).getValue(), is(equalTo("this.getElement()")));
    assertThat(parameters.get(3).getType(), is(equalTo(BASIC_ELEMENT)));
    assertThat(parameters.get(4).getValue(), is(equalTo("LocatorBy.byCss(\"css\")")));
    assertThat(parameters.get(4).getType(), is(equalTo(SELECTOR)));
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
                        new UtamArgument(ARG_NAME, "string"),
                        new UtamArgument(ARG_NAME, "number")
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
                new UtamArgument("str"),
                new UtamArgument("str")
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
                    new UtamArgument[]{new UtamArgument("attrName", "string")},
                    Collections.singletonList(PrimitiveType.NUMBER)));
    assertThat(
        e.getMessage(),
        containsString(
            String.format(ERR_ARGS_WRONG_TYPE, ARGS_CONTEXT, "Integer", "String")));
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
                    new UtamArgument[]{new UtamArgument("nameValue")},
                    Collections.singletonList(PrimitiveType.NUMBER)));
    assertThat(
        e.getMessage(),
        containsString(
            String.format(ERR_ARGS_WRONG_TYPE, "test", "Integer", "String")));
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
            new UtamArgument("title", "string")
        });
    UtamArgument arg = new UtamArgument(selector);
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
    UtamArgument utamArgument = new UtamArgument("name", FUNCTION_TYPE_PROPERTY);
    MethodParameter parameter = getParameter(utamArgument, FUNCTION);
    assertThat(parameter, is(nullValue()));

    utamArgument = new UtamArgument(new UtamMethodAction[0]);
    parameter = getParameter(utamArgument, null);
    assertThat(parameter, is(nullValue()));
  }

  @Test
  public void testGetPredicateMethod() {
    TranslationContext context = getTestTranslationContext();
    MethodContext methodContext = new MethodContext();
    UtamMethodAction conditionMock = mock(UtamMethodAction.class);
    when(conditionMock.getComposeAction(context, methodContext, false))
        .thenReturn(mock(ComposeMethodStatement.class));
    UtamArgument utamArgument = new UtamArgument(new UtamMethodAction[]{conditionMock});
    List<ComposeMethodStatement> predicate = utamArgument.getPredicate(context, methodContext);
    assertThat(predicate, is(hasSize(1)));
  }

  @Test
  public void testGetPredicateWithEmptyArray() {
    TranslationContext context = getTestTranslationContext();
    MethodContext methodContext = new MethodContext();
    UtamArgument utamArgument = new UtamArgument(new UtamMethodAction[0]);
    utamArgument.getPredicate(context, methodContext);
  }

  @Test
  public void testUnknownValueThrows() {
    UtamError e = expectThrows(UtamError.class,
        () -> new UtamArgument(new UtamError("?")).getArgByValue(getTestTranslationContext()));
    assertThat(e.getMessage(), containsString(ERR_WHILE_PARSING));
  }

  @Test
  public void testUnknownTypeThrows() {
    UtamError e = expectThrows(UtamError.class,
        () -> new UtamArgument("name", "type").getArgByNameType());
    assertThat(e.getMessage(), containsString(ERR_WHILE_PARSING));
  }
}
