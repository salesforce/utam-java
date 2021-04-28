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
import static org.testng.Assert.assertThrows;
import static org.testng.Assert.expectThrows;
import static utam.compiler.grammar.TestUtilities.getTestTranslationContext;
import static utam.compiler.grammar.UtamArgument.ERR_ARGS_NAME_TYPE_MANDATORY;
import static utam.compiler.grammar.UtamArgument.ERR_ARGS_TYPE_NOT_SUPPORTED;
import static utam.compiler.grammar.UtamArgument.ERR_ARGS_WRONG_TYPE;
import static utam.compiler.grammar.UtamArgument.ERR_NAME_TYPE_REDUNDANT;
import static utam.compiler.grammar.UtamArgument.ERR_PREDICATE_REDUNDANT;
import static utam.compiler.grammar.UtamArgument.Processor.ERR_ARGS_DUPLICATE_NAMES;
import static utam.compiler.grammar.UtamArgument.Processor.ERR_ARGS_WRONG_COUNT;
import static utam.compiler.grammar.UtamArgument.getArgsProcessor;
import static utam.compiler.helpers.TypeUtilities.FUNCTION;
import static utam.compiler.helpers.TypeUtilities.SELECTOR;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.testng.annotations.Test;
import utam.compiler.helpers.MethodContext;
import utam.compiler.helpers.PrimitiveType;
import utam.compiler.helpers.TranslationContext;
import utam.compiler.helpers.TypeUtilities;
import utam.compiler.representation.ComposeMethodStatement;
import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.TypeProvider;
import utam.core.framework.consumer.UtamError;
import utam.core.selenium.element.LocatorBy;

/**
 * Provides tests for the ArgumentsProcessor type
 *
 * @author james.evans
 */
public class UtamArgument_Tests {

  private static final String ARGS_CONTEXT = "test";

  @Test
  public void testRedundantNameAndType() {
    UtamError e =
        expectThrows(
            UtamError.class,
            () ->
                getArgsProcessor(
                    new UtamArgument[]{new UtamArgument("value", "argName", "string", null)},
                    ARGS_CONTEXT));
    assertThat(e.getMessage(),
        containsString(String.format(ERR_NAME_TYPE_REDUNDANT, ARGS_CONTEXT)));
  }

  @Test
  public void testRedundantPredicate() {
    UtamArgument value = new UtamArgument("value");
    value.conditions = new UtamMethodAction[0];
    UtamArgument nameAndType = new UtamArgument("argName", "string");
    nameAndType.conditions = new UtamMethodAction[0];
    UtamError e =
        expectThrows(
            UtamError.class,
            () ->
                getArgsProcessor(
                    new UtamArgument[]{value},
                    ARGS_CONTEXT));
    assertThat(e.getMessage(),
        containsString(String.format(ERR_PREDICATE_REDUNDANT, ARGS_CONTEXT)));
    e =
        expectThrows(
            UtamError.class,
            () ->
                getArgsProcessor(
                    new UtamArgument[]{nameAndType},
                    ARGS_CONTEXT));
    assertThat(e.getMessage(),
        containsString(String.format(ERR_PREDICATE_REDUNDANT, ARGS_CONTEXT)));
  }

  @Test
  public void testPrimitiveArgsWithoutExpectedTypes() {
    List<MethodParameter> parameters =
        UtamArgument.getArgsProcessor(
            new UtamArgument[]{
                new UtamArgument("name1", "string"),
                new UtamArgument("name2", "number"),
                new UtamArgument("name3", "boolean"),
                new UtamArgument("name4", "locator"),
            }, ARGS_CONTEXT)
            .getOrdered();
    assertThat(parameters, hasSize(4));
    assertThat(parameters.get(0).getValue(), is(equalTo("name1")));
    assertThat(parameters.get(0).getType().getSimpleName(), is(equalTo("String")));
    assertThat(parameters.get(1).getValue(), is(equalTo("name2")));
    assertThat(parameters.get(1).getType().getSimpleName(), is(equalTo("Integer")));
    assertThat(parameters.get(2).getValue(), is(equalTo("name3")));
    assertThat(parameters.get(2).getType().getSimpleName(), is(equalTo("Boolean")));
    assertThat(parameters.get(3).getValue(), is(equalTo("name4")));
    assertThat(parameters.get(3).getType().getSimpleName(),
        is(equalTo(SELECTOR.getSimpleName())));
  }

  @Test
  public void testPrimitiveArgsWithExpectedTypes() {
    List<TypeProvider> expectedTypes = Stream
        .of(PrimitiveType.STRING, PrimitiveType.NUMBER, PrimitiveType.BOOLEAN, SELECTOR)
        .collect(Collectors.toList());
    List<MethodParameter> parameters =
        UtamArgument.getArgsProcessor(
            new UtamArgument[]{
                new UtamArgument("name1", "string"),
                new UtamArgument("name2", "number"),
                new UtamArgument("name3", "boolean"),
                new UtamArgument("name4", "locator"),
            }, expectedTypes, ARGS_CONTEXT)
            .getOrdered();
    assertThat(parameters, hasSize(4));
    assertThat(parameters.get(0).getValue(), is(equalTo("name1")));
    assertThat(parameters.get(0).getType().getSimpleName(), is(equalTo("String")));
    assertThat(parameters.get(1).getValue(), is(equalTo("name2")));
    assertThat(parameters.get(1).getType().getSimpleName(), is(equalTo("Integer")));
    assertThat(parameters.get(2).getValue(), is(equalTo("name3")));
    assertThat(parameters.get(2).getType().getSimpleName(), is(equalTo("Boolean")));
    assertThat(parameters.get(3).getValue(), is(equalTo("name4")));
    assertThat(parameters.get(3).getType().getSimpleName(),
        is(equalTo(SELECTOR.getSimpleName())));
  }

  /**
   * The getParameters static method with duplicate argument names should throw the proper
   * exception
   */
  @Test
  public void testGetParametersWithDuplicateNamesThrows() {
    final String ARG_NAME = "name";
    UtamError e =
        expectThrows(
            UtamError.class,
            () ->
                UtamArgument.getArgsProcessor(
                    new UtamArgument[]{
                        new UtamArgument(ARG_NAME, "string"), new UtamArgument(ARG_NAME, "number")
                    },
                    ARGS_CONTEXT));
    assertThat(
        e.getMessage(),
        containsString(String.format(ERR_ARGS_DUPLICATE_NAMES, ARGS_CONTEXT, ARG_NAME)));
  }

  @Test
  public void duplicateLiteralsAreAllowed() {
    List<MethodParameter> parameters =
        getArgsProcessor(
            new UtamArgument[]{
                new UtamArgument("str"),
                new UtamArgument("str")
            },
            Stream.of(
                PrimitiveType.STRING,
                PrimitiveType.STRING)
                .collect(Collectors.toList()),
            ARGS_CONTEXT).getOrdered();
    assertThat(parameters, hasSize(2));
    assertThat(parameters.get(0).getValue(), is(equalTo("\"str\"")));
    assertThat(parameters.get(0).getType(), is(equalTo(PrimitiveType.STRING)));
    assertThat(parameters.get(1).getValue(), is(equalTo("\"str\"")));
    assertThat(parameters.get(1).getType(), is(equalTo(PrimitiveType.STRING)));
  }

  /**
   * The getParameters static method with an invalid argument type should throw the proper
   * exception
   */
  @Test
  public void testInvalidParameterTypeThrows() {
    UtamError e =
        expectThrows(
            UtamError.class,
            () ->
                getArgsProcessor(
                    new UtamArgument[]{new UtamArgument("name", "int")}, ARGS_CONTEXT));
    assertThat(
        e.getMessage(),
        containsString(String.format(ERR_ARGS_TYPE_NOT_SUPPORTED, ARGS_CONTEXT, "int")));
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
                getArgsProcessor(
                    new UtamArgument[]{},
                    Collections.singletonList(PrimitiveType.NUMBER),
                    ARGS_CONTEXT));
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
                getArgsProcessor(
                    new UtamArgument[]{new UtamArgument("attrName", "string")},
                    Collections.singletonList(PrimitiveType.NUMBER),
                    ARGS_CONTEXT));
    assertThat(
        e.getMessage(),
        containsString(
            String.format(ERR_ARGS_WRONG_TYPE, ARGS_CONTEXT, "Integer", "String")));
  }

  @Test
  public void testLiteralParameters() {
    List<MethodParameter> parameters =
        getArgsProcessor(
            new UtamArgument[]{
                new UtamArgument("nameValue"),
                new UtamArgument(1),
                new UtamArgument(true)
            },
            Arrays.asList(PrimitiveType.STRING, PrimitiveType.NUMBER, PrimitiveType.BOOLEAN),
            "foo")
            .getOrdered();
    assertThat(parameters, hasSize(3));
    assertThat(parameters.get(0).getValue(), is(equalTo("\"nameValue\"")));
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
  public void testLiteralsWithMismatchedTypesThrows() {
    UtamError e =
        expectThrows(
            UtamError.class,
            () ->
                getArgsProcessor(
                    new UtamArgument[]{new UtamArgument("nameValue")},
                    Collections.singletonList(PrimitiveType.NUMBER),
                    ARGS_CONTEXT));
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
        getArgsProcessor(null, Collections.EMPTY_LIST, ARGS_CONTEXT).getOrdered(),
        is(empty()));
    assertThat(getArgsProcessor(null, ARGS_CONTEXT).getOrdered(), is(empty()));
  }

  /**
   * The getParameters static method with a null value for the args argument with expected type list
   * should throw the proper exception
   */
  @Test
  public void testArgsCountError() {
    UtamError e =
        expectThrows(
            UtamError.class,
            () ->
                getArgsProcessor(
                    null,
                    Collections.singletonList(PrimitiveType.STRING),
                    ARGS_CONTEXT));
    assertThat(
        e.getMessage(), containsString(String.format(ERR_ARGS_WRONG_COUNT, ARGS_CONTEXT, 1, 0)));
  }

  /**
   * The getParameters static method for a set of arguments that allows values with an invalid value
   * type for the argument value should throw the proper exception
   */
  @Test
  public void testInvalidValueTypeThrows() {
    UtamError e =
        expectThrows(
            UtamError.class,
            () ->
                getArgsProcessor(
                    new UtamArgument[]{new UtamArgument('c')},
                    ARGS_CONTEXT));
    assertThat(e.getMessage(), containsString(
        String.format(ERR_ARGS_TYPE_NOT_SUPPORTED, ARGS_CONTEXT, Character.class.getName())));
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
                getArgsProcessor(
                    new UtamArgument[]{new UtamArgument(null, "String")},
                    ARGS_CONTEXT));
    assertThat(
        e.getMessage(), containsString(String.format(ERR_ARGS_NAME_TYPE_MANDATORY, ARGS_CONTEXT)));
    e =
        expectThrows(
            UtamError.class,
            () ->
                getArgsProcessor(
                    new UtamArgument[]{new UtamArgument("name", null)},
                    ARGS_CONTEXT));
    assertThat(
        e.getMessage(), containsString(String.format(ERR_ARGS_NAME_TYPE_MANDATORY, ARGS_CONTEXT)));
    e =
        expectThrows(
            UtamError.class,
            () -> getArgsProcessor(new UtamArgument[]{new UtamArgument(null)}, ARGS_CONTEXT));
    assertThat(
        e.getMessage(), containsString(String.format(ERR_ARGS_NAME_TYPE_MANDATORY, ARGS_CONTEXT)));
  }

  @Test
  public void testFunctionTypeReturnsNullParameter() {
    UtamArgument utamArgument = new UtamArgument(null, "name", "function", null);
    assertThat(utamArgument.getParameterOrValue(ARGS_CONTEXT, null), is(nullValue()));
  }

  @Test
  public void testSelectorByNameType() {
    UtamArgument utamArgument = new UtamArgument("name", "locator");
    MethodParameter parameter = utamArgument.getParameterOrValue(ARGS_CONTEXT, null);
    assertThat(parameter.getType(), is(equalTo(SELECTOR)));
    parameter = utamArgument.getParameterOrValue(ARGS_CONTEXT, SELECTOR);
    assertThat(parameter.getType(), is(equalTo(SELECTOR)));
  }

  @Test
  public void testSelectorByValue() {
    UtamArgument utamArgument = new UtamArgument(new UtamSelector(".css"));
    MethodParameter parameter = utamArgument.getParameterOrValue(ARGS_CONTEXT, null);
    assertThat(parameter.getType(), is(equalTo(SELECTOR)));
    parameter = utamArgument.getParameterOrValue(ARGS_CONTEXT, SELECTOR);
    assertThat(parameter.getType(), is(equalTo(SELECTOR)));
    assertThat(parameter.getValue(), is(equalTo("LocatorBy.byCss(\".css\")")));
  }

  @Test
  public void testAllSelectorTypesByValue() {
    final String str = "selector";
    UtamSelector selector = new UtamSelector(null, str, null, null);
    UtamArgument utamArgument = new UtamArgument(selector);
    String prefix = LocatorBy.class.getSimpleName();
    MethodParameter parameter = utamArgument.getParameterOrValue(ARGS_CONTEXT, SELECTOR);
    assertThat(parameter.getValue(), is(equalTo(prefix + ".byAccessibilityId(\"selector\")")));

    selector = new UtamSelector(null, null, str, null);
    utamArgument = new UtamArgument(selector);
    parameter = utamArgument.getParameterOrValue(ARGS_CONTEXT, SELECTOR);
    assertThat(parameter.getValue(), is(equalTo(prefix + ".byClassChain(\"selector\")")));

    selector = new UtamSelector(null, null, null,
        "new UiSelector().checkable()");

    utamArgument = new UtamArgument(selector);
    parameter = utamArgument.getParameterOrValue(ARGS_CONTEXT, SELECTOR);
    assertThat(parameter.getValue(),
        is(equalTo(prefix + ".byUiAutomator(\"new UiSelector().checkable()\")")));
  }

  @Test
  public void testSelectorArgWithParameter() {
    UtamArgument selectorArg = new UtamArgument("title", "string");
    UtamSelector selector = new UtamSelector(".selector[title='%s']", new UtamArgument[] {selectorArg});
    UtamArgument arg = new UtamArgument(selector);
    List<MethodParameter> parameters = UtamArgument.getArgsProcessor(
        new UtamArgument[] {arg}, Arrays.asList(SELECTOR), ARGS_CONTEXT).getOrdered();
    assertThat(parameters, hasSize(2));
    assertThat(parameters.get(0).getValue(),
        is(equalTo("LocatorBy.byCss(String.format(\".selector[title='%s']\", title))")));
    assertThat(parameters.get(0).getType().isSameType(SELECTOR), is(equalTo(true)));
    assertThat(parameters.get(0).getDeclaration(), is(emptyString()));
    assertThat(parameters.get(0).isLiteral(), is(equalTo(true)));
    assertThat(parameters.get(0).isSelectorArgument(), is(equalTo(false)));
    assertThat(parameters.get(1).getValue(), is(equalTo("title")));
    assertThat(parameters.get(1).getType().isSameType(PrimitiveType.STRING), is(equalTo(true)));
    assertThat(parameters.get(1).getDeclaration(), is(equalTo("String title")));
    assertThat(parameters.get(1).isLiteral(), is(equalTo(false)));
    assertThat(parameters.get(1).isSelectorArgument(), is(equalTo(true)));
  }

  @Test
  public void testFunctionArgType() {
    UtamArgument utamArgument = new UtamArgument("name", "function");
    MethodParameter parameter = utamArgument.getParameterOrValue(ARGS_CONTEXT, FUNCTION);
    assertThat(parameter, is(nullValue()));
  }

  @Test
  public void testGetPredicateMethod() {
    TranslationContext context = getTestTranslationContext();
    MethodContext methodContext = new MethodContext("method", null, false);
    UtamArgument utamArgument = new UtamArgument("name", "function");
    UtamMethodAction conditionMock = mock(UtamMethodAction.class);
    when(conditionMock.getComposeAction(context, methodContext, false))
        .thenReturn(mock(ComposeMethodStatement.class));
    utamArgument.conditions = new UtamMethodAction[]{conditionMock};
    List<ComposeMethodStatement> predicate = utamArgument.getPredicate(context, methodContext);
    assertThat(predicate, is(hasSize(1)));
  }

  @Test
  public void testGetPredicateMethodWithValueThrows() {
    TranslationContext context = getTestTranslationContext();
    MethodContext methodContext = new MethodContext("method", null, false);
    UtamArgument utamArgument = new UtamArgument("name", "function");
    utamArgument.value = true;
    assertThrows(() -> utamArgument.getPredicate(context, methodContext));
    utamArgument.value = null;
    utamArgument.conditions = new UtamMethodAction[0];
    assertThrows(() -> utamArgument.getPredicate(context, methodContext));
  }
}
