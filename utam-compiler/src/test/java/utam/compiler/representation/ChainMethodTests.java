/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.representation;

import utam.core.declarative.representation.TypeProvider;
import utam.compiler.grammar.UtamElement;
import utam.compiler.grammar.UtamSelector;
import utam.compiler.representation.PageObjectValidationTestHelper.MethodInfo;
import utam.core.framework.consumer.UtamError;
import org.testng.annotations.Test;
import utam.compiler.helpers.*;

import java.util.Arrays;
import java.util.Collections;

import static utam.compiler.grammar.TestUtilities.*;
import static utam.compiler.representation.ChainMethod.ERR_CHAIN_LINK_ARGS_NOT_SUPPORTED;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.testng.Assert.expectThrows;

/**
 * Provides tests for the ChainMethod class
 *
 * @author james.evans
 */
public class ChainMethodTests {

  private static final TypeProvider FIRST =
      new TypeUtilities.FromString("utam.pageObjects.test.First");
  private static final String METHOD_NAME = "chain";
  private static final String SECOND_TYPE = "utam.pageObjects.test.Second";
  private static final String THIRD_TYPE = "utam.pageObjects.test.Third";
  private static final TypeProvider THIRD = new TypeUtilities.FromString(THIRD_TYPE);
  private static final TypeProvider SECOND = new TypeUtilities.FromString(SECOND_TYPE);
  private static final String LIST_IMPORT = "java.util.List";
  private static final String COLLECTOR_IMPORT = "java.util.stream.Collectors";

  @Test
  public void testFirstSingle() {
    ChainMethod.Link first = new ChainMethod.Link(FIRST, "first", false);
    ChainMethod method = new ChainMethod(METHOD_NAME, Collections.singletonList(first));
    assertThat(method.isPublic(), is(equalTo(true)));
    MethodInfo expected = new MethodInfo(METHOD_NAME, "First");
    expected.addCodeLine("getFirst()");
    expected.addImportedTypes(FIRST.getFullName());
    expected.test(method);
  }

  @Test
  public void testFirstList() {
    ChainMethod.Link first = new ChainMethod.Link(FIRST, "firstList", true);
    ChainMethod method = new ChainMethod(METHOD_NAME, Collections.singletonList(first));
    MethodInfo expected = new MethodInfo(METHOD_NAME, "List<First>");
    expected.addCodeLine("getFirstList()");
    expected.addImportedTypes(FIRST.getFullName(), LIST_IMPORT);
    expected.test(method);
  }

  @Test
  public void testFirstSingleSecondSingle() {
    ChainMethod.Link first = new ChainMethod.Link(FIRST, "first", false);
    ChainMethod.Link second = new ChainMethod.Link(SECOND, "second", false);
    ChainMethod method = new ChainMethod(METHOD_NAME, Arrays.asList(first, second));
    MethodInfo expected = new MethodInfo(METHOD_NAME, "Second");
    expected.addCodeLine("getFirst().getSecond()");
    expected.addImportedTypes(SECOND_TYPE);
    expected.test(method);
  }

  @Test
  public void testFirstSingleSecondSingleThirdSingle() {
    ChainMethod.Link first = new ChainMethod.Link(FIRST, "first", false);
    ChainMethod.Link second = new ChainMethod.Link(SECOND, "second", false);
    ChainMethod.Link third = new ChainMethod.Link(THIRD, "third", false);
    ChainMethod method = new ChainMethod(METHOD_NAME, Arrays.asList(first, second, third));
    MethodInfo expected = new MethodInfo(METHOD_NAME, "Third");
    expected.addCodeLine("getFirst().getSecond().getThird()");
    expected.addImportedTypes(THIRD_TYPE);
    expected.test(method);
  }

  @Test
  public void testFirstSingleSecondSingleThirdList() {
    ChainMethod.Link first = new ChainMethod.Link(FIRST, "first", false);
    ChainMethod.Link second = new ChainMethod.Link(SECOND, "second", false);
    ChainMethod.Link third = new ChainMethod.Link(THIRD, "thirdList", true);
    ChainMethod method = new ChainMethod(METHOD_NAME, Arrays.asList(first, second, third));
    MethodInfo expected = new MethodInfo(METHOD_NAME, "List<Third>");
    expected.addCodeLine("getFirst().getSecond().getThirdList()");
    expected.addImportedTypes(THIRD_TYPE, LIST_IMPORT);
    expected.test(method);
  }

  @Test
  public void testFirstSingleSecondList() {
    ChainMethod.Link first = new ChainMethod.Link(FIRST, "first", false);
    ChainMethod.Link second = new ChainMethod.Link(SECOND, "secondList", true);
    ChainMethod method = new ChainMethod(METHOD_NAME, Arrays.asList(first, second));
    MethodInfo expected = new MethodInfo(METHOD_NAME, "List<Second>");
    expected.addCodeLine("getFirst().getSecondList()");
    expected.addImportedTypes(SECOND_TYPE, LIST_IMPORT);
    expected.test(method);
  }

  @Test
  public void testFirstSingleSecondListThirdSingle() {
    ChainMethod.Link first = new ChainMethod.Link(FIRST, "first", false);
    ChainMethod.Link second = new ChainMethod.Link(SECOND, "secondList", true);
    ChainMethod.Link third = new ChainMethod.Link(THIRD, "third", false);
    ChainMethod method = new ChainMethod(METHOD_NAME, Arrays.asList(first, second, third));
    MethodInfo expected = new MethodInfo(METHOD_NAME, "List<Third>");
    expected.addCodeLine(
        "getFirst().getSecondList()"
            + ".stream().map(element -> element.getThird())"
            + ".collect(Collectors.toList())");
    expected.addImpliedImportedTypes(COLLECTOR_IMPORT);
    expected.addImportedTypes(THIRD_TYPE, LIST_IMPORT);
    expected.test(method);
  }

  @Test
  public void testFirstSingleSecondListThirdList() {
    ChainMethod.Link first = new ChainMethod.Link(FIRST, "first", false);
    ChainMethod.Link second = new ChainMethod.Link(SECOND, "secondList", true);
    ChainMethod.Link third = new ChainMethod.Link(THIRD, "thirdList", true);
    ChainMethod method = new ChainMethod(METHOD_NAME, Arrays.asList(first, second, third));
    MethodInfo expected = new MethodInfo(METHOD_NAME, "List<Third>");
    expected.addCodeLine(
        "getFirst().getSecondList()"
            + ".stream().flatMap(element -> element.getThirdList().stream())"
            + ".collect(Collectors.toList())");
    expected.addImportedTypes(THIRD_TYPE, LIST_IMPORT);
    expected.addImpliedImportedTypes(COLLECTOR_IMPORT);
    expected.test(method);
  }

  @Test
  public void testFirstListSecondSingle() {
    ChainMethod.Link first = new ChainMethod.Link(FIRST, "firstList", true);
    ChainMethod.Link second = new ChainMethod.Link(SECOND, "second", false);
    ChainMethod method = new ChainMethod(METHOD_NAME, Arrays.asList(first, second));
    MethodInfo expected = new MethodInfo(METHOD_NAME, "List<Second>");
    expected.addCodeLine(
        "getFirstList().stream().map(element -> element.getSecond())"
            + ".collect(Collectors.toList())");
    expected.addImportedTypes(SECOND_TYPE, LIST_IMPORT);
    expected.addImpliedImportedTypes(COLLECTOR_IMPORT);
    expected.test(method);
  }

  @Test
  public void testFirstListSecondList() {
    ChainMethod.Link first = new ChainMethod.Link(FIRST, "firstList", true);
    ChainMethod.Link second = new ChainMethod.Link(SECOND, "secondList", true);
    ChainMethod method = new ChainMethod(METHOD_NAME, Arrays.asList(first, second));
    MethodInfo expected = new MethodInfo(METHOD_NAME, "List<Second>");
    expected.addCodeLine(
        "getFirstList().stream().flatMap(element -> element.getSecondList().stream()).collect(Collectors.toList())");
    expected.addImportedTypes(SECOND_TYPE, LIST_IMPORT);
    expected.addImpliedImportedTypes(COLLECTOR_IMPORT);
    expected.test(method);
  }

  @Test
  public void testFirstElementCantHaveArgs() {
    ElementContext elementContext =
        new ElementContext.Custom(
            null,
            "first",
            FIRST,
            getCssSelector("css"),
            false,
            Collections.singletonList(new ParameterUtils.Regular("arg", PrimitiveType.STRING)), false);
    UtamError e = expectThrows(UtamError.class, () -> new ChainMethod.Link(elementContext));
    assertThat(
        e.getMessage(), is(equalTo(String.format(ERR_CHAIN_LINK_ARGS_NOT_SUPPORTED, "first"))));
  }

  @Test
  public void testFirstElement() {
    TranslationContext context = getTestTranslationContext();
    compile(new UtamElement("first", new String[] { TEST_URI }, new UtamSelector("selector")), context);
    ElementContext elementContext = context.getElement("first");
    StringBuilder stringBuilder = new StringBuilder();
    new ChainMethod.Link(elementContext).setCodeString(stringBuilder, ChainMethod.Cardinality.ONE);
    assertThat(
        stringBuilder.toString(), is(equalTo("." + getElementPrivateMethodCalled("first") + "()")));
  }

  @Test
  public void testFirstElementList() {
    TranslationContext context = getTestTranslationContext();
    compile(new UtamElement("first", new String[] { TEST_URI }, new UtamSelector("selector", true)), context);
    ElementContext elementContext = context.getElement("first");
    ChainMethod.Link first = new ChainMethod.Link(elementContext);
    ChainMethod.Link second = new ChainMethod.Link(SECOND, "secondList", false);
    ChainMethod method = new ChainMethod(METHOD_NAME, Arrays.asList(first, second));
    assertThat(
        String.join(";", method.getCodeLines()),
        is(
            equalTo(
                getElementPrivateMethodCalled("first")
                    + "().stream().map(element -> element.getSecondList()).collect(Collectors.toList())")));
  }
}
