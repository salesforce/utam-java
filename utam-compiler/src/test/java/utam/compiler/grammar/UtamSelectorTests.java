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
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static org.testng.Assert.expectThrows;
import static utam.compiler.grammar.DeserializerUtilities.expectCompilerErrorFromFile;
import static utam.compiler.grammar.TestUtilities.getTestTranslationContext;

import java.util.List;
import java.util.Objects;
import org.testng.annotations.Test;
import utam.compiler.UtamCompilationError;
import utam.compiler.helpers.LocatorCodeGeneration;
import utam.compiler.helpers.PrimitiveType;
import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.PageObjectMethod;
import utam.core.selenium.element.LocatorBy;

public class UtamSelectorTests {

  private static LocatorCodeGeneration getLocatorContext(UtamSelector utamSelector) {
    return utamSelector.getElementCodeGenerationHelper("test", getTestTranslationContext());
  }

  private static String getBuilderString(UtamSelector selector) {
    return selector.getElementCodeGenerationHelper("test", getTestTranslationContext()).getBuilderString();
  }

  @Test
  public void testSimpleCssSelector() {
    String value = ".css";
    UtamSelector selector = new UtamSelector(value);
    LocatorCodeGeneration context = getLocatorContext(selector);
    assertThat(context.getLocator(), is(equalTo(LocatorBy.byCss(value))));
    assertThat(context.getBuilderString(),
        is(equalTo("LocatorBy.byCss(\".css\")")));
  }

  @Test
  public void testSelectorNonLiteralParameters() {
    PageObjectMethod method = Objects
        .requireNonNull(new DeserializerUtilities().getContext("selector/selectorArgs")
            .getElement("test")).getElementMethod();
    List<MethodParameter> parameters = method.getDeclaration().getParameters();
    assertThat(parameters, hasSize(2));
    MethodParameter strParameter = parameters.get(0);
    assertThat(strParameter.isLiteral(), is(false));
    assertThat(strParameter.getValue(), is("str"));
    assertThat(strParameter.getType().isSameType(PrimitiveType.STRING), is(true));
    assertThat(strParameter.getDescription(), is(nullValue()));
    MethodParameter numberParameter = parameters.get(1);
    assertThat(numberParameter.isLiteral(), is(false));
    assertThat(numberParameter.getValue(), is("num"));
    assertThat(numberParameter.getType().isSameType(PrimitiveType.NUMBER), is(true));
  }

  /**
   * should throw the appropriate exception when used with an unknown format specifier
   */
  @Test
  public void testGetParametersWithInvalidArgThrows() {
    UtamSelector selector = new UtamSelector("selector[%f]");
    RuntimeException e = expectThrows(UtamCompilationError.class, () ->
        selector.getElementCodeGenerationHelper("test", getTestTranslationContext()));
    assertThat(e.getMessage(), containsString("error 111: element \"test\": "
        + "unknown selector parameter type \"%f\", only string and number are supported"));
  }

  @Test
  public void testWrongArgTypeProvided() {
    Exception e = expectCompilerErrorFromFile("selector/selectorWrongArgs");
    assertThat(
        e.getMessage(),
        containsString(
            "error 109: element \"test\" selector \"str[%s]\": parameter \"num\" has incorrect type: "
                + "expected \"String\", found \"Integer\""));
  }

  @Test
  public void testArgsWithDuplicateNamesThrows() {
    Exception e = expectCompilerErrorFromFile("selector/selectorSameArgs");
    assertThat(e.getMessage(),
        containsString(
            "error 107: element \"test\" selector \"str[%s] num[%s]\": parameter with name \"str\" is already declared"));
  }

  @Test
  public void testGetBuilderString() {
    final String prefix = LocatorBy.class.getSimpleName();

    UtamSelector selector = new UtamSelector("css", null, null, null);
    assertThat(getBuilderString(selector), is(equalTo(prefix + ".byCss(\"css\")")));

    selector = new UtamSelector(null, "accessId", null, null);
    assertThat(getBuilderString(selector),
        is(equalTo(prefix + ".byAccessibilityId(\"accessId\")")));

    selector = new UtamSelector(null, null, "chain", null);
    assertThat(getBuilderString(selector), is(equalTo(prefix + ".byClassChain(\"chain\")")));

    selector = new UtamSelector(null, null, null, "new UiSelector().checkable()");
    assertThat(getBuilderString(selector),
        is(equalTo(prefix + ".byUiAutomator(\"new UiSelector().checkable()\")")));
  }

  @Test
  public void testInvalidSelectorFormatThrows() {
    Exception e = expectCompilerErrorFromFile("selector/wrongFormat");
    assertThat(e.getMessage(),
        containsString(
            "error 1000: element \"test\": format of selector is incorrect"));
  }
}
