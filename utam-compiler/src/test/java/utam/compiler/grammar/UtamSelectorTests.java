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
import static org.testng.Assert.expectThrows;
import static utam.compiler.grammar.TestUtilities.getTestTranslationContext;
import static utam.compiler.grammar.UtamArgument.ERR_ARGS_WRONG_TYPE;
import static utam.compiler.grammar.UtamSelector.ERR_SELECTOR_PARAM_UNKNOWN_TYPE;

import java.util.List;
import org.testng.annotations.Test;
import utam.compiler.helpers.LocatorCodeGeneration;
import utam.compiler.helpers.PrimitiveType;
import utam.core.declarative.representation.MethodParameter;
import utam.core.framework.consumer.UtamError;
import utam.core.selenium.element.LocatorBy;

public class UtamSelectorTests {

  static final String SELECTOR_STRING = "selector";

  static UtamSelector getUtamCssSelector() {
    return new UtamSelector(SELECTOR_STRING);
  }

  static UtamSelector getListCssSelector() {
    return new UtamSelector(SELECTOR_STRING, true);
  }

  private static LocatorCodeGeneration getLocatorContext(UtamSelector utamSelector) {
    return utamSelector.getCodeGenerationHelper(getTestTranslationContext());
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
    UtamSelector selector = new UtamSelector("str[%s] num[%d]", new UtamArgument[]{
        new UtamArgument("strArg", "string"), new UtamArgument("numArg", "number")
    });
    LocatorCodeGeneration context = getLocatorContext(selector);
    List<MethodParameter> parameters = context.getParameters();
    assertThat(parameters, hasSize(2));
    assertThat(parameters.get(0).isLiteral(), is(false));
    assertThat(parameters.get(0).getValue(), is("strArg"));
    assertThat(parameters.get(0).getType(), is(equalTo(PrimitiveType.STRING)));
    assertThat(parameters.get(1).getType(), is(equalTo(PrimitiveType.NUMBER)));
    assertThat(context.getBuilderString(),
        is(equalTo("LocatorBy.byCss(String.format(\"str[%s] num[%d]\", strArg, numArg))")));
  }

  @Test
  public void testSelectorLiteralParameters() {
    UtamSelector selector = new UtamSelector("str[%s] num[%d]",
        new UtamArgument[]{new UtamArgument("\"literal\""), new UtamArgument(1)});
    LocatorCodeGeneration context = getLocatorContext(selector);
    List<MethodParameter> parameters = context.getParameters();
    assertThat(parameters, hasSize(2));
    assertThat(parameters.get(0).getType(), is(equalTo(PrimitiveType.STRING)));
    assertThat(parameters.get(1).getType(), is(equalTo(PrimitiveType.NUMBER)));
    assertThat(context.getBuilderString(),
        is(equalTo("LocatorBy.byCss(String.format(\"str[%s] num[%d]\", \"literal\", 1))")));
  }

  /**
   * should throw the appropriate exception when used with an unknown format specifier
   */
  @Test
  public void testGetParametersWithInvalidArgThrows() {
    UtamSelector selector = new UtamSelector("selector[%f]");
    UtamError e = expectThrows(UtamError.class, () -> getLocatorContext(selector));
    assertThat(
        e.getMessage(), containsString(String.format(ERR_SELECTOR_PARAM_UNKNOWN_TYPE, "%f")));
  }

  @Test
  public void testWrongArgTypeProvided() {
    UtamSelector selector = new UtamSelector("selector[%d]",
        new UtamArgument[]{new UtamArgument("name", "string")});
    UtamError e = expectThrows(UtamError.class, () -> getLocatorContext(selector));
    assertThat(
        e.getMessage(),
        is(containsString(
            String.format(ERR_ARGS_WRONG_TYPE, "selector 'selector[%d]'", "Integer", "String"))));
  }
}
