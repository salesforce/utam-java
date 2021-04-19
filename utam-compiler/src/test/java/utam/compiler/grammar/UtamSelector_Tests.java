/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import utam.compiler.helpers.PrimitiveType;
import utam.core.declarative.representation.MethodParameter;
import utam.core.framework.consumer.UtamError;
import org.testng.annotations.Test;

import java.util.List;
import java.util.function.Supplier;
import utam.core.selenium.element.LocatorBy;

import static utam.compiler.grammar.UtamArgument.ERR_ARGS_WRONG_TYPE;
import static utam.compiler.grammar.UtamSelector.*;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;
import static org.testng.Assert.expectThrows;

public class UtamSelector_Tests {

  static final String SELECTOR_STRING = "selector";
  private static final String CLASSCHAIN_SELECTOR_STRING = "**/XCUIElementTypeStaticText[`label == 'something'`]";
  private static final String UIAUTOMATOR_SELECTOR_STRING = "checked()";

  static UtamSelector getUtamCssSelector() {
    return new UtamSelector(SELECTOR_STRING);
  }

  static UtamSelector getAccessIdSelector() {
    return new UtamSelector(null, SELECTOR_STRING, null, null);
  }

  static UtamSelector getListCssSelector() {
    return new UtamSelector(SELECTOR_STRING, true);
  }

  private static void testRedundantSelector(Supplier<UtamSelector> utamSelector) {
    UtamError e = expectThrows(UtamError.class, utamSelector::get);
    assertThat(e.getMessage(), is(equalTo(ERR_SELECTOR_REDUNDANT_FORMAT)));
  }

  /** A valid UtamSelector should be able to be created */
  @Test
  public void testCreation() {
    String value = "stringArgSelector[%s]";
    UtamSelector selector = new UtamSelector(value,
        new UtamArgument[]{new UtamArgument("text", "string")});
    UtamSelector.Context context = selector.getContext();
    assertThat(context.getLocator(), is(equalTo(LocatorBy.byCss(value))));
    assertThat(context.getBuilderString(), is(equalTo("LocatorBy.byCss(String.format(\"stringArgSelector[%s]\", text))")));
  }

  /** The getRootSelector method with a list should throw the proper exception */
  @Test
  public void testGetRootSelectorWithListThrows() {
    UtamSelector selector = getListCssSelector();
    UtamError e = expectThrows(UtamError.class, selector::validateRootSelector);
    assertThat(e.getMessage(), containsString(ERR_ROOT_SELECTOR_LIST));
  }

  /**
   * The getRootSelector method with parameters in the selector should throw the proper exception
   */
  @Test
  public void rootSelectorWithSelectorParametersThrows() {
    // with string parameter
    UtamSelector selector = new UtamSelector("selector[%s]", new UtamArgument[] { new UtamArgument("string") });
    UtamError e = expectThrows(UtamError.class, selector::validateRootSelector);
    assertThat(e.getMessage(), containsString(ERR_ROOT_SELECTOR_ARGS));

    // with number
    selector = new UtamSelector("selector[%d]", new UtamArgument[] { new UtamArgument(1) });
    e = expectThrows(UtamError.class, selector::validateRootSelector);
    assertThat(e.getMessage(), containsString(ERR_ROOT_SELECTOR_ARGS));
  }

  /** The getParameters method with a string argument should return valid values */
  @Test
  public void testGetParametersWithStringArg() {
    UtamSelector selector = new UtamSelector("selector[%s]",
        new UtamArgument[] {new UtamArgument("text", "string")});
    UtamSelector.Context context = selector.getContext();
    List<MethodParameter> parameters = context.getParameters();
    assertThat(parameters, hasSize(1));
    assertThat(parameters.get(0).getType(), is(equalTo(PrimitiveType.STRING)));
    assertThat(parameters.get(0).getValue(), is(equalTo("text")));
    assertThat(parameters.get(0).isLiteral(), is(false));
  }

  /** The getParameters method with a number argument should return valid values */
  @Test
  public void testGetParametersWithIntegerArg() {
    UtamSelector selector = new UtamSelector("selector[%d]",
        new UtamArgument[] {new UtamArgument("intArg", "number")});
    UtamSelector.Context context = selector.getContext();
    List<MethodParameter> parameters = context.getParameters();
    assertThat(parameters, hasSize(1));
    assertThat(parameters.get(0).getType(), is(equalTo(PrimitiveType.NUMBER)));
    assertThat(parameters.get(0).getValue(), is(equalTo("intArg")));
  }

  @Test
  public void testMultiParameters() {
    UtamSelector selector = new UtamSelector("selector[%d][%s]", new UtamArgument[] {
          new UtamArgument("intArg", "number"), new UtamArgument("strArg", "string")
        });
    UtamSelector.Context context = selector.getContext();
    List<MethodParameter> parameters = context.getParameters();
    assertThat(parameters, hasSize(2));
    assertThat(parameters.get(0).getType(), is(equalTo(PrimitiveType.NUMBER)));
    assertThat(parameters.get(1).getType(), is(equalTo(PrimitiveType.STRING)));
  }

  @Test
  public void testLiteralParameter() {
    UtamSelector selector = new UtamSelector("selector[%s]",
        new UtamArgument[] {new UtamArgument("\"literal\"")});
    UtamSelector.Context context = selector.getContext();
    List<MethodParameter> parameters = context.getParameters();
    assertThat(parameters.get(0).getValue(), is(equalTo("\"literal\"")));
  }

  /**
   * The getParameters method should throw the appropriate exception when used with an unknown
   * format specifier
   */
  @Test
  public void testGetParametersWithInvalidArgThrows() {
    UtamError e = expectThrows(UtamError.class, () -> new UtamSelector("selector[%f]"));
    assertThat(
        e.getMessage(), containsString(String.format(ERR_SELECTOR_PARAM_UNKNOWN_TYPE, "%f")));
  }

  /**
   * The getParameters method should throw the appropriate exception when used with an unknown
   * format specifier
   */
  @Test
  public void testWrongArgTypeProvided() {
    UtamError e = expectThrows(UtamError.class, () -> new UtamSelector("selector[%d]",
        new UtamArgument[] {new UtamArgument("name", "string")}));
    assertThat(
        e.getMessage(),
        is(containsString(String.format(ERR_ARGS_WRONG_TYPE, "selector 'selector[%d]'", "Integer", "String"))));
  }

  /**
   * The getRootSelector and getRootSelectorType method should return valid values accessibility ID
   * selector for native page
   */
  @Test
  public void testGetAccessIDSelector() {
    UtamSelector selector = new UtamSelector(null, SELECTOR_STRING, null, null);
    UtamSelector.Context context = selector.getContext();
    assertThat(context.getLocator(), is(equalTo(LocatorBy.byAccessibilityId("selector"))));
    assertThat(context.getBuilderString(), is(equalTo("LocatorBy.byAccessibilityId(\"selector\")")));
  }

  /** The getRootSelector method should throw exception not configure any selector */
  @Test
  public void testNoSelectorConfigureThrow() {
    UtamError e = expectThrows(UtamError.class, () -> new UtamSelector(null, null, null, null));
    assertThat(e.getMessage(), containsString(ERR_SELECTOR_MISSING));
    e = expectThrows(UtamError.class,  () -> new UtamSelector(null));
    assertThat(e.getMessage(), containsString(ERR_SELECTOR_MISSING));
  }

  @Test
  public void testRedundantSelector() {
    testRedundantSelector(() -> new UtamSelector(SELECTOR_STRING, SELECTOR_STRING, null, null));
    testRedundantSelector(() -> new UtamSelector(null, SELECTOR_STRING, CLASSCHAIN_SELECTOR_STRING, null));
    testRedundantSelector(() -> new UtamSelector(null, null, CLASSCHAIN_SELECTOR_STRING, UIAUTOMATOR_SELECTOR_STRING));
    testRedundantSelector(() -> new UtamSelector(SELECTOR_STRING, null, null, UIAUTOMATOR_SELECTOR_STRING));
  }

  @Test
  public void testNullSelectorString() {
    UtamError e = expectThrows(UtamError.class, () -> new UtamSelector(null));
    assertThat(e.getMessage(), is(equalTo(ERR_SELECTOR_MISSING)));
  }
}
