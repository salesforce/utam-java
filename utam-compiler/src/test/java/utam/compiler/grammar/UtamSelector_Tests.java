/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import utam.core.appium.element.UIAutomator;
import utam.compiler.helpers.PrimitiveType;
import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.TypeProvider;
import utam.core.framework.consumer.UtamError;
import org.testng.annotations.Test;
import utam.core.selenium.element.Selector;

import java.util.List;
import java.util.function.Supplier;

import static org.testng.Assert.assertThrows;
import static utam.compiler.grammar.UtamArgument.ERR_ARGS_WRONG_TYPE;
import static utam.compiler.grammar.UtamSelector.*;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;
import static org.testng.Assert.expectThrows;

public class UtamSelector_Tests {

  static final String SELECTOR_STRING = "selector";
  private static final String CLASSCHAIN_SELECTOR_STRING = "**/XCUIElementTypeStaticText[`label == 'something'`]";
  private static final String UIAUTOMATOR_SELECTOR_STRING = "checked()";
  private static final String ELEMENT_NAME = "test";

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
    UtamSelector selector = new UtamSelector("stringArgSelector[%s]");
    selector.args = new UtamArgument[] {new UtamArgument("text", "string")};
    assertThat(selector, is(not(nullValue())));
    assertThat(selector.getSelector().getValue(), is(equalTo("stringArgSelector[%s]")));
    assertThat(selector.getSelector().getType(), is(equalTo(Selector.Type.CSS)));

    selector = new UtamSelector("stringArgSelector[%s]");
    selector.args = new UtamArgument[] {new UtamArgument("text", "string")};
    assertThat(selector, is(not(nullValue())));
    assertThat(selector.getSelector().getValue(), is(equalTo("stringArgSelector[%s]")));
    assertThat(selector.getSelector().getType(), is(equalTo(Selector.Type.CSS)));
  }

  /** The getRootSelector method should return valid values */
  @Test
  public void testGetRootSelector() {
    UtamSelector selector = getUtamCssSelector();
    selector.validateRootSelector();
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
    UtamSelector selector = new UtamSelector("selector[%s]");
    UtamError e = expectThrows(UtamError.class, selector::validateRootSelector);
    assertThat(e.getMessage(), containsString(ERR_ROOT_SELECTOR_ARGS));

    // with number
    selector = new UtamSelector("selector[%d]");
    e = expectThrows(UtamError.class, selector::validateRootSelector);
    assertThat(e.getMessage(), containsString(ERR_ROOT_SELECTOR_ARGS));
  }

  /**
   * The getRootSelector method with a list of expected arguments should throw the proper exception
   */
  @Test
  public void rootSelectorWithArgumentsThrows() {
    UtamSelector selector = getUtamCssSelector();
    selector.args = new UtamArgument[] {};
    UtamError e = expectThrows(UtamError.class, selector::validateRootSelector);
    assertThat(e.getMessage(), containsString(ERR_ROOT_SELECTOR_ARGS));
  }

  /**
   * The getRootSelector method with parameters in the selector and a list of expected arguments
   * should throw the proper exception
   */
  @Test
  public void rootSelectorWithParametersAndArgumentsThrows() {
    UtamSelector selector = new UtamSelector("selector[%s]");
    selector.args = new UtamArgument[] {new UtamArgument("text", "string")};
    UtamError e = expectThrows(UtamError.class, selector::validateRootSelector);
    assertThat(e.getMessage(), containsString(ERR_ROOT_SELECTOR_ARGS));
  }

  /** The getParameters method with a string argument should return valid values */
  @Test
  public void testGetParametersWithStringArg() {
    UtamSelector selector = new UtamSelector("selector[%s]");
    selector.args = new UtamArgument[] {new UtamArgument("text", "string")};
    List<TypeProvider> parametersTypes = selector.getParametersTypes();
    assertThat(parametersTypes, hasSize(1));
    assertThat(parametersTypes.get(0), is(equalTo(PrimitiveType.STRING)));
    List<MethodParameter> parameters = selector.getParameters(ELEMENT_NAME);
    assertThat(parameters, hasSize(1));
    assertThat(parameters.get(0).getType(), is(equalTo(PrimitiveType.STRING)));
    assertThat(parameters.get(0).getValue(), is(equalTo("text")));
    assertThat(parameters.get(0).isLiteral(), is(false));
  }

  /** The getParameters method with a number argument should return valid values */
  @Test
  public void testGetParametersWithIntegerArg() {
    UtamSelector selector = new UtamSelector("selector[%d]");
    selector.args = new UtamArgument[] {new UtamArgument("intArg", "number")};
    List<TypeProvider> parametersTypes = selector.getParametersTypes();
    assertThat(parametersTypes, hasSize(1));
    assertThat(parametersTypes.get(0), is(equalTo(PrimitiveType.NUMBER)));
    List<MethodParameter> parameters = selector.getParameters(ELEMENT_NAME);
    assertThat(parameters, hasSize(1));
    assertThat(parameters.get(0).getType(), is(equalTo(PrimitiveType.NUMBER)));
    assertThat(parameters.get(0).getValue(), is(equalTo("intArg")));
  }

  @Test
  public void testMultiParameters() {
    UtamSelector selector = new UtamSelector("selector[%d][%s]");
    selector.args =
        new UtamArgument[] {
          new UtamArgument("intArg", "number"), new UtamArgument("strArg", "string")
        };
    List<TypeProvider> parametersTypes = selector.getParametersTypes();
    assertThat(parametersTypes, hasSize(2));
    assertThat(parametersTypes.get(0), is(equalTo(PrimitiveType.NUMBER)));
    assertThat(parametersTypes.get(1), is(equalTo(PrimitiveType.STRING)));
    List<MethodParameter> parameters = selector.getParameters(ELEMENT_NAME);
    assertThat(parameters, hasSize(2));
    assertThat(parameters.get(0).getType(), is(equalTo(PrimitiveType.NUMBER)));
    assertThat(parameters.get(1).getType(), is(equalTo(PrimitiveType.STRING)));
  }

  @Test
  public void testLiteralParameter() {
    UtamSelector selector = new UtamSelector("selector[%s]");
    selector.args = new UtamArgument[] {new UtamArgument("\"literal\"")};
    assertThat(selector.getParameters(ELEMENT_NAME).get(0).getValue(), is(equalTo("\"literal\"")));
  }

  /**
   * The getParameters method should throw the appropriate exception when used with an unknown
   * format specifier
   */
  @Test
  public void testGetParametersWithInvalidArgThrows() {
    UtamSelector selector = new UtamSelector("selector[%f]");
    UtamError e = expectThrows(UtamError.class, () -> selector.getParameters(ELEMENT_NAME));
    assertThat(
        e.getMessage(), containsString(String.format(ERR_SELECTOR_PARAM_UNKNOWN_TYPE, "%f")));
    e = expectThrows(UtamError.class, selector::getParametersTypes);
    assertThat(
        e.getMessage(), containsString(String.format(ERR_SELECTOR_PARAM_UNKNOWN_TYPE, "%f")));
  }

  /**
   * The getParameters method should throw the appropriate exception when used with an unknown
   * format specifier
   */
  @Test
  public void testWrongArgTypeProvided() {
    UtamSelector selector = new UtamSelector("selector[%d]");
    selector.args = new UtamArgument[] {new UtamArgument("name", "string")};
    UtamError e = expectThrows(UtamError.class, () -> selector.getParameters(ELEMENT_NAME));
    assertThat(
        e.getMessage(),
        is(equalTo(String.format(ERR_ARGS_WRONG_TYPE, ELEMENT_NAME, "Integer", "String"))));
  }

  /**
   * The getRootSelector and getRootSelectorType method should return valid values accessibility ID
   * selector for native page
   */
  @Test
  public void testGetAccessIDSelector() {
    UtamSelector selector = new UtamSelector(null, SELECTOR_STRING, null, null);
    assertThat(selector.getSelector().getValue(), is(equalTo(SELECTOR_STRING)));
    assertThat(selector.getSelector().getType(), is(equalTo(Selector.Type.ACCESSID)));
  }

  /**
   * The getRootSelector and getRootSelectorType method should return valid values ios classchain
   * selector for native page
   */
  @Test
  public void testGetClassChainSelectorQuotedByBacktick() {
    UtamSelector selector = new UtamSelector(null, null, CLASSCHAIN_SELECTOR_STRING, null);
    assertThat(selector.getSelector().getValue(), is(equalTo(CLASSCHAIN_SELECTOR_STRING)));
    assertThat(selector.getSelector().getType(), is(equalTo(Selector.Type.CLASSCHAIN)));
  }

  @Test
  public void testGetClassChainSelectorQuotedByDollarSign() {
    String locator = "**/XCUIElementTypeStaticText[$label == 'something'$]";
    UtamSelector selector = new UtamSelector(null, null, locator, null);
    assertThat(selector.getSelector().getValue(), is(equalTo(locator)));
    assertThat(selector.getSelector().getType(), is(equalTo(Selector.Type.CLASSCHAIN)));
  }

  @Test
  public void testGetClassChainSelectorWithoutAttribute() {
    String locator = "**/XCUIElementTypeStaticText";
    UtamSelector selector = new UtamSelector(null, null, locator, null);
    assertThat(selector.getSelector().getValue(), is(equalTo(locator)));
    assertThat(selector.getSelector().getType(), is(equalTo(Selector.Type.CLASSCHAIN)));
  }

  @Test
  public void testGetClassChainSelectorWithPositiveIndex() {
    String locator = "**/XCUIElementTypeStaticText[1]";
    UtamSelector selector = new UtamSelector(null, null, locator, null);
    assertThat(selector.getSelector().getValue(), is(equalTo(locator)));
    assertThat(selector.getSelector().getType(), is(equalTo(Selector.Type.CLASSCHAIN)));
  }

  @Test
  public void testGetClassChainSelectorWithNegtiveIndex() {
    String locator = "**/XCUIElementTypeStaticText[-1]";
    UtamSelector selector = new UtamSelector(null, null, locator, null);
    assertThat(selector.getSelector().getValue(), is(equalTo(locator)));
    assertThat(selector.getSelector().getType(), is(equalTo(Selector.Type.CLASSCHAIN)));
  }

  @Test
  public void testGetClassChainSelectorWithIntegerArg() {
    String locator = "**/XCUIElementTypeStaticText[%d]";
    UtamSelector selector = new UtamSelector(null, null, locator, null);
    assertThat(selector.getSelector().getValue(), is(equalTo(locator)));
    assertThat(selector.getSelector().getType(), is(equalTo(Selector.Type.CLASSCHAIN)));
  }

  @Test
  public void testGetClassChainSelectorWithBeginsWith() {
    String locator = "**/XCUIElementTypeStaticText[`label BEGINSWITH 'something'`]";
    UtamSelector selector = new UtamSelector(null, null, locator, null);
    assertThat(selector.getSelector().getValue(), is(equalTo(locator)));
    assertThat(selector.getSelector().getType(), is(equalTo(Selector.Type.CLASSCHAIN)));
  }

  @Test
  public void testGetClassChainSelectorWithEndsWith() {
    String locator = "**/XCUIElementTypeStaticText[`label ENDSWITH 'something'`]";
    UtamSelector selector = new UtamSelector(null, null, locator, null);
    assertThat(selector.getSelector().getValue(), is(equalTo(locator)));
    assertThat(selector.getSelector().getType(), is(equalTo(Selector.Type.CLASSCHAIN)));
  }

  @Test
  public void testGetClassChainSelectorWithContains() {
    String locator = "**/XCUIElementTypeStaticText[`label CONTAINS 'something'`]";
    UtamSelector selector = new UtamSelector(null, null, locator, null);
    assertThat(selector.getSelector().getValue(), is(equalTo(locator)));
    assertThat(selector.getSelector().getType(), is(equalTo(Selector.Type.CLASSCHAIN)));
  }

  @Test
  public void testGetClassChainSelectorWithOr() {
    String locator = "**/XCUIElementTypeStaticText[`label OR 'something'`]";
    UtamSelector selector = new UtamSelector(null, null, locator, null);
    assertThat(selector.getSelector().getValue(), is(equalTo(locator)));
    assertThat(selector.getSelector().getType(), is(equalTo(Selector.Type.CLASSCHAIN)));
  }

  @Test
  public void testGetClassChainSelectorWithAnd() {
    String locator = "**/XCUIElementTypeStaticText[`label AND 'something'`]";
    UtamSelector selector = new UtamSelector(null, null, locator, null);
    assertThat(selector.getSelector().getValue(), is(equalTo(locator)));
    assertThat(selector.getSelector().getType(), is(equalTo(Selector.Type.CLASSCHAIN)));
  }

  @Test
  public void testGetClassChainSelectorWithContainsAnd() {
    String locator = "**/XCUIElementTypeStaticText[`label CONTAINS 'something' AND text == 'fake value'`]";
    UtamSelector selector = new UtamSelector(null, null, locator, null);
    assertThat(selector.getSelector().getValue(), is(equalTo(locator)));
    assertThat(selector.getSelector().getType(), is(equalTo(Selector.Type.CLASSCHAIN)));
  }

  @Test
  public void testGetClassChainSelectorWithMultipleLevels() {
    String locator = "**/XCUIElementTypeCell[`name == 'cell.appTitle'`]/XCUIElementTypeStaticText[-1]";
    UtamSelector selector = new UtamSelector(null, null, locator, null);
    assertThat(selector.getSelector().getValue(), is(equalTo(locator)));
    assertThat(selector.getSelector().getType(), is(equalTo(Selector.Type.CLASSCHAIN)));
  }

  /**
   * The getRootSelector and getRootSelectorType method should return valid values android
   * uiautomator selector for native page
   */
  @Test
  public void testGetUIAutomatorSelectorUsingMethodChecked() {
    UtamSelector selector = new UtamSelector(null, null, null, UIAUTOMATOR_SELECTOR_STRING);
    assertThat(selector.getSelector().getValue(), is(equalTo(UIAutomator.UI_AUTOMATOR_SELECTOR_PREFIX + UIAUTOMATOR_SELECTOR_STRING)));
    assertThat(selector.getSelector().getType(), is(equalTo(Selector.Type.UIAUTOMATOR)));
  }

  @Test
  public void testGetUIAutomatorSelectorUsingMethodCheckable() {
    String locatorString = buildUIAutomatorLocator(UIAutomator.Method.CHECKABLE);
    UtamSelector selector = new UtamSelector(null, null, null, locatorString);
    assertThat(selector.getSelector().getValue(), is(equalTo(UIAutomator.UI_AUTOMATOR_SELECTOR_PREFIX + locatorString)));
    assertThat(selector.getSelector().getType(), is(equalTo(Selector.Type.UIAUTOMATOR)));
  }

  @Test
  public void testGetUIAutomatorSelectorUsingMethodClassName() {
    String locatorString = buildUIAutomatorLocator(UIAutomator.Method.CLASSNAME);
    UtamSelector selector = new UtamSelector(null, null, null, locatorString);
    assertThat(selector.getSelector().getValue(), is(equalTo(UIAutomator.UI_AUTOMATOR_SELECTOR_PREFIX + locatorString)));
    assertThat(selector.getSelector().getType(), is(equalTo(Selector.Type.UIAUTOMATOR)));
  }

  @Test
  public void testGetUIAutomatorSelectorUsingMethodDescription() {
    String locatorString = buildUIAutomatorLocator(UIAutomator.Method.DESCRIPTION);
    UtamSelector selector = new UtamSelector(null, null, null, locatorString);
    assertThat(selector.getSelector().getValue(), is(equalTo(UIAutomator.UI_AUTOMATOR_SELECTOR_PREFIX + locatorString)));
    assertThat(selector.getSelector().getType(), is(equalTo(Selector.Type.UIAUTOMATOR)));
  }

  @Test
  public void testGetUIAutomatorSelectorUsingMethodDescriptionContains() {
    String locatorString = buildUIAutomatorLocator(UIAutomator.Method.DESCRIPTIONCONTAINS);
    UtamSelector selector = new UtamSelector(null, null, null, locatorString);
    assertThat(selector.getSelector().getValue(), is(equalTo(UIAutomator.UI_AUTOMATOR_SELECTOR_PREFIX + locatorString)));
    assertThat(selector.getSelector().getType(), is(equalTo(Selector.Type.UIAUTOMATOR)));
  }

  @Test
  public void testGetUIAutomatorSelectorUsingMethodDescriptionStartsWith() {
    String locatorString = buildUIAutomatorLocator(UIAutomator.Method.DESCRIPTIONSTARTSWITH);
    UtamSelector selector = new UtamSelector(null, null, null, locatorString);
    assertThat(selector.getSelector().getValue(), is(equalTo(UIAutomator.UI_AUTOMATOR_SELECTOR_PREFIX + locatorString)));
    assertThat(selector.getSelector().getType(), is(equalTo(Selector.Type.UIAUTOMATOR)));
  }

  @Test
  public void testGetUIAutomatorSelectorUsingMethodEnabled() {
    String locatorString = buildUIAutomatorLocator(UIAutomator.Method.ENABLED);
    UtamSelector selector = new UtamSelector(null, null, null, locatorString);
    assertThat(selector.getSelector().getValue(), is(equalTo(UIAutomator.UI_AUTOMATOR_SELECTOR_PREFIX + locatorString)));
    assertThat(selector.getSelector().getType(), is(equalTo(Selector.Type.UIAUTOMATOR)));
  }

  @Test
  public void testGetUIAutomatorSelectorUsingMethodResourceId() {
    String locatorString = buildUIAutomatorLocator(UIAutomator.Method.RESOURCEID);
    UtamSelector selector = new UtamSelector(null, null, null, locatorString);
    assertThat(selector.getSelector().getValue(), is(equalTo(UIAutomator.UI_AUTOMATOR_SELECTOR_PREFIX + locatorString)));
    assertThat(selector.getSelector().getType(), is(equalTo(Selector.Type.UIAUTOMATOR)));
  }

  @Test
  public void testGetUIAutomatorSelectorUsingMethodSelected() {
    String locatorString = buildUIAutomatorLocator(UIAutomator.Method.SELECTED);
    UtamSelector selector = new UtamSelector(null, null, null, locatorString);
    assertThat(selector.getSelector().getValue(), is(equalTo(UIAutomator.UI_AUTOMATOR_SELECTOR_PREFIX + locatorString)));
    assertThat(selector.getSelector().getType(), is(equalTo(Selector.Type.UIAUTOMATOR)));
  }

  /** The getRootSelector method should throw exception not configure any selector */
  @Test
  public void testNoSelectorConfigureThrow() {
    UtamError e = expectThrows(UtamError.class, () -> new UtamSelector(null, null, null, null));
    assertThat(e.getMessage(), containsString(ERR_SELECTOR_MISSING));
    e = expectThrows(UtamError.class,  () -> new UtamSelector(null));
    assertThat(e.getMessage(), containsString(ERR_SELECTOR_MISSING));
    UtamSelector empty = new UtamSelector(SELECTOR_STRING);
    assertThat(empty.getSelector().getValue(), is(equalTo(SELECTOR_STRING)));
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

  @Test
  public void testValidateUIAutomatorSelectorIncludingPrefix() {
    UtamError e = expectThrows(UtamError.class, () -> new UtamSelector(null, null, null, "new UiSelector()." + CLASSCHAIN_SELECTOR_STRING));
    assertThat(e.getMessage(), is(equalTo(ERR_SELECTOR_UIAUTOMATOR_WRONG_FORMAT)));
  }

  @Test
  public void testValidateUIAutomatorSelectorUnsupportMethod() {
    UtamError e = expectThrows(UtamError.class, () -> new UtamSelector(null, null, null, "unsupported()"));
    assertThat(e.getMessage(), is(equalTo(ERR_SELECTOR_UIAUTOMATOR_UNSUPPORTED_METHOD)));
    assertThrows(() -> new UtamSelector(null, null, null, "nomethod"));
  }

  @Test
  public void testValidateClassChainSelectorInCorrectQuote() {
      UtamError e = expectThrows(UtamError.class, () -> new UtamSelector(null, null, "**/XCUIElementTypeStaticText[\"label == 'something'\"]", null));
      assertThat(e.getMessage(), is(equalTo(ERR_SELECTOR_CLASSCHAIN_UNSUPPORTED_QUOTE)));
  }

  @Test
  public void testValidateClassChainSelectorNoAttribute() {
    String selectorString = "**/XCUIElementTypeStaticText";
    UtamSelector selector = new UtamSelector(null, null, selectorString, null);
    assertThat(selector.getSelector().getValue(), is(equalTo(selectorString)));
    assertThat(selector.getSelector().getType(), is(equalTo(Selector.Type.CLASSCHAIN)));
  }

  @Test
  public void testValidateClassChainSelectorUnsupportOperatorThrows() {
      UtamError e = expectThrows(UtamError.class, () -> new UtamSelector(null, null, "**/XCUIElementTypeStaticText[$label STARTWITH 'something'$]", null));
      assertThat(e.getMessage(), is(equalTo(ERR_SELECTOR_CLASSCHAIN_UNSUPPORTED_OPERATOR)));
  }

  @Test
  public void testValidateClassChainSelectorNoSpaceThrows() {
      UtamError e = expectThrows(UtamError.class, () -> new UtamSelector(null, null, "**/XCUIElementTypeStaticText[`label=='something'`]", null));
      assertThat(e.getMessage(), is(equalTo(ERR_SELECTOR_CLASSCHAIN_UNSUPPORTED_OPERATOR)));
  }

  @Test
  public void testGetClassChainSelectorIncorrectAtSublevelThrows() {
    UtamError e = expectThrows(UtamError.class, () -> new UtamSelector(null, null, "**/XCUIElementTypeCell[`name == 'cell.appTitle'`]/XCUIElementTypeStaticText[`name EQUALS 'something'`]", null));
    assertThat(e.getMessage(), is(equalTo(ERR_SELECTOR_CLASSCHAIN_UNSUPPORTED_OPERATOR)));
  }

  static String buildUIAutomatorLocator(UIAutomator.Method method) {
      return method.toString() + "()";
  }
}
