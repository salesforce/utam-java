/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.helpers;

import static utam.compiler.helpers.ParameterUtils.getParametersValuesString;
import static utam.compiler.helpers.TypeUtilities.SELECTOR;

import java.util.Collections;
import java.util.List;
import java.util.regex.Matcher;
import utam.compiler.helpers.ParameterUtils.Literal;
import utam.core.declarative.representation.MethodParameter;
import utam.core.element.Locator;
import utam.core.selenium.element.LocatorBy;

/**
 * helper class to generate code for selector
 *
 * @author elizaveta.ivanova
 * @since 234
 */
public class LocatorCodeGeneration {

  private final String builderValue;
  private final Locator locator;
  private final List<MethodParameter> parameters;

  /**
   * Initializes a new instance of the LocatorCodeGeneration class
   *
   * @param type the type of selector
   * @param locator the locator
   * @param parameters the list of parameters
   */
  public LocatorCodeGeneration(
      SelectorType type, Locator locator, List<MethodParameter> parameters) {
    this.locator = locator;
    this.parameters = parameters;
    this.builderValue = getSelectorAsString(type, locator.getStringValue(), parameters);
  }

  /**
   * Initializes a new instance of the LocatorCodeGeneration class, only used in unit tests
   *
   * @param locatorCss the CSS selector fo the loctor to use
   */
  // used in tests
  public LocatorCodeGeneration(String locatorCss) {
    this(SelectorType.css, LocatorBy.byCss(locatorCss), Collections.emptyList());
  }

  private static String getSelectorAsString(
      SelectorType type, String valueStr, List<MethodParameter> parameters) {
    String escapedValue = escapeDoubleQuotes(valueStr);
    String stringValue =
        parameters.isEmpty()
            ? String.format("\"%s\"", escapedValue)
            : String.format(
                "String.format(\"%s\", %s)", escapedValue, getParametersValuesString(parameters));
    return String.format(type.pattern, SELECTOR.getSimpleName(), stringValue);
  }

  private static String escapeDoubleQuotes(String selectorString) {
    return selectorString.replaceAll("\"", Matcher.quoteReplacement("\\\""));
  }

  /**
   * Gets the builder string for the locator
   *
   * @return the builder string
   */
  public String getBuilderString() {
    return builderValue;
  }

  /**
   * Gets the parameters of the locator
   *
   * @return the list of parameters of the locator
   */
  public List<MethodParameter> getParameters() {
    return parameters;
  }

  /**
   * Gets the locator
   *
   * @return the locator
   */
  public Locator getLocator() {
    return locator;
  }

  /**
   * Gets a literal parameter
   *
   * @return the literal parameter for the locator
   */
  public MethodParameter getLiteralParameter() {
    String strValue = getBuilderString();
    List<MethodParameter> nestedParameters = getParameters();
    return new Literal(strValue, SELECTOR, nestedParameters);
  }

  /** The types of selectors usable */
  public enum SelectorType {
    /** A CSS selector */
    css("%s.byCss(%s)"),

    /** An accessibility ID for mobile applications */
    accessid("%s.byAccessibilityId(%s)"),

    /** A class chain for iOS applications */
    classchain("%s.byClassChain(%s)"),

    /** A UIAutomator locator for Android applications */
    uiautomator("%s.byUiAutomator(%s)");

    private final String pattern;

    SelectorType(String pattern) {
      this.pattern = pattern;
    }
  }
}
