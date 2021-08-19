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
import java.util.Set;
import java.util.regex.Matcher;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import utam.compiler.helpers.ParameterUtils.Literal;
import utam.core.declarative.representation.MethodParameter;
import utam.core.element.Locator;

/**
 * helper class to generate code for selector
 *
 * @author elizaveta.ivanova
 * @since 234
 */
public class LocatorCodeGeneration {

  public static final Set<String> SUPPORTED_SELECTOR_TYPES = Stream.of(SelectorType.values())
      .map(Enum::name).collect(Collectors.toSet());
  public static final String SUPPORTED_SELECTOR_TYPES_STRING =
      String.join(",", SUPPORTED_SELECTOR_TYPES);

  private final String builderValue;
  private final Locator locator;
  private final List<MethodParameter> parameters;

  public LocatorCodeGeneration(SelectorType type, Locator locator,
      List<MethodParameter> parameters) {
    this.locator = locator;
    this.parameters = parameters;
    this.builderValue = getSelectorAsString(type, locator.getStringValue(), parameters);
  }

  // used in tests
  public LocatorCodeGeneration(Locator locator) {
    this(locator, Collections.emptyList());
  }

  // used in tests
  public LocatorCodeGeneration(Locator locator, List<MethodParameter> parameters) {
    this(SelectorType.css, locator, parameters);
  }

  private static String getSelectorAsString(SelectorType type, String valueStr,
      List<MethodParameter> parameters) {
    String escapedValue = escapeDoubleQuotes(valueStr);
    String stringValue = parameters.isEmpty() ? String.format("\"%s\"", escapedValue) :
        String.format("String.format(\"%s\", %s)", escapedValue,
            getParametersValuesString(parameters));
    return String.format(type.pattern, SELECTOR.getSimpleName(), stringValue);
  }

  private static String escapeDoubleQuotes(String selectorString) {
    return selectorString.replaceAll("\"", Matcher.quoteReplacement("\\\""));
  }

  public String getBuilderString() {
    return builderValue;
  }

  public List<MethodParameter> getParameters() {
    return parameters;
  }

  public Locator getLocator() {
    return locator;
  }

  public MethodParameter getLiteralParameter() {
    String strValue = getBuilderString();
    List<MethodParameter> nestedParameters = getParameters();
    return new Literal(strValue, SELECTOR, nestedParameters);
  }

  public enum SelectorType {
    css("%s.byCss(%s)"),
    accessid("%s.byAccessibilityId(%s)"),
    classchain("%s.byClassChain(%s)"),
    uiautomator("%s.byUiAutomator(%s)");

    private final String pattern;

    SelectorType(String pattern) {
      this.pattern = pattern;
    }
  }
}
