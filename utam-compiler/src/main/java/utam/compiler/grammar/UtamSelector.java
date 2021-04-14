/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import static utam.compiler.grammar.UtamArgument.getArgsProcessor;
import static utam.compiler.helpers.ParameterUtils.getParametersValuesString;
import static utam.compiler.helpers.TypeUtilities.SELECTOR;
import static utam.core.element.Locator.SELECTOR_STRING_PARAMETER;
import static utam.core.selenium.element.LocatorBy.SELECTOR_INTEGER_PARAMETER;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.regex.Matcher;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import utam.compiler.helpers.PrimitiveType;
import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.TypeProvider;
import utam.core.element.Locator;
import utam.core.framework.consumer.UtamError;
import utam.core.selenium.element.LocatorBy;

/**
 * selector object mapping class and methods are public to be used in tests
 *
 * @author elizaveta.ivanova
 * @since 228
 */
public class UtamSelector {

  static final String ERR_ROOT_SELECTOR_LIST = "root selector can't be list";
  static final String ERR_ROOT_SELECTOR_ARGS = "root selector can't have parameters";
  static final String ERR_SELECTOR_PARAM_UNKNOWN_TYPE =
      "unknown selector parameter type '%s', only string and number are supported";
  private static final String SUPPORTED_SELECTOR_TYPES =
      Stream.of(Type.values())
          .map(Enum::name)
          .collect(Collectors.joining(","));


  static final String ERR_SELECTOR_MISSING =
      String.format("one of { %s } should be set for selector", SUPPORTED_SELECTOR_TYPES);
  static final String ERR_SELECTOR_REDUNDANT_FORMAT =
      String.format("only one of selector types { %s } can be set", SUPPORTED_SELECTOR_TYPES);
  final boolean isReturnAll;
  final UtamArgument[] args;
  private final Context context;

  @JsonCreator
  UtamSelector(
      @JsonProperty(value = "css") String css,
      @JsonProperty(value = "accessid") String accessid,
      @JsonProperty(value = "classchain") String classchain,
      @JsonProperty(value = "uiautomator") String uiautomator,
      @JsonProperty(value = "returnAll", defaultValue = "false") boolean isReturnAll,
      @JsonProperty(value = "args") UtamArgument[] args) {
    this.isReturnAll = isReturnAll;
    this.args = args;
    Locator locator;
    Type type;
    if (css != null) {
      locator = LocatorBy.byCss(css);
      type = Type.css;
    } else if (accessid != null) {
      locator = LocatorBy.byAccessibilityId(accessid);
      type = Type.accessid;
    } else if (classchain != null) {
      locator = LocatorBy.byClassChain(classchain);
      type = Type.classchain;
    } else if (uiautomator != null) {
      locator = LocatorBy.byUiAutomator(uiautomator);
      type = Type.uiautomator;
    } else {
      throw new UtamError(ERR_SELECTOR_MISSING);
    }
    checkRedundantValue(css, accessid, classchain, uiautomator);
    String str = locator.getStringValue();
    List<MethodParameter> parameters = getArgsProcessor(args, getParametersTypes(str), String.format("selector '%s'", str))
        .getOrdered();
    context = new Context(type, locator, parameters);
  }

  public Context getContext() {
    return context;
  }

  // used in tests
  UtamSelector(String css, String accessid, String classchain, String uiautomator) {
    this(css, accessid, classchain, uiautomator, false, null);
  }

  // used in tests
  public UtamSelector(String cssSelector) {
    this(cssSelector, null, null, null);
  }

  // used in tests
  public UtamSelector(String cssSelector, boolean isList) {
    this(cssSelector, null, null, null, isList, null);
  }

  // used in tests
  UtamSelector(String cssSelector, UtamArgument[] args) {
    this(cssSelector, null, null, null, false, args);
  }

  private static void checkRedundantValue(String... values) {
    String notNullValue = null;
    for (String value : values) {
      if (value == null) {
        continue;
      }
      if (notNullValue != null) {
        throw new UtamError(ERR_SELECTOR_REDUNDANT_FORMAT);
      }
      notNullValue = value;
    }
  }

  final void validateRootSelector() {
    if (isReturnAll) {
      throw new UtamError(ERR_ROOT_SELECTOR_LIST);
    }
    if (!context.getParameters().isEmpty()) {
      throw new UtamError(ERR_ROOT_SELECTOR_ARGS);
    }
  }

  // parse selector string to find parameters %s or %d
  private static List<TypeProvider> getParametersTypes(String str) {
    List<TypeProvider> res = new ArrayList<>();
    while (str.indexOf("%") > 0) {
      int index = str.indexOf("%");
      if (str.indexOf(SELECTOR_INTEGER_PARAMETER) == index) {
        res.add(PrimitiveType.NUMBER);
        str = str.replaceFirst(SELECTOR_INTEGER_PARAMETER, "");
      } else if (str.indexOf(SELECTOR_STRING_PARAMETER) == index) {
        res.add(PrimitiveType.STRING);
        str = str.replaceFirst(SELECTOR_STRING_PARAMETER, "");
      } else {
        throw new UtamError(
            String.format(ERR_SELECTOR_PARAM_UNKNOWN_TYPE, str.substring(index, index + 2)));
      }
    }
    return res;
  }

  enum Type {
    css("%s.byCss(%s)"),
    accessid("%s.byAccessibilityId(%s)"),
    classchain("%s.byClassChain(%s)"),
    uiautomator("%s.byUiAutomator(%s)");

    private final String pattern;

    Type(String pattern) {
      this.pattern = pattern;
    }
  }

  public static class Context {

    private final String builderValue;
    private final Locator locator;
    private final List<MethodParameter> parameters;

    Context(Type type, Locator locator, List<MethodParameter> parameters) {
      this.locator = locator;
      String stringValue = parameters.isEmpty() ? String
          .format("\"%s\"", escapeDoubleQuotes(locator.getStringValue())) :
          String.format("String.format(\"%s\", %s)",
              escapeDoubleQuotes(locator.getStringValue()), getParametersValuesString(parameters));
      this.builderValue = String.format(type.pattern, SELECTOR.getSimpleName(), stringValue);
      this.parameters = parameters;
    }

    // used in tests
    public Context(Locator locator) {
      this(locator, Collections.EMPTY_LIST);
    }

    // used in tests
    public Context(Locator locator, List<MethodParameter> parameters) {
      this(Type.css, locator, parameters);
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
  }
}
