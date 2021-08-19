/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import static utam.core.element.Locator.SELECTOR_INTEGER_PARAMETER;
import static utam.core.element.Locator.SELECTOR_STRING_PARAMETER;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.ArrayList;
import java.util.List;
import utam.compiler.grammar.UtamArgument.ArgsProcessor;
import utam.compiler.grammar.UtamArgument.ArgsProcessorWithExpectedTypes;
import utam.compiler.helpers.LocatorCodeGeneration;
import utam.compiler.helpers.PrimitiveType;
import utam.compiler.helpers.TranslationContext;
import utam.core.declarative.representation.TypeProvider;
import utam.core.framework.consumer.UtamError;

/**
 * selector object mapping class and methods are public to be used in tests
 *
 * @author elizaveta.ivanova
 * @since 228
 */
public class UtamSelector extends UtamRootSelector {

  static final String ERR_SELECTOR_PARAM_UNKNOWN_TYPE =
      "unknown selector parameter type '%s', only string and number are supported";
  private final boolean isReturnAll;
  private final UtamArgument[] args;
  private LocatorCodeGeneration context;

  @JsonCreator
  UtamSelector(
      @JsonProperty(value = "css") String css,
      @JsonProperty(value = "accessid") String accessid,
      @JsonProperty(value = "classchain") String classchain,
      @JsonProperty(value = "uiautomator") String uiautomator,
      @JsonProperty(value = "returnAll", defaultValue = "false") boolean isReturnAll,
      @JsonProperty(value = "args") UtamArgument[] args) {
    super(css, accessid, classchain, uiautomator);
    this.isReturnAll = isReturnAll;
    this.args = args;
  }

  // used in tests
  public UtamSelector(String css, String accessid, String classchain, String uiautomator) {
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

  // parse selector string to find parameters %s or %d
  private static List<TypeProvider> getParametersTypes(String str) {
    List<TypeProvider> res = new ArrayList<>();
    while (str.contains("%")) {
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

  /**
   * get instance of code generation helper. helper needs context to process args.
   *
   * @param translationContext instance of context
   * @return helper instance used further for code generation
   */
  public LocatorCodeGeneration getCodeGenerationHelper(TranslationContext translationContext) {
    if (context == null) {
      ArgsProcessor argsProcessor = new ArgsProcessorWithExpectedTypes(translationContext,
          String.format("selector '%s'", getLocator().getStringValue()),
          getParametersTypes(getLocator().getStringValue()));
      context = new LocatorCodeGeneration(getSelectorType(), getLocator(),
          argsProcessor.getParameters(args));
    }
    return context;
  }

  boolean isReturnAll() {
    return isReturnAll;
  }
}
