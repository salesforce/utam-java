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
import com.fasterxml.jackson.databind.JsonNode;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import utam.compiler.UtamCompilationError;
import utam.compiler.UtamCompilerIntermediateError;
import utam.compiler.grammar.UtamMethodAction.ArgumentsProvider;
import utam.compiler.helpers.LocatorCodeGeneration;
import utam.compiler.helpers.MethodContext;
import utam.compiler.helpers.ParametersContext;
import utam.compiler.helpers.ParametersContext.StatementParametersContext;
import utam.compiler.helpers.PrimitiveType;
import utam.compiler.helpers.TranslationContext;
import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.TypeProvider;

/**
 * selector object mapping class and methods are public to be used in tests
 *
 * @author elizaveta.ivanova
 * @since 228
 */
class UtamSelector extends UtamRootSelector {

  static final String ERR_SELECTOR_PARAM_UNKNOWN_TYPE =
      "unknown selector parameter type '%s', only string and number are supported";
  private final boolean isReturnAll;
  private final JsonNode argsNode;
  private LocatorCodeGeneration locatorContext;

  @JsonCreator
  UtamSelector(
      @JsonProperty(value = "css") String css,
      @JsonProperty(value = "accessid") String accessid,
      @JsonProperty(value = "classchain") String classchain,
      @JsonProperty(value = "uiautomator") String uiautomator,
      @JsonProperty(value = "returnAll", defaultValue = "false") boolean isReturnAll,
      @JsonProperty(value = "args") JsonNode argsNode) {
    super(css, accessid, classchain, uiautomator);
    this.isReturnAll = isReturnAll;
    this.argsNode = argsNode;
  }

  /**
   * Initializes a new instance of the UtamSelector class, only used in unit tests
   *
   * @param css         the CSS selector
   * @param accessid    the mobile accessibility ID
   * @param classchain  the iOS class chain
   * @param uiautomator the Android UI automator ID
   */
  UtamSelector(String css, String accessid, String classchain, String uiautomator) {
    this(css, accessid, classchain, uiautomator, false, null);
  }

  /**
   * Initializes a new instance of the UtamSelector class, only used in unit tests
   *
   * @param cssSelector the CSS selector
   */
  UtamSelector(String cssSelector) {
    this(cssSelector, null, null, null);
  }

  /**
   * process node
   *
   * @param node        json node
   * @param elementName element for context
   * @return object of selector
   */
  static UtamSelector processSelectorNode(JsonNode node, String elementName) {
    String parserContext = String.format("element \"%s\"", elementName);
    return JsonDeserializer.readNode(node,
        UtamSelector.class,
        cause -> new UtamCompilerIntermediateError(cause, node, "US000", parserContext,
            cause.getMessage()));
  }

  /**
   * parse selector string to find parameters %s or %d
   *
   * @param str selector string
   * @return list of types of selector args
   */
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
        throw new UtamCompilationError(
            String.format(ERR_SELECTOR_PARAM_UNKNOWN_TYPE, str.substring(index, index + 2)));
      }
    }
    return res;
  }

  /**
   * get instance of code generation helper. helper needs context to process args.
   *
   * @param selectorHolder name of the structure that has selector
   * @param context        instance of context
   * @param methodContext  selector can be used as an argument, then we need method context
   * @return helper instance used further for code generation
   */
  LocatorCodeGeneration getCodeGenerationHelper(String selectorHolder, TranslationContext context,
      MethodContext methodContext) {
    if (this.locatorContext == null) {
      String parserContext = String
          .format("%s selector \"%s\"", selectorHolder, getLocator().getStringValue());
      List<TypeProvider> expectedArgsTypes = getParametersTypes(getLocator().getStringValue());
      ArgumentsProvider provider = new ArgumentsProvider(argsNode, parserContext);
      ParametersContext parametersContext = new StatementParametersContext(parserContext, context,
          argsNode, methodContext);
      List<UtamArgument> arguments = provider.getArguments(true);
      List<MethodParameter> parameters = arguments
          .stream()
          .map(arg -> arg.asParameter(context, methodContext, parametersContext))
          .collect(Collectors.toList());
      parametersContext.setNestedParameters(parameters, expectedArgsTypes);
      this.locatorContext = new LocatorCodeGeneration(getSelectorType(), getLocator(), parameters);
    }
    return this.locatorContext;
  }

  boolean isReturnAll() {
    return isReturnAll;
  }
}
