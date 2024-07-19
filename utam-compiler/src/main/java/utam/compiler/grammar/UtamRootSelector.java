/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import static utam.compiler.diagnostics.ValidationUtilities.VALIDATION;
import static utam.compiler.grammar.JsonDeserializer.readNode;
import static utam.compiler.helpers.ElementContext.ROOT_ELEMENT_NAME;
import static utam.core.selenium.appium.LocatorClassChain.getSupportedClassChainOperators;
import static utam.core.selenium.appium.LocatorClassChain.getSupportedClassChainQuotes;
import static utam.core.selenium.appium.LocatorUIAutomator.Method.getSupportedMethods;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.JsonNode;
import java.util.Arrays;
import java.util.Objects;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import utam.compiler.UtamCompilationError;
import utam.compiler.helpers.LocatorCodeGeneration.SelectorType;
import utam.core.element.Locator;
import utam.core.selenium.appium.LocatorClassChain;
import utam.core.selenium.appium.LocatorUIAutomator;
import utam.core.selenium.element.LocatorBy;

/**
 * root selector mapping class, base class for element selector. Separated to a base class because
 * root selector can't have args or be marked as a list. Regular element selector requires context
 * to be processed, root selector does not.
 *
 * @author elizaveta.ivanova
 * @since 236
 */
class UtamRootSelector {

  /** The supported selector types as a comma-delimited string */
  private final Locator locator;

  private final SelectorType selectorType;
  private final boolean isMultipleTypes;

  @JsonCreator
  UtamRootSelector(
      @JsonProperty(value = "css") String css,
      @JsonProperty(value = "accessid") String accessid,
      @JsonProperty(value = "classchain") String classchain,
      @JsonProperty(value = "uiautomator") String uiautomator) {
    if (css != null) {
      locator = LocatorBy.byCss(css);
      VALIDATION.validateNotEmptyString(css, "selector", "css");
      selectorType = SelectorType.css;
    } else if (accessid != null) {
      locator = LocatorBy.byAccessibilityId(accessid);
      VALIDATION.validateNotEmptyString(accessid, "selector", "accessid");
      selectorType = SelectorType.accessid;
    } else if (classchain != null) {
      locator = LocatorBy.byClassChain(classchain);
      VALIDATION.validateNotEmptyString(classchain, "selector", "classchain");
      selectorType = SelectorType.classchain;
    } else if (uiautomator != null) {
      locator = LocatorBy.byUiAutomator(uiautomator);
      VALIDATION.validateNotEmptyString(uiautomator, "selector", "uiautomator");
      selectorType = SelectorType.uiautomator;
    } else {
      selectorType = null;
      locator = null;
    }
    isMultipleTypes =
        Stream.of(css, classchain, uiautomator, accessid).filter(Objects::nonNull).toArray().length
            > 1;
  }

  static Locator processRootSelectorNode(JsonNode node) {
    if (node == null || node.isNull()) {
      return null;
    }
    String parserContext = String.format("element \"%s\"", ROOT_ELEMENT_NAME);
    UtamRootSelector selector =
        readNode(node, UtamRootSelector.class, VALIDATION.getErrorMessage(1000, parserContext));
    selector.validateSelector(node, parserContext);
    return selector.getLocator();
  }

  private static void validateUIAutomatorSelector(
      String parserContext, JsonNode node, String uiautomator) {
    try {
      if (uiautomator.startsWith("new UiScrollable")) {
        // Examples
        // 1. new UiScrollable(new UiSelector().scrollable(true)).scrollIntoView(new
        // UiSelector().resourceId("com.salesforce.chatter:id/app_launcher_menu_item"))
        // 2. new UiScrollable(new
        // UiSelector().scrollable(true)).scrollIntoView(resourceId("com.salesforce.chatter:id/app_launcher_menu_item"))
        String match1 =
            uiautomator.substring(
                uiautomator.indexOf(".") + 1,
                uiautomator.indexOf("(", uiautomator.indexOf(".") + 1));
        if (!match1.equals("scrollable")) {
          if (Stream.of(LocatorUIAutomator.Method.values())
              .noneMatch(method -> method.getValue().equals(match1))) {
            throw new UtamCompilationError(
                node,
                VALIDATION.getErrorMessage(1004, parserContext, match1, getSupportedMethods(true)));
          }
        }
        // Extract input to first method
        String match2 = uiautomator.substring(uiautomator.indexOf("))") + 2);
        // Extract inner locator
        // continue validating inner method
        uiautomator = match2.substring(match2.indexOf("(") + 1, match2.indexOf("))") + 1);
      }
      if (uiautomator.startsWith("new UiSelector")) {
        // Example - new UiSelector().resourceId("com.salesforce.chatter:id/app_launcher_menu_item")
        // extract method
        // continue validating method
        uiautomator = uiautomator.substring(uiautomator.indexOf(".") + 1);
      }
      // Example - resourceId("com.salesforce.chatter:id/app_launcher_menu_item")
      String match = uiautomator.substring(0, uiautomator.indexOf("("));
      if (Stream.of(LocatorUIAutomator.Method.values())
          .noneMatch(method -> method.getValue().equals(match))) {
        throw new UtamCompilationError(
            node,
            VALIDATION.getErrorMessage(1004, parserContext, match, getSupportedMethods(false)));
      }
    } catch (StringIndexOutOfBoundsException e) {
      throw new UtamCompilationError(node, VALIDATION.getErrorMessage(1000, parserContext));
    }
  }

  private static void validateSubClassChainSelector(
      String parserContext, JsonNode node, String classchain) {
    // Only do attribute check when using any
    if (classchain.contains("[")) {
      classchain = classchain.substring(classchain.indexOf("[") + 1, classchain.indexOf("]"));
      if (classchain.matches("-?[0-9]*") || classchain.equals("%d")) {
        return;
      }
      // validateQuote
      if (!classchain.startsWith(LocatorClassChain.Quote.SINGLE_BACKTICK.toString())
          && !classchain.startsWith(LocatorClassChain.Quote.SINGLE_DOLLARSIGN.toString())) {
        throw new UtamCompilationError(
            node,
            VALIDATION.getErrorMessage(
                1005, parserContext, classchain, getSupportedClassChainQuotes()));
      }
      // validateOperator
      String usedOpers =
          Stream.of(classchain.split(" "))
              .filter(
                  subString ->
                      (subString.matches("[A-Z]*")
                          || subString.equals(LocatorClassChain.Operator.EQUAL.toString())))
              .collect(Collectors.joining(","));
      if ((Stream.of(usedOpers.split(","))
              .filter(
                  operator ->
                      Arrays.toString(LocatorClassChain.Operator.values()).contains(operator))
              .collect(Collectors.joining(",")))
          .isEmpty()) {
        throw new UtamCompilationError(
            node,
            VALIDATION.getErrorMessage(
                1006, parserContext, classchain, getSupportedClassChainOperators()));
      }
    }
  }

  void validateSelector(JsonNode node, String selectorContext) {
    if (selectorType == null) {
      throw new UtamCompilationError(node, VALIDATION.getErrorMessage(1002, selectorContext));
    }
    if (isMultipleTypes) {
      throw new UtamCompilationError(node, VALIDATION.getErrorMessage(1003, selectorContext));
    }
    if (selectorType == SelectorType.uiautomator) {
      validateUIAutomatorSelector(selectorContext, node, this.locator.getStringValue());
    }
    if (selectorType == SelectorType.classchain) {
      // To avoid to split the string based on the / in attribute part, for example:
      // **/XCUIElementTypeStaticText[`text ==
      // 'https://q3lex.lightning.force.com/lightning/r/Account/sdf/view'`]
      Stream.of(this.locator.getStringValue().split("/XCUIElement"))
          .forEach(str -> validateSubClassChainSelector(selectorContext, node, str));
    }
  }

  Locator getLocator() {
    return locator;
  }

  SelectorType getSelectorType() {
    return selectorType;
  }
}
