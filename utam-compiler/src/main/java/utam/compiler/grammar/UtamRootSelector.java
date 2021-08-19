/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.Objects;
import java.util.stream.Stream;
import utam.compiler.UtamCompilationError;
import utam.compiler.helpers.LocatorCodeGeneration;
import utam.compiler.helpers.LocatorCodeGeneration.SelectorType;
import utam.core.element.Locator;
import utam.core.framework.consumer.UtamError;
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

  static final String ERR_SELECTOR_MISSING =
      String.format("one of { %s } should be set for selector",
          LocatorCodeGeneration.SUPPORTED_SELECTOR_TYPES_STRING);

  static final String ERR_SELECTOR_REDUNDANT =
      String.format("only one of selector types { %s } can be set",
          LocatorCodeGeneration.SUPPORTED_SELECTOR_TYPES_STRING);
  private final Locator locator;

  private final SelectorType selectorType;

  @JsonCreator
  UtamRootSelector(
      @JsonProperty(value = "css") String css,
      @JsonProperty(value = "accessid") String accessid,
      @JsonProperty(value = "classchain") String classchain,
      @JsonProperty(value = "uiautomator") String uiautomator) {
    if (css != null) {
      locator = LocatorBy.byCss(css);
      selectorType = SelectorType.css;
    } else if (accessid != null) {
      locator = LocatorBy.byAccessibilityId(accessid);
      selectorType = SelectorType.accessid;
    } else if (classchain != null) {
      locator = LocatorBy.byClassChain(classchain);
      selectorType = SelectorType.classchain;
    } else if (uiautomator != null) {
      locator = LocatorBy.byUiAutomator(uiautomator);
      selectorType = SelectorType.uiautomator;
    } else {
      throw new UtamError(ERR_SELECTOR_MISSING);
    }
    if (Stream.of(css, classchain, uiautomator, accessid)
        .filter(Objects::nonNull).toArray().length > 1) {
      throw new UtamCompilationError(ERR_SELECTOR_REDUNDANT);
    }
  }

  Locator getLocator() {
    return locator;
  }

  SelectorType getSelectorType() {
    return selectorType;
  }
}
