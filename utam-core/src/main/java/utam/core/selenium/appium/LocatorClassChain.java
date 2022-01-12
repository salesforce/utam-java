/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.selenium.appium;

import io.appium.java_client.MobileBy;
import java.util.Arrays;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.openqa.selenium.By;
import utam.core.framework.consumer.UtamError;
import utam.core.selenium.element.LocatorBy;

/**
 * owned by mobile team
 *
 * @since 230
 */
public class LocatorClassChain extends LocatorBy {

  private static final String SUPPORTED_CLASSCHAIN_QUOTES =
      Stream.of(LocatorClassChain.Quote.values())
          .map(Quote::toString)
          .collect(Collectors.joining(","));
  static final String ERR_SELECTOR_CLASSCHAIN_UNSUPPORTED_QUOTE =
      String.format("only one of quotes {%s} can be set", SUPPORTED_CLASSCHAIN_QUOTES);
  private static final String SUPPORTED_CLASSCHAIN_OPERATORS =
      Stream.of(LocatorClassChain.Operator.values())
          .map(Operator::toString)
          .collect(Collectors.joining(","));
  static final String ERR_SELECTOR_CLASSCHAIN_UNSUPPORTED_OPERATOR =
      String.format("only operator {%s} can be set, and must be leading and ending with space(s)",
          SUPPORTED_CLASSCHAIN_OPERATORS);

  /**
   * Initializes a new instance of the LocatorClassChain class
   *
   * @param selectorString selector string to use
   */
  public LocatorClassChain(String selectorString) {
    super(selectorString);
    // To avoid to split the string based on the / in attribute part, for example:
    // **/XCUIElementTypeStaticText[`text == 'https://q3lex.lightning.force.com/lightning/r/Account/sdf/view'`]
    Stream.of(selectorString.split("/XCUIElement"))
        .forEach(LocatorClassChain::validateSubClassChainSelector);
  }

  @Override
  public LocatorBy getCopy(String valueWithParameters) {
    return new LocatorClassChain(valueWithParameters);
  }

  private static void validateQuote(String classchain) {
    if (!classchain.startsWith(LocatorClassChain.Quote.SINGLE_BACKTICK.toString())
        && !classchain.startsWith(LocatorClassChain.Quote.SINGLE_DOLLARSIGN.toString())) {
      throw new UtamError(ERR_SELECTOR_CLASSCHAIN_UNSUPPORTED_QUOTE);
    }
  }

  private static void validateOperator(String classchain) {
    String usedOpers = Stream.of(classchain.split(" "))
        .filter(subString -> (subString.matches("[A-Z]*") ||
            subString.equals(LocatorClassChain.Operator.EQUAL.toString())))
        .collect(Collectors.joining(","));
    if ((Stream.of(usedOpers.split(","))
        .filter(operator -> Arrays.toString(LocatorClassChain.Operator.values()).contains(operator))
        .collect(Collectors.joining(","))).isEmpty()) {
      throw new UtamError(ERR_SELECTOR_CLASSCHAIN_UNSUPPORTED_OPERATOR);
    }
  }

  private static void validateSubClassChainSelector(String classchain) {
    // Only do attribute check when using any
    if (classchain.contains("[")) {
      classchain = classchain.substring(classchain.indexOf("[") + 1, classchain.indexOf("]"));
      if (classchain.matches("-?[0-9]*") || classchain.equals("%d")) {
        return;
      }
      validateQuote(classchain);
      validateOperator(classchain);
    }
  }

  @Override
  public By getValue() {
    return MobileBy.iOSClassChain(stringValue);
  }

  enum Quote {
    SINGLE_DOLLARSIGN("$"),
    SINGLE_BACKTICK("`");

    private final String quoteValue;

    Quote(String quoteValue) {
      this.quoteValue = quoteValue;
    }

    @Override
    public String toString() {
      return quoteValue;
    }
  }

  enum Operator {
    EQUAL("=="),
    BEGINSWITH("BEGINSWITH"),
    ENDSWITH("ENDSWITH"),
    CONTAINS("CONTAINS"),
    OR("OR"),
    AND("AND");

    private final String operatorValue;

    Operator(String operatorValue) {
      this.operatorValue = operatorValue;
    }

    @Override
    public String toString() {
      return operatorValue;
    }
  }
}
