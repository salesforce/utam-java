/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.selenium.appium;

import io.appium.java_client.MobileBy;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.openqa.selenium.By;
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
  private static final String SUPPORTED_CLASSCHAIN_OPERATORS =
      Stream.of(LocatorClassChain.Operator.values())
          .map(Operator::toString)
          .collect(Collectors.joining(","));

  /**
   * Initializes a new instance of the LocatorClassChain class
   *
   * @param selectorString selector string to use
   */
  public LocatorClassChain(String selectorString) {
    super(selectorString);
  }

  public static String getSupportedClassChainQuotes() {
    return SUPPORTED_CLASSCHAIN_QUOTES;
  }

  public static String getSupportedClassChainOperators() {
    return SUPPORTED_CLASSCHAIN_OPERATORS;
  }

  @Override
  public LocatorBy getCopy(String valueWithParameters) {
    return new LocatorClassChain(valueWithParameters);
  }

  @Override
  public By getValue() {
    return MobileBy.iOSClassChain(stringValue);
  }

  public enum Quote {
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

  public enum Operator {
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
