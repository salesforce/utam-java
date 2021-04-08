/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.appium.element;

import utam.core.selenium.element.LocatorNodeImpl;
import utam.core.selenium.element.LocatorUtilities;

/**
 * @author elizaveta.ivanova
 * @since 230
 */
public class ClassChain extends LocatorNodeImpl {

  ClassChain(String selector) {
    this(selector, selector, LocatorUtilities.EMPTY_FILTER);
  }

  public ClassChain(String selector, String selectorString, Filter filter) {
    super(Mobile.byClassChain(selector), selectorString, LocatorUtilities.DEFAULT_TRANSFORMER, filter);
  }

  @Override
  protected LocatorNodeImpl getCopy() {
    return new ClassChain(getSelector().getValue(), getSelectorString(), getFilterCopy());
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
