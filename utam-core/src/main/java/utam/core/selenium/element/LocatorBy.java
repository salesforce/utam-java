/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.selenium.element;

import java.util.Arrays;
import java.util.Objects;
import java.util.regex.Pattern;
import org.openqa.selenium.By;
import utam.core.element.Locator;
import utam.core.selenium.appium.LocatorAccessibilityId;
import utam.core.selenium.appium.LocatorClassChain;
import utam.core.selenium.appium.LocatorUIAutomator;

/**
 * combines selector string with its type into one object
 *
 * @author elizaveta.ivanova
 * @since 230
 */
public abstract class LocatorBy implements Locator<By> {

  /** The string value of the locator */
  protected final String stringValue;

  private final int parametersCount;

  /**
   * Initializes a new instance of the LocatorBy class
   *
   * @param stringValue value to use to locate elements
   */
  protected LocatorBy(String stringValue) {
    this.stringValue = stringValue;
    this.parametersCount = getParametersCount(stringValue);
  }

  /**
   * Get a locator using a CSS selector
   *
   * @param value CSS selector to use
   * @return the locator object
   */
  public static LocatorBy byCss(String value) {
    return new LocatorByCss(value);
  }

  /**
   * Get a locator using a mobile accessibility ID selector
   *
   * @param value mobile accessibility ID to use
   * @return the locator object
   */
  public static LocatorBy byAccessibilityId(String value) {
    return new LocatorAccessibilityId(value);
  }

  /**
   * Get a locator using an iOS class chain selector
   *
   * @param value iOS class chain to use
   * @return the locator object
   */
  public static LocatorBy byClassChain(String value) {
    return new LocatorClassChain(value);
  }

  /**
   * Get a locator using an Android UI Automator ID selector
   *
   * @param value Android UI Automator ID to use
   * @return the locator object
   */
  public static LocatorBy byUiAutomator(String value) {
    return new LocatorUIAutomator(value);
  }

  static int getParametersCount(String string) {
    if (string.equals(SELECTOR_STRING_PARAMETER)) {
      return 1;
    }
    return string.split(Pattern.quote(SELECTOR_STRING_PARAMETER)).length
        - 1
        + string.split(Pattern.quote(SELECTOR_INTEGER_PARAMETER)).length
        - 1;
  }

  @Override
  public LocatorBy setParameters(Object... parameters) {
    if (parameters == null || parameters.length == 0) {
      if (parametersCount > 0) {
        throw new ArrayIndexOutOfBoundsException(
            String.format(
                "Locator '%s' requires %d parameters, none were provided",
                stringValue, parametersCount));
      }
    }
    if (parametersCount == 0) {
      return this;
    }
    if (parametersCount > parameters.length) {
      throw new ArrayIndexOutOfBoundsException(
          String.format(
              "Locator '%s' requires %d parameters, only %d were provided",
              stringValue, parametersCount, parameters.length));
    }
    Object[] values =
        Arrays.copyOfRange(parameters, parameters.length - parametersCount, parameters.length);
    String mutableValue = String.format(stringValue, values);
    return getCopy(mutableValue);
  }

  /**
   * Gets a copy of a locator object
   *
   * @param valueWithParameters the value to use
   * @return the locator object
   */
  // public because used from other package
  public abstract LocatorBy getCopy(String valueWithParameters);

  @Override
  public String getStringValue() {
    return stringValue;
  }

  @Override
  public boolean equals(Object obj) {
    if (!(obj instanceof LocatorBy)) {
      return false;
    }
    return ((LocatorBy) obj).getValue().equals(getValue());
  }

  @Override
  public int hashCode() {
    return Objects.hash(getClass(), stringValue);
  }
}
