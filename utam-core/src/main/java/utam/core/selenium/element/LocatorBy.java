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

  protected final String stringValue;
  private final int parametersCount;

  protected LocatorBy(String stringValue) {
    this.stringValue = stringValue;
    this.parametersCount = getParametersCount(stringValue);
  }

  public static LocatorBy byCss(String value) {
    return new LocatorByCss(value);
  }

  public static LocatorBy byAccessibilityId(String value) {
    return new LocatorAccessibilityId(value);
  }

  public static LocatorBy byClassChain(String value) {
    return new LocatorClassChain(value);
  }

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
  public LocatorBy setParameters(Object...parameters) {
    if (parameters == null || parameters.length == 0) {
      if (parametersCount > 0) {
        throw new ArrayIndexOutOfBoundsException(String
            .format("Locator '%s' requires %d parameters, none were provided", stringValue,
                parametersCount));
      }
    }
    if(parametersCount == 0) {
      return this;
    }
    if (parametersCount > parameters.length) {
      throw new ArrayIndexOutOfBoundsException(String
          .format("Locator '%s' requires %d parameters, only %d were provided", stringValue,
              parametersCount, parameters.length));
    }
    Object[] values = Arrays.copyOfRange(parameters, parameters.length - parametersCount, parameters.length);
    String mutableValue = String.format(stringValue, values);
    return getCopy(mutableValue);
  }

  // public because used from other package
  abstract public LocatorBy getCopy(String valueWithParameters);

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
