/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.core.selenium.element;

import utam.core.framework.consumer.UtamError;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

/**
 * locator parameters
 *
 * @author elizaveta.ivanova
 * @since 226
 */
public final class LocatorParameters implements Locator.Parameters {

  public static final String SELECTOR_STRING_PARAMETER = "%s";
  public static final String SELECTOR_INTEGER_PARAMETER = "%d";
  static final String PARAMETERS_NUMBER_ERROR = "out of provided parameters {%s} %d applied";
  static final String UNSUPPORTED_PARAMETER_TYPE =
      "unsupported parameter type '%s', supported are Integer or String";
  static final String NULL_PARAMETER = "Provided parameter is null";
  static final String INDEX_OUT_OF_BOUNDS =
      "index %d is out of bounds: total number of parameters is %d";
  private final List<Value> parameters = new ArrayList<>();
  private int currentIndex = 0;

  @SafeVarargs
  public LocatorParameters(Object... values) {
    if (values == null) {
      throw new NullPointerException(NULL_PARAMETER);
    }
    for (Object value : values) {
      if (value == null) {
        throw new NullPointerException(NULL_PARAMETER);
      }
      if (value instanceof String) {
        parameters.add(new StringValue((String) value));
      } else if (value instanceof Integer) {
        parameters.add(new NumberValue((Integer) value));
      } else {
        throw new IllegalArgumentException(
            String.format(UNSUPPORTED_PARAMETER_TYPE, value.getClass().getName()));
      }
    }
  }

  static int getAllParametersCount(String string) {
    if(string.equals(SELECTOR_STRING_PARAMETER)) { //this can happen for inner text filter
      return 1;
    }
    return string.split(Pattern.quote(SELECTOR_STRING_PARAMETER)).length
        - 1
        + string.split(Pattern.quote(SELECTOR_INTEGER_PARAMETER)).length
        - 1;
  }

  @Override
  public String apply(String target) {
    int count = getAllParametersCount(target);
    if (count <= 0) {
      return target;
    }
    Object[] values = new Object[count];
    for (int i = 0; i < count; i++) {
      values[i] = get();
      next();
    }
    return String.format(target, values);
  }

  @Override
  public boolean isEmpty() {
    return parameters.isEmpty();
  }

  final Object get() {
    if (currentIndex >= 0 && currentIndex < parameters.size()) {
      return parameters.get(currentIndex).getValue();
    }
    throw new IndexOutOfBoundsException(
        String.format(INDEX_OUT_OF_BOUNDS, currentIndex, parameters.size()));
  }

  void next() {
    currentIndex++;
  }

  final void start() {
    currentIndex = 0;
  }

  final void end(LocatorImpl locator) {
    if (currentIndex != parameters.size()) {
      String values =
          parameters.stream().map(p -> p.getValue().toString()).collect(Collectors.joining(","));
      throw new UtamError(
          locator.getErrorPrefix() + String.format(PARAMETERS_NUMBER_ERROR, values, currentIndex));
    }
    currentIndex = -1;
  }

  LocatorImpl setParameters(LocatorImpl locator) {
    return (LocatorImpl) locator.setParameters(this);
  }

  abstract static class Value {

    abstract Object getValue();
  }

  static final class StringValue extends Value {

    private final String value;

    StringValue(String value) {
      this.value = value;
    }

    @Override
    Object getValue() {
      return value;
    }
  }

  static final class NumberValue extends Value {

    private final Integer value;

    NumberValue(Integer value) {
      this.value = value;
    }

    @Override
    Object getValue() {
      return value;
    }
  }
}
