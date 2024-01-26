/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.helpers;

import static utam.compiler.helpers.ParameterUtils.getParametersValuesString;
import static utam.compiler.helpers.TypeUtilities.JAVA_OBJECT_TYPE;

import java.util.Collections;
import java.util.List;
import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.TypeProvider;

/**
 * supported matcher types
 *
 * @author elizaveta.ivanova
 * @since 232
 */
public enum MatcherType {

  /** a matcher for a true condition */
  isTrue("Boolean.TRUE.equals(%s)", Collections.emptyList(), PrimitiveType.BOOLEAN),

  /** a matcher for a false condition */
  isFalse("Boolean.FALSE.equals(%s)", Collections.emptyList(), PrimitiveType.BOOLEAN),

  /** a matcher for a string containing a substring */
  stringContains(
      "(%s!= null && %s.contains(%s))",
      Collections.singletonList(PrimitiveType.STRING), PrimitiveType.STRING),

  /** a matcher for a string equaling another string */
  stringEquals(
      "%s.equals(%s)", Collections.singletonList(PrimitiveType.STRING), PrimitiveType.STRING),

  /** a matcher for an object being not null */
  notNull("%s != null", Collections.emptyList(), JAVA_OBJECT_TYPE);

  private final String methodCodeMask;
  private final List<TypeProvider> expectedParametersTypes;
  private final TypeProvider operandType;

  MatcherType(
      String methodCodeMask, List<TypeProvider> expectedParametersTypes, TypeProvider operandType) {
    this.methodCodeMask = methodCodeMask;
    this.expectedParametersTypes = expectedParametersTypes;
    this.operandType = operandType;
  }

  /**
   * Gets the code for the matcher
   *
   * @param actualValue the actual value of the matcher
   * @param matcherParameters the list of parameters for the matcher
   * @return the code for the matcher
   */
  public String getCode(String actualValue, List<MethodParameter> matcherParameters) {
    if (this == isTrue || this == isFalse || this == notNull) {
      return String.format(methodCodeMask, actualValue);
    }
    String expectedValue = getParametersValuesString(matcherParameters);
    if (this == stringContains) {
      return String.format(methodCodeMask, actualValue, actualValue, expectedValue);
    }
    // expected.equals(actual) to avoid NPE if actual is null
    return String.format(methodCodeMask, expectedValue, actualValue);
  }

  /**
   * Gets the expected parameter types for the matcher
   *
   * @return the expected parameter types for the matcher
   */
  public List<TypeProvider> getExpectedParametersTypes() {
    return expectedParametersTypes;
  }

  /**
   * Gets the type of the operand
   *
   * @return the tupe of the operand
   */
  public TypeProvider getOperandType() {
    return operandType;
  }
}
