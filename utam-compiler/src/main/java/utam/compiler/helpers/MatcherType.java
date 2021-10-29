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
import utam.compiler.UtamCompilationError;
import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.TypeProvider;

/**
 * supported matcher types
 *
 * @author elizaveta.ivanova
 * @since 232
 */
public enum MatcherType {

  isTrue("Boolean.TRUE.equals(%s)", Collections.emptyList(), PrimitiveType.BOOLEAN),
  isFalse("Boolean.FALSE.equals(%s)", Collections.emptyList(), PrimitiveType.BOOLEAN),
  stringContains("(%s!= null && %s.contains(%s))", Collections.singletonList(PrimitiveType.STRING),
      PrimitiveType.STRING),
  stringEquals("%s.equals(%s)", Collections.singletonList(PrimitiveType.STRING),
      PrimitiveType.STRING),
  notNull("%s != null", Collections.emptyList(), JAVA_OBJECT_TYPE);

  private static final String ERR_INCORRECT_MATCHER_FOR_METHOD = ": matcher '%s' requires applied method to return type '%s', returned is '%s'";

  private final String methodCodeMask;
  private final List<TypeProvider> expectedParametersTypes;
  private final TypeProvider operandType;

  MatcherType(String methodCodeMask, List<TypeProvider> expectedParametersTypes,
      TypeProvider operandType) {
    this.methodCodeMask = methodCodeMask;
    this.expectedParametersTypes = expectedParametersTypes;
    this.operandType = operandType;
  }

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

  public List<TypeProvider> getExpectedParametersTypes() {
    return expectedParametersTypes;
  }

  public TypeProvider getOperandType() {
    return operandType;
  }

  public String getIncorrectTypeError(TypeProvider operandType) {
    return String.format(ERR_INCORRECT_MATCHER_FOR_METHOD,
        name(),
        getOperandType().getSimpleName(), operandType.getSimpleName());
  }

  public void checkOperandForMatcher(TypeProvider operandType, String validationContext) {
    if (this == notNull) { // not null allows any type
      return;
    }
    TypeProvider expectedType = getOperandType();
    if (!operandType.isSameType(expectedType)) {
      throw new UtamCompilationError(validationContext + getIncorrectTypeError(operandType));
    }
  }
}
