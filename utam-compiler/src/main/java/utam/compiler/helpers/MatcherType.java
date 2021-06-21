/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.helpers;

import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.TypeProvider;

import java.util.Collections;
import java.util.List;

import static utam.compiler.helpers.ParameterUtils.getParametersValuesString;

/**
 * @author elizaveta.ivanova
 * @since 232
 */
public enum MatcherType {
  isTrue("%s", "return %s;", Collections.EMPTY_LIST, PrimitiveType.BOOLEAN),
  isFalse("Boolean.FALSE.equals(%s)", "return Boolean.FALSE.equals(%s);", Collections.EMPTY_LIST, PrimitiveType.BOOLEAN),
  stringContains("{ String tmp = %s;\nreturn tmp!= null && tmp.contains(%s); }",
      "String tmp = %s;\nreturn tmp!= null && tmp.contains(%s);",
      Collections.singletonList(PrimitiveType.STRING), PrimitiveType.STRING),
  stringEquals("%s.equals(%s)", "return %s.equals(%s);", Collections.singletonList(PrimitiveType.STRING), PrimitiveType.STRING);

  private final String methodCodeMask;
  private final String predicateCodeMask;
  private final List<TypeProvider> expectedParametersTypes;
  private final TypeProvider operandType;

  MatcherType(String methodCodeMask, String predicateCodeMask, List<TypeProvider> expectedParametersTypes, TypeProvider operandType) {
    this.methodCodeMask = methodCodeMask;
    this.predicateCodeMask = predicateCodeMask;
    this.expectedParametersTypes = expectedParametersTypes;
    this.operandType = operandType;
  }

  public String getCode(boolean isInsidePredicate, List<MethodParameter> matcherParameters, String actualValue) {
    String expectedValue = getParametersValuesString(matcherParameters);
    String codeMask = isInsidePredicate? predicateCodeMask : methodCodeMask;
    if(this == isTrue || this == isFalse) {
      return String.format(codeMask, actualValue);
    }
    if(this == stringContains) {
      return String.format(codeMask, actualValue, expectedValue);
    }
    // expected.equals(actual) to avoid NPE if actual is null
    return String.format(codeMask, expectedValue, actualValue);
  }

  public List<TypeProvider> getExpectedParametersTypes() {
    return expectedParametersTypes;
  }

  public TypeProvider getOperandType() {
    return operandType;
  }
}
