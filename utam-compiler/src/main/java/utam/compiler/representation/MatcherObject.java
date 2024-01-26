/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.representation;

import static utam.compiler.diagnostics.ValidationUtilities.VALIDATION;
import static utam.compiler.helpers.MatcherType.*;
import static utam.compiler.helpers.TypeUtilities.VOID;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import utam.compiler.UtamCompilationError;
import utam.compiler.helpers.MatcherType;
import utam.compiler.helpers.PrimitiveType;
import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.TypeProvider;

/**
 * Matcher object includes matcher type and parameters
 *
 * @author elizaveta.ivanova
 * @since 240
 */
public class MatcherObject {

  private final MatcherType matcherType;
  private final List<MethodParameter> matcherParameters;
  private final String errorContextString;

  /**
   * Initializes a new instance of the Matcher class
   *
   * @param matcherType the type of matcher to create
   * @param matcherParameters the list of parameters for the matcher
   * @param errorContextString error context, one of element name or method name
   */
  public MatcherObject(
      MatcherType matcherType, List<MethodParameter> matcherParameters, String errorContextString) {
    this.matcherType = matcherType;
    this.matcherParameters = matcherParameters;
    this.errorContextString = errorContextString;
  }

  /**
   * get matcher type enum
   *
   * @return matcher type
   */
  public MatcherType getMatcherType() {
    return matcherType;
  }

  /**
   * get list of matcher parameters
   *
   * @return list
   */
  public List<MethodParameter> getParameters() {
    return matcherParameters;
  }

  String getCode(String actualValue) {
    return matcherType.getCode(actualValue, getParameters());
  }

  TypeProvider getOperandType() {
    return matcherType.getOperandType();
  }

  /**
   * Checks the operand is of the valid type
   *
   * @param operandType the operand type
   */
  public void checkMatcherOperand(TypeProvider operandType) {
    if (!isCompatibleMatcher(operandType)) {
      String errorMsg =
          VALIDATION.getErrorMessage(
              1202,
              errorContextString,
              operandType.getSimpleName(),
              getCompatibleMatchers(operandType));
      throw new UtamCompilationError(errorMsg);
    }
  }

  private boolean isCompatibleMatcher(TypeProvider operandType) {
    if (matcherType == notNull) { // not null allows any type
      return !operandType.isSameType(VOID);
    }
    return operandType.isSameType(matcherType.getOperandType());
  }

  private static String getCompatibleMatchers(TypeProvider operandType) {
    if (operandType.isSameType(VOID)) {
      return "none";
    }
    if (operandType.isSameType(PrimitiveType.STRING)) {
      return Stream.of(stringContains, stringEquals, notNull)
          .map(Enum::name)
          .collect(Collectors.joining(", "));
    }
    if (operandType.isSameType(PrimitiveType.BOOLEAN)) {
      return Stream.of(isTrue, isFalse, notNull).map(Enum::name).collect(Collectors.joining(", "));
    }
    return notNull.name();
  }
}
