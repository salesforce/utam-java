/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.representation;

import static utam.compiler.diagnostics.ValidationUtilities.VALIDATION;

import java.util.List;
import utam.compiler.UtamCompilationError;
import utam.compiler.helpers.MatcherType;
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
  private final Integer wrongTypeErrorCode;
  private final String errorContextString;

  /**
   * Initializes a new instance of the Matcher class
   *
   * @param matcherType        the type of matcher to create
   * @param matcherParameters  the list of parameters for the matcher
   * @param wrongTypeErrorCode error code for incorrect type
   * @param errorContextString error context, one of element name or method name
   */
  public MatcherObject(MatcherType matcherType,
      List<MethodParameter> matcherParameters,
      Integer wrongTypeErrorCode,
      String errorContextString) {
    this.matcherType = matcherType;
    this.matcherParameters = matcherParameters;
    this.wrongTypeErrorCode = wrongTypeErrorCode;
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

  public void checkMatcherOperand(TypeProvider operandType) {
    if (!matcherType.isCorrectOperandType(operandType)) {
      String errorMsg = VALIDATION.getErrorMessage(wrongTypeErrorCode,
          errorContextString, matcherType.getOperandType().getSimpleName(),
          operandType.getSimpleName());
      throw new UtamCompilationError(errorMsg);
    }
  }
}
