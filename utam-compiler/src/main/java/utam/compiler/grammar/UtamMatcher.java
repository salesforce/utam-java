/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.List;
import utam.compiler.UtamCompilationError;
import utam.compiler.grammar.UtamArgument.ArgsProcessor;
import utam.compiler.grammar.UtamArgument.ArgsProcessorWithExpectedTypes;
import utam.compiler.helpers.MatcherType;
import utam.compiler.helpers.MethodContext;
import utam.compiler.helpers.TranslationContext;
import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.TypeProvider;

/**
 * result matcher is used in compose statements or in element filter
 *
 * @author elizaveta.ivanova
 * @since 232
 */
class UtamMatcher {

  static final String ERR_INCORRECT_MATCHER_FOR_METHOD = ": matcher '%s' requires applied method to return type '%s'";
  private final UtamArgument[] args;
  private final MatcherType matcherType;

  @JsonCreator
  UtamMatcher(
      @JsonProperty(value = "type", required = true) MatcherType matcherType,
      @JsonProperty(value = "args") UtamArgument[] args) {
    this.args = args;
    this.matcherType = matcherType;
  }

  // get parameters for a matcher inside element's filter
  List<MethodParameter> getParameters(TranslationContext context, String elementName) {
    String argsContextString = String.format("element '%s'", elementName);
    ArgsProcessor argsProcessor = new ArgsProcessorWithExpectedTypes(context, argsContextString, matcherType);
    return argsProcessor.getParameters(args);
  }

  // get parameters for a matcher inside method
  List<MethodParameter> getParameters(TranslationContext context, MethodContext methodContext) {
    String argsContextString = String.format("method '%s'", methodContext.getName());
    ArgsProcessor argsProcessor = new ArgsProcessorWithExpectedTypes(context, argsContextString, matcherType);
    return argsProcessor.getParameters(args);
  }

  MatcherType getMatcherType() {
    return this.matcherType;
  }

  // check that type provided as result to match is correct
  void checkOperandForMatcher(TypeProvider operandType, String validationContext) {
    TypeProvider expectedType = getMatcherType().getOperandType();
    if (!operandType.isSameType(expectedType)) {
      throw new UtamCompilationError(validationContext
          + String.format(ERR_INCORRECT_MATCHER_FOR_METHOD,
          getMatcherType().name(),
          expectedType.getSimpleName()));
    }
  }
}
