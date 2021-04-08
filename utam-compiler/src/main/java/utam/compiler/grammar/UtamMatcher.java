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
import utam.compiler.helpers.MatcherType;
import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.TypeProvider;

import java.util.List;

import static utam.compiler.grammar.UtamArgument.getArgsProcessor;

/**
 * matcher used in compose statements or in element filter
 *
 * @author elizaveta.ivanova
 * @since 232
 */
class UtamMatcher {

  final UtamArgument[] args;
  final MatcherType matcherType;

  @JsonCreator
  UtamMatcher(
      @JsonProperty(value = "type", required = true) MatcherType matcherType,
      @JsonProperty(value = "args") UtamArgument[] args) {
    this.args = args;
    this.matcherType = matcherType;
  }

  List<MethodParameter> getParameters(String matcherContext) {
    return getArgsProcessor(
            args, this.matcherType.getExpectedParametersTypes(),
            String.format("%s: matcher '%s'", matcherContext, matcherType))
        .getOrdered();
  }

  TypeProvider getMatcherOperandType() {
    return this.matcherType.getOperandType();
  }
}
