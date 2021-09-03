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
import utam.compiler.grammar.ArgsProcessor.ArgsProcessorWithExpectedTypes;
import utam.compiler.helpers.MatcherType;
import utam.compiler.helpers.MethodContext;
import utam.compiler.helpers.StatementContext;
import utam.compiler.helpers.TranslationContext;
import utam.core.declarative.representation.MethodParameter;

/**
 * result matcher is used in compose statements or in element filter
 *
 * @author elizaveta.ivanova
 * @since 232
 */
class UtamMatcher {

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

  // get parameters for a matcher inside method statement
  List<MethodParameter> getParameters(TranslationContext context, MethodContext methodContext,
      StatementContext statementContext) {
    String argsContextString = String.format("method '%s'", methodContext.getName());
    ArgsProcessor argsProcessor = new ArgsProcessorWithExpectedTypes(context, argsContextString,
        matcherType, p -> methodContext.setStatementParameter(p, statementContext));
    return argsProcessor.getParameters(args);
  }

  MatcherType getMatcherType() {
    return this.matcherType;
  }
}
