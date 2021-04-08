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
import utam.compiler.helpers.ActionType;
import utam.compiler.helpers.MatcherType;
import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.TypeProvider;
import utam.core.framework.consumer.UtamError;

import java.util.List;

import static utam.compiler.grammar.UtamArgument.getArgsProcessor;
import static utam.compiler.helpers.ActionableActionType.getActionType;

/**
 * @author elizaveta.ivanova
 * @since 228
 */
final class UtamElementFilter {

  static final String ERR_INCORRECT_MATCHER_FOR_METHOD = "element '%s': matcher '%s' needs applied method to return type '%s'";
  final UtamArgument[] applyArgs;
  final String applyMethod;
  final UtamMatcher matcher;
  private final boolean isFindFirst;
  private List<MethodParameter> matcherParameters;
  private List<MethodParameter> applyMethodParameters;

  @JsonCreator
  UtamElementFilter(
      @JsonProperty(value = "apply", required = true) String apply,
      @JsonProperty(value = "args") UtamArgument[] applyArgs,
      @JsonProperty(value = "matcher", required = true) UtamMatcher matcher,
      @JsonProperty(value = "findFirst", defaultValue = "true") boolean isFindFirst) {
    this.applyArgs = applyArgs;
    this.matcher = matcher;
    this.applyMethod = apply;
    this.isFindFirst = isFindFirst;
  }

  // used from tests
  UtamElementFilter(String applyMethod, UtamMatcher matcher) {
    this(applyMethod, null, matcher, false);
  }

  void setElementFilter(
      UtamElement.Type elementNodeType, TypeProvider elementType, String elementName) {
    List<TypeProvider> expectedParameters;
    if (elementNodeType == UtamElement.Type.BASIC) {
      ActionType actionType = getActionType(this.applyMethod, elementType, elementName);
      expectedParameters = actionType.getParametersTypes();
      if(!actionType.getReturnType().equals(matcher.getMatcherOperandType())) {
        throw new UtamError(String.format(ERR_INCORRECT_MATCHER_FOR_METHOD, elementName, matcher.matcherType,
                matcher.getMatcherOperandType().getSimpleName()));
      }
    } else {
      expectedParameters = null;
    }
    this.applyMethodParameters =
        getArgsProcessor(
                applyArgs, expectedParameters, String.format("element '%s' filter", elementName))
            .getOrdered();
    this.matcherParameters = this.matcher.getParameters(elementName);
  }

  List<MethodParameter> getApplyMethodParameters() {
    return this.applyMethodParameters;
  }

  List<MethodParameter> getMatcherParameters() {
    return this.matcherParameters;
  }

  MatcherType getMatcherType() {
    return this.matcher.matcherType;
  }

  boolean getFindFirst() {
    return this.isFindFirst;
  }
}
