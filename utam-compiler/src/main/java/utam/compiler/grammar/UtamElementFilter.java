/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import static utam.compiler.helpers.BasicElementActionType.getBasicActionType;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.List;
import utam.compiler.grammar.ArgsProcessor.ArgsProcessorBasicAction;
import utam.compiler.helpers.ActionType;
import utam.compiler.helpers.MatcherType;
import utam.compiler.helpers.TranslationContext;
import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.TypeProvider;

/**
 * @author elizaveta.ivanova
 * @since 228
 */
final class UtamElementFilter {

  final String applyMethod;
  private final UtamArgument[] applyArgs;
  private final UtamMatcher matcher;
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

  void setElementFilter(TranslationContext context, UtamElement.Type elementNodeType, TypeProvider elementType, String elementName) {
    String contextString = String.format("element '%s' filter", elementName);
    if (elementNodeType == UtamElement.Type.BASIC) {
      ActionType actionType = getBasicActionType(this.applyMethod, elementType, elementName);
      matcher.getMatcherType().checkOperandForMatcher(actionType.getReturnType(), contextString);
      this.applyMethodParameters = new ArgsProcessorBasicAction(context, contextString, actionType)
          .getParameters(applyArgs);
    } else {
      this.applyMethodParameters = new ArgsProcessor(context, contextString)
          .getParameters(applyArgs);
    }
    this.matcherParameters = this.matcher.getParameters(context, elementName);
  }

  List<MethodParameter> getApplyMethodParameters() {
    return this.applyMethodParameters;
  }

  List<MethodParameter> getMatcherParameters() {
    return this.matcherParameters;
  }

  MatcherType getMatcherType() {
    return this.matcher.getMatcherType();
  }

  boolean getFindFirst() {
    return this.isFindFirst;
  }
}
