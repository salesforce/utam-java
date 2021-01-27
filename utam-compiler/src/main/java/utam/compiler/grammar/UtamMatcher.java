package utam.compiler.grammar;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import utam.compiler.helpers.MatcherType;
import declarative.representation.MethodParameter;
import declarative.representation.TypeProvider;

import java.util.List;

import static utam.compiler.grammar.UtamArgument.literalParameters;

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

  List<MethodParameter> getParameters(String elementName) {
    return literalParameters(
            args, this.matcherType.getExpectedParametersTypes(),
            String.format("element '%s' matcher '%s'", elementName, matcherType))
        .getOrdered();
  }

  TypeProvider getMatcherOperandType() {
    return this.matcherType.getOperandType();
  }
}
