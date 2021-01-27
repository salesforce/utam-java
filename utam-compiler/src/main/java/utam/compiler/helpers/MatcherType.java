package utam.compiler.helpers;

import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.TypeProvider;
import utam.core.framework.consumer.UtamError;

import java.util.Collections;
import java.util.List;

import static utam.compiler.helpers.ParameterUtils.getParametersValuesString;
import static utam.compiler.helpers.TypeUtilities.isParametersTypesMatch;

/**
 * @author elizaveta.ivanova
 * @since 232
 */
public enum MatcherType {
  isTrue("%s", Collections.EMPTY_LIST, PrimitiveType.BOOLEAN),
  isFalse("Boolean.FALSE.equals(%s)", Collections.EMPTY_LIST, PrimitiveType.BOOLEAN),
  stringContains("%s.contains(%s)", Collections.singletonList(PrimitiveType.STRING), PrimitiveType.STRING),
  stringEquals("%s.equals(%s)", Collections.singletonList(PrimitiveType.STRING), PrimitiveType.STRING);

  private final String codeMask;
  private final List<TypeProvider> expectedParametersTypes;
  private final TypeProvider operandType;

  MatcherType(String codeMask, List<TypeProvider> expectedParametersTypes, TypeProvider operandType) {
    this.codeMask = codeMask;
    this.expectedParametersTypes = expectedParametersTypes;
    this.operandType = operandType;
  }

  public String getCode(List<MethodParameter> matcherParameters, String methodInvocationString) {
    if(!isParametersTypesMatch(this.expectedParametersTypes, matcherParameters)) {
      throw new UtamError(TypeUtilities.getUnmatchedParametersErr(this.expectedParametersTypes, matcherParameters));
    }
    if(this.expectedParametersTypes.isEmpty()) {
      return String.format(codeMask, methodInvocationString);
    }
    return String.format(codeMask, methodInvocationString, getParametersValuesString(matcherParameters));
  }

  public List<TypeProvider> getExpectedParametersTypes() {
    return expectedParametersTypes;
  }

  public TypeProvider getOperandType() {
    return operandType;
  }
}
