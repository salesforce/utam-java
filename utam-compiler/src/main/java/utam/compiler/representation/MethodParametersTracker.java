/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.representation;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import utam.compiler.UtamCompilationError;
import utam.core.declarative.representation.MethodParameter;

/**
 * track duplicate method parameters
 *
 * @author elizaveta.ivanova
 * @since 236
 */
public class MethodParametersTracker {

  private static final String ERR_CONTEXT_DUPLICATE_PARAMETERS = "duplicate parameters with name '%s' in the %s";

  private final List<MethodParameter> methodParameters = new ArrayList<>();
  private final Set<String> parameterNames = new HashSet<>();
  private final String methodContext;

  public MethodParametersTracker(String methodContext) {
    this.methodContext = methodContext;
  }

  void setMethodParameter(MethodParameter parameter) {
    if(!parameter.isLiteral()) {
      String parameterName = parameter.getValue();
      if(parameterNames.contains(parameterName)) {
        throw new UtamCompilationError(
            String.format(
                ERR_CONTEXT_DUPLICATE_PARAMETERS, parameterName, methodContext));
      }
      parameterNames.add(parameterName);
    }
    methodParameters.add(parameter);
  }

  public void setMethodParameters(List<MethodParameter> parameters) {
    parameters.forEach(this::setMethodParameter);
  }

  public List<MethodParameter> getMethodParameters() {
    return methodParameters;
  }
}
