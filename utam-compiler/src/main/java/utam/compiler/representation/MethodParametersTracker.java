/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.representation;

import static utam.compiler.diagnostics.ValidationUtilities.VALIDATION;

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

  private final List<MethodParameter> methodParameters = new ArrayList<>();
  private final Set<String> parameterNames = new HashSet<>();
  private final String methodContext;

  /**
   * Initializes a new instance of the MethodParametersTracker class
   *
   * @param methodContext the method context for tracking parameters
   */
  public MethodParametersTracker(String methodContext) {
    this.methodContext = methodContext;
  }

  public void setMethodParameter(MethodParameter parameter) {
    if (!parameter.isLiteral()) {
      String parameterName = parameter.getValue();
      if (parameterNames.contains(parameterName)) {
        throw new UtamCompilationError(
            VALIDATION.getErrorMessage(107, methodContext, parameterName));
      }
      parameterNames.add(parameterName);
    }
    if (parameter.getNestedParameters() != null) {
      parameter.getNestedParameters().forEach(this::setMethodParameter);
    }
    // set parameter if it's literal as well, so that we could import its type (for Locator)
    methodParameters.add(parameter);
  }

  /**
   * Sets the method parameters
   *
   * @param parameters the list of parameters to set
   */
  public void setMethodParameters(List<MethodParameter> parameters) {
    parameters.forEach(this::setMethodParameter);
  }

  /**
   * Gets the method parameters
   *
   * @return the list of the method parameters to set
   */
  public List<MethodParameter> getMethodParameters() {
    return methodParameters;
  }
}
