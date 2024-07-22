/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.helpers;

import static utam.compiler.diagnostics.ValidationUtilities.VALIDATION;
import static utam.compiler.helpers.ParameterUtils.isExpectedType;
import static utam.compiler.helpers.TypeUtilities.PARAMETER_REFERENCE;

import java.util.AbstractMap.SimpleEntry;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map.Entry;
import java.util.Set;
import java.util.stream.Collectors;
import utam.compiler.UtamCompilationError;
import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.TypeProvider;

/**
 * Tracker of method parameters
 *
 * @author elizaveta.ivanova
 * @since 238
 */
public abstract class ParametersContext {

  final String contextString;
  final TranslationContext context;
  private final Set<String> parameterNames = new HashSet<>();

  /**
   * construct an instance of parameters context
   *
   * @param contextString parser context
   * @param context translation context
   */
  ParametersContext(String contextString, TranslationContext context) {
    this.contextString = contextString;
    this.context = context;
  }

  private static boolean isArgReference(MethodParameter parameter) {
    return parameter != null && PARAMETER_REFERENCE.isSameType(parameter.getType());
  }

  /**
   * check that all parameters were used, throws an error if there was not
   *
   * @return list of parameters
   */
  public abstract List<MethodParameter> getParameters();

  /**
   * set parameter to context
   *
   * @param parameter processed parameter
   */
  public abstract void setParameter(MethodParameter parameter);

  /**
   * check that parameters types and number match expected, return parameters
   *
   * @param expectedTypes expected types
   * @return list of parameters
   */
  public List<MethodParameter> getParameters(List<TypeProvider> expectedTypes) {
    List<MethodParameter> parameters =
        this.getParameters().stream()
            // replace argumentReferences before validation
            .map(this::getUnwrappedParameter)
            .collect(Collectors.toList());
    if (expectedTypes != null) {
      checkParametersCount(parameters, expectedTypes);
      for (int i = 0; i < parameters.size(); i++) {
        checkParameterType(parameters.get(i), expectedTypes.get(i));
      }
    }
    return parameters;
  }

  /**
   * validate nested parameters against expected types
   *
   * @param parameters list of parameters
   * @param expectedTypes expected types
   */
  public void setNestedParameters(
      List<MethodParameter> parameters, List<TypeProvider> expectedTypes) {
    ParametersContext validationOfTypes =
        new StatementParametersContext(contextString, context, null);
    parameters.forEach(validationOfTypes::setParameter);
    validationOfTypes.getParameters(expectedTypes);
  }

  abstract MethodParameter getReferencedParameter(MethodParameter p);

  abstract MethodParameter getUnwrappedParameter(MethodParameter parameter);

  abstract void registerMethodLevelParameter(MethodParameter parameter);

  /**
   * check that parameters number match expected
   *
   * @param parameters values to check
   * @param expectedTypes expected types
   */
  private void checkParametersCount(
      List<MethodParameter> parameters, List<TypeProvider> expectedTypes) {
    if (expectedTypes != null) {
      int expectedCount = expectedTypes.size();
      int actualCount = parameters.size();
      if (expectedCount != actualCount) {
        String message =
            VALIDATION.getErrorMessage(
                108, contextString, String.valueOf(expectedCount), String.valueOf(actualCount));
        throw new UtamCompilationError(message);
      }
    }
  }

  /**
   * check that parameter name is unique
   *
   * @param parameter value to check
   */
  void setParameterUniqueName(MethodParameter parameter) {
    if (parameter != null && !parameter.isLiteral()) {
      String name = parameter.getValue();
      if (parameterNames.contains(name)) {
        String message = VALIDATION.getErrorMessage(107, contextString, name);
        throw new UtamCompilationError(message);
      }
      parameterNames.add(name);
    }
  }

  /**
   * check that parameter type match
   *
   * @param parameter value to check
   * @param expected expected type
   */
  final void checkParameterType(MethodParameter parameter, TypeProvider expected) {
    if (!isExpectedType(parameter, expected)) {
      String expectedType = expected.getSimpleName();
      String actualType = parameter.getType().getSimpleName();
      String parameterValue = parameter.getValue();
      String message =
          VALIDATION.getErrorMessage(109, contextString, parameterValue, expectedType, actualType);
      throw new UtamCompilationError(message);
    }
  }

  /**
   * parameters of abstract method
   *
   * @author elizaveta.ivanova
   * @since 238
   */
  static class AbstractParametersContext extends MethodParametersContext {

    /**
     * construct an instance of parameters context
     *
     * @param contextString parser context
     * @param context translation context
     */
    AbstractParametersContext(
        String contextString, TranslationContext context, boolean hasMethodLevelArgs) {
      super(contextString, context, hasMethodLevelArgs);
    }

    @Override
    public List<MethodParameter> getParameters() {
      return declaredMethodParams.stream().map(Entry::getValue).collect(Collectors.toList());
    }
  }

  /**
   * parameters of compose method
   *
   * @author elizaveta.ivanova
   * @since 238
   */
  static class MethodParametersContext extends ParametersContext {

    final List<Entry<Boolean, MethodParameter>> declaredMethodParams = new ArrayList<>();
    private final boolean hasMethodLevelArgs;

    MethodParametersContext(
        String contextString, TranslationContext context, boolean hasMethodLevelArgs) {
      super(contextString, context);
      this.hasMethodLevelArgs = hasMethodLevelArgs;
    }

    @Override
    MethodParameter getUnwrappedParameter(MethodParameter parameter) {
      return parameter;
    }

    @Override
    MethodParameter getReferencedParameter(MethodParameter argReference) {
      String parameterName = argReference.getValue();
      for (int i = 0; i < declaredMethodParams.size(); i++) {
        Entry<Boolean, MethodParameter> alreadyDeclared = declaredMethodParams.get(i);
        if (alreadyDeclared.getValue().getValue().equals(parameterName)) {
          MethodParameter parameter = alreadyDeclared.getValue();
          declaredMethodParams.set(i, new SimpleEntry<>(true, parameter));
          return parameter;
        }
      }
      String message = VALIDATION.getErrorMessage(502, contextString, parameterName);
      throw new UtamCompilationError(message);
    }

    /**
     * set parameter at the method level
     *
     * @param parameter object
     */
    @Override
    public void setParameter(MethodParameter parameter) {
      String parameterName = parameter.getValue();
      if (isArgReference(parameter)) {
        // can't set at method level type of reference
        String message = VALIDATION.getErrorMessage(501, contextString, parameterName);
        throw new UtamCompilationError(message);
      }
      if (parameter.isLiteral()) {
        // can't set at method level literal
        String message = VALIDATION.getErrorMessage(105, contextString);
        throw new UtamCompilationError(message);
      }
      setParameterUniqueName(parameter);
      declaredMethodParams.add(new SimpleEntry<>(false, parameter));
    }

    @Override
    void registerMethodLevelParameter(MethodParameter p) {
      // parameter is never null or literal!
      String parameterName = p.getValue();
      if (hasMethodLevelArgs) {
        // check name and type
        for (int i = 0; i < declaredMethodParams.size(); i++) {
          Entry<Boolean, MethodParameter> alreadyDeclared = declaredMethodParams.get(i);
          MethodParameter parameter = alreadyDeclared.getValue();
          if (parameter.getValue().equals(parameterName)) {
            // if parameter from method level already used, and it's not arg reference
            // then throw an error
            if (alreadyDeclared.getKey() && !isArgReference(p)) {
              throw new UtamCompilationError(
                  VALIDATION.getErrorMessage(107, contextString, parameterName));
            }
            declaredMethodParams.set(i, new SimpleEntry<>(true, parameter));
            // if same parameter declared at method level - check same type
            checkParameterType(p, parameter.getType());
            return;
          }
        }
        // cant find declared parameter
        String message = VALIDATION.getErrorMessage(502, contextString, parameterName);
        throw new UtamCompilationError(message);
      }
      setParameterUniqueName(p);
      declaredMethodParams.add(new SimpleEntry<>(true, p));
    }

    /**
     * check that all parameters were used, trows an error if there was not
     *
     * @return list of parameters
     */
    public List<MethodParameter> getParameters() {
      return declaredMethodParams.stream()
          .map(
              p -> {
                // check that each parameter was used in statements
                if (!p.getKey()) {
                  String message =
                      VALIDATION.getErrorMessage(503, contextString, p.getValue().getValue());
                  throw new UtamCompilationError(message);
                }
                return p.getValue();
              })
          .collect(Collectors.toList());
    }
  }

  /**
   * parameters at statement level, or if no need to merge those
   *
   * @author elizaveta.ivanova
   * @since 238
   */
  public static class StatementParametersContext extends ParametersContext {

    private final ParametersContext methodParameters;
    private final List<MethodParameter> statementsParameters = new ArrayList<>();

    /**
     * construct an instance of parameters context
     *
     * @param methodContext method context
     * @param contextString parser context
     * @param context translation context
     */
    public StatementParametersContext(
        String contextString, TranslationContext context, MethodContext methodContext) {
      super(contextString, context);
      this.methodParameters = methodContext == null ? null : methodContext.getParametersContext();
    }

    private StatementParametersContext(StatementParametersContext parent) {
      super(parent.contextString, parent.context);
      this.methodParameters = parent.methodParameters;
    }

    @Override
    public List<MethodParameter> getParameters() {
      return statementsParameters;
    }

    @Override
    MethodParameter getReferencedParameter(MethodParameter p) {
      throw new IllegalStateException("Unsupported for statement level");
    }

    @Override
    void registerMethodLevelParameter(MethodParameter parameter) {
      throw new IllegalStateException("Unsupported for statement level");
    }

    @Override
    MethodParameter getUnwrappedParameter(MethodParameter parameter) {
      if (isArgReference(parameter)) {
        if (methodParameters == null) {
          String message = VALIDATION.getErrorMessage(502, contextString, parameter.getValue());
          throw new UtamCompilationError(message);
        }
        return methodParameters.getReferencedParameter(parameter);
      }
      return parameter;
    }

    /**
     * Sets a statement parameter
     *
     * @param parameter the parameter to set
     */
    @Override
    public void setParameter(MethodParameter parameter) {
      if (parameter == null) { // for predicate
        return;
      }
      if (isArgReference(parameter)) {
        if (methodParameters == null) {
          String message = VALIDATION.getErrorMessage(502, contextString, parameter.getValue());
          throw new UtamCompilationError(message);
        }
        MethodParameter referenced = methodParameters.getReferencedParameter(parameter);
        statementsParameters.add(referenced);
        return;
      }
      if (parameter.getNestedParameters() != null) {
        ParametersContext nestedParameters = new StatementParametersContext(this);
        for (MethodParameter nestedParameter : parameter.getNestedParameters()) {
          nestedParameters.setParameter(nestedParameter);
        }
      }
      if (!parameter.isLiteral()) { // check unique name and set to method level
        if (methodParameters != null) {
          methodParameters.registerMethodLevelParameter(parameter);
        }
        setParameterUniqueName(parameter);
      }
      statementsParameters.add(parameter);
    }
  }
}
