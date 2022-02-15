/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.helpers;

import static utam.compiler.helpers.TypeUtilities.PARAMETER_REFERENCE;

import com.fasterxml.jackson.databind.JsonNode;
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
  final JsonNode argsNode;
  private final Set<String> parameterNames = new HashSet<>();

  /**
   * construct an instance of parameters context
   *
   * @param contextString parser context
   * @param context       translation context
   * @param argsNode      json node with args
   */
  ParametersContext(String contextString, TranslationContext context, JsonNode argsNode) {
    this.contextString = contextString;
    this.context = context;
    this.argsNode = argsNode;
  }

  private static boolean isArgReference(MethodParameter parameter) {
    return parameter != null && PARAMETER_REFERENCE.isSameType(parameter.getType());
  }

  /**
   * check that all parameters were used, trows an error if there was not
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
   */
  public List<MethodParameter> getParameters(List<TypeProvider> expectedTypes) {
    List<MethodParameter> parameters = this.getParameters()
        .stream()
        // replace argumentReferences before validation
        .map(this::getUnwrappedParameter)
        .collect(Collectors.toList());
    if (expectedTypes != null) {
      checkParametersCount(argsNode, parameters, expectedTypes);
      for (int i = 0; i < parameters.size(); i++) {
        checkParameterType(argsNode, parameters.get(i), expectedTypes.get(i));
      }
    }
    return parameters;
  }

  /**
   * validate nested parameters against expected types
   *
   * @param parameters    list of parameters
   * @param expectedTypes expected types
   */
  public void setNestedParameters(List<MethodParameter> parameters,
      List<TypeProvider> expectedTypes) {
    ParametersContext validationOfTypes = new StatementParametersContext(contextString, context,
        argsNode, null);
    parameters.forEach(validationOfTypes::setParameter);
    validationOfTypes.getParameters(expectedTypes);
  }

  abstract MethodParameter getReferencedParameter(MethodParameter p, JsonNode argsNode);

  abstract MethodParameter getUnwrappedParameter(MethodParameter parameter);

  abstract void registerParameter(MethodParameter parameter, JsonNode argsNode);

  /**
   * check that parameters number match expected
   *
   * @param argsNode      context node
   * @param parameters    values to check
   * @param expectedTypes expected types
   */
  private void checkParametersCount(
      JsonNode argsNode,
      List<MethodParameter> parameters,
      List<TypeProvider> expectedTypes) {
    if (expectedTypes != null) {
      int expectedCount = expectedTypes.size();
      int actualCount = parameters.size();
      if (expectedCount != actualCount) {
        String message = context
            .getErrorMessage("UA008", contextString, String.valueOf(expectedCount),
                String.valueOf(actualCount));
        throw new UtamCompilationError(argsNode, message);
      }
    }
  }

  /**
   * check that parameter name is unique
   *
   * @param parameter value to check
   * @param argsNode  context node
   */
  void setParameterUniqueName(MethodParameter parameter, JsonNode argsNode) {
    if (parameter != null && !parameter.isLiteral()) {
      String name = parameter.getValue();
      if (parameterNames.contains(name)) {
        // unclear how to test - by this time already validated from method action
        String message = context.getErrorMessage("UA007", contextString, name);
        throw new UtamCompilationError(argsNode, message);
      }
      parameterNames.add(name);
    }
  }

  /**
   * check that parameter type match
   *
   * @param argsNode  context node
   * @param parameter value to check
   * @param expected  expected type
   */
  final void checkParameterType(
      JsonNode argsNode,
      MethodParameter parameter,
      TypeProvider expected) {
    if (parameter == null) { //function
      return;
    }
    if (!expected.isSameType(parameter.getType())) {
      String expectedType = expected.getSimpleName();
      String actualType = parameter.getType().getSimpleName();
      String parameterValue = parameter.getValue();
      String message = context
          .getErrorMessage("UA009", contextString, parameterValue, expectedType, actualType);
      throw new UtamCompilationError(argsNode, message);
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
     * @param argsNode      json node with method args
     * @param contextString parser context
     * @param context       translation context
     */
    AbstractParametersContext(String contextString,
        TranslationContext context, JsonNode argsNode) {
      super(contextString, context, argsNode);
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

    MethodParametersContext(String contextString,
        TranslationContext context, JsonNode argsNode) {
      super(contextString, context, argsNode);
      hasMethodLevelArgs = argsNode != null && !argsNode.isNull();
    }

    @Override
    MethodParameter getUnwrappedParameter(MethodParameter parameter) {
      return parameter;
    }

    @Override
    MethodParameter getReferencedParameter(MethodParameter argReference, JsonNode argsNode) {
      String parameterName = argReference.getValue();
      for (int i = 0; i < declaredMethodParams.size(); i++) {
        Entry<Boolean, MethodParameter> alreadyDeclared = declaredMethodParams.get(i);
        if (alreadyDeclared.getValue().getValue().equals(parameterName)) {
          MethodParameter parameter = alreadyDeclared.getValue();
          declaredMethodParams.set(i, new SimpleEntry<>(true, parameter));
          return parameter;
        }
      }
      String message = context.getErrorMessage("UM002", contextString, parameterName);
      throw new UtamCompilationError(argsNode, message);
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
        String message = context.getErrorMessage("UM004", contextString, parameterName);
        throw new UtamCompilationError(argsNode, message);
      }
      if (parameter.isLiteral()) {
        // can't set at method level literal
        String message = context.getErrorMessage("UA005", contextString);
        throw new UtamCompilationError(argsNode, message);
      }
      setParameterUniqueName(parameter, argsNode);
      declaredMethodParams.add(new SimpleEntry<>(false, parameter));
    }

    @Override
    void registerParameter(MethodParameter p, JsonNode argsNode) {
      // parameter is never null or literal!
      String parameterName = p.getValue();
      if (hasMethodLevelArgs) {
        // check name and type
        for (int i = 0; i < declaredMethodParams.size(); i++) {
          Entry<Boolean, MethodParameter> alreadyDeclared = declaredMethodParams.get(i);
          if (alreadyDeclared.getValue().getValue().equals(parameterName)) {
            MethodParameter parameter = alreadyDeclared.getValue();
            declaredMethodParams.set(i, new SimpleEntry<>(true, parameter));
            // if same parameter declared at method level - check same type
            checkParameterType(argsNode, p, parameter.getType());
            return;
          }
        }
        // cant find declared parameter
        String message = context.getErrorMessage("UM002", contextString, parameterName);
        throw new UtamCompilationError(argsNode, message);
      }
      setParameterUniqueName(p, argsNode);
      declaredMethodParams.add(new SimpleEntry<>(true, p));
    }

    /**
     * check that all parameters were used, trows an error if there was not
     *
     * @return list of parameters
     */
    public List<MethodParameter> getParameters() {
      return declaredMethodParams.stream().map(p -> {
        // check that each parameter was used in statements
        if (!p.getKey()) {
          String message = context.getErrorMessage("UM003", contextString, p.getValue().getValue());
          throw new UtamCompilationError(message);
        }
        return p.getValue();
      }).collect(Collectors.toList());
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
     * @param argsNode      json node with method args
     * @param methodContext method context
     * @param contextString parser context
     * @param context       translation context
     */
    public StatementParametersContext(
        String contextString,
        TranslationContext context,
        JsonNode argsNode,
        MethodContext methodContext) {
      super(contextString, context, argsNode);
      this.methodParameters = methodContext == null ? null : methodContext.getParametersContext();
    }

    private StatementParametersContext(StatementParametersContext parent) {
      super(parent.contextString, parent.context, parent.argsNode);
      this.methodParameters = parent.methodParameters;
    }

    @Override
    public List<MethodParameter> getParameters() {
      return statementsParameters;
    }

    @Override
    MethodParameter getReferencedParameter(MethodParameter p, JsonNode node) {
      throw new IllegalStateException("Unsupported for statement level");
    }

    @Override
    void registerParameter(MethodParameter parameter, JsonNode argsNode) {
      throw new IllegalStateException("Unsupported for statement level");
    }

    @Override
    MethodParameter getUnwrappedParameter(MethodParameter parameter) {
      if (isArgReference(parameter)) {
        if (methodParameters == null) {
          String message = context.getErrorMessage("UM002", contextString, parameter.getValue());
          throw new UtamCompilationError(argsNode, message);
        }
        return methodParameters.getReferencedParameter(parameter, argsNode);
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
          String message = context.getErrorMessage("UM002", contextString, parameter.getValue());
          throw new UtamCompilationError(argsNode, message);
        }
        MethodParameter referenced = methodParameters.getReferencedParameter(parameter, argsNode);
        statementsParameters.add(referenced);
        return;
      }
      if (parameter.getNestedParameters() != null) {
        ParametersContext nestedParameters = new StatementParametersContext(this);
        for (MethodParameter nestedParameter : parameter.getNestedParameters()) {
          nestedParameters.setParameter(nestedParameter);
        }
      }
      if (!parameter.isLiteral()) { //check unique name and set to method level
        if (methodParameters != null) {
          methodParameters.registerParameter(parameter, argsNode);
        }
        setParameterUniqueName(parameter, argsNode);
      }
      statementsParameters.add(parameter);
    }

  }
}
