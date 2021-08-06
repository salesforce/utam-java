/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import static utam.compiler.helpers.PrimitiveType.BOOLEAN;
import static utam.compiler.helpers.PrimitiveType.NUMBER;
import static utam.compiler.helpers.PrimitiveType.STRING;
import static utam.compiler.helpers.PrimitiveType.isPrimitiveType;
import static utam.compiler.helpers.TypeUtilities.BASIC_ELEMENT;
import static utam.compiler.helpers.TypeUtilities.REFERENCE;
import static utam.compiler.helpers.TypeUtilities.SELECTOR;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import java.util.ArrayList;
import java.util.List;
import utam.compiler.UtamCompilationError;
import utam.compiler.grammar.UtamArgumentDeserializer.ElementReference;
import utam.compiler.helpers.ActionType;
import utam.compiler.helpers.LocatorCodeGeneration;
import utam.compiler.helpers.MatcherType;
import utam.compiler.helpers.MethodContext;
import utam.compiler.helpers.ParameterUtils.Literal;
import utam.compiler.helpers.ParameterUtils.Regular;
import utam.compiler.helpers.PrimitiveType;
import utam.compiler.helpers.TranslationContext;
import utam.compiler.representation.ComposeMethodStatement;
import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.TypeProvider;

/**
 * UTAM argument inside selector, method, filter, matcher etc.
 *
 * @author elizaveta.ivanova
 * @since 228
 */
@JsonDeserialize(using = UtamArgumentDeserializer.class)
public class UtamArgument {

  static final String FUNCTION_TYPE_PROPERTY = "function";
  static final String SELECTOR_TYPE_PROPERTY = "locator";
  static final String ELEMENT_TYPE_PROPERTY = "element";
  static final String ERR_ARGS_WRONG_TYPE = "%s: expected type is '%s', actual was '%s'";
  static final String ERR_ARGS_DUPLICATE_NAMES = "%s: duplicate arguments names '%s'";
  static final String ERR_ARGS_WRONG_COUNT = "%s: expected %s parameters, provided %s";
  static final String ERR_WHILE_PARSING = "not reachable because of deserializer";
  final Object value;
  private final String name;
  private final String type;
  private final UtamMethodAction[] conditions;

  @JsonCreator
  public UtamArgument(
      @JsonProperty(value = "value") Object value,
      @JsonProperty(value = "name") String name,
      @JsonProperty(value = "type") String type,
      @JsonProperty(value = "predicate") UtamMethodAction[] conditions) {
    this.name = name;
    this.type = type;
    this.value = value;
    this.conditions = conditions;
  }

  UtamArgument(UtamMethodAction[] conditions) {
    this(null, null, FUNCTION_TYPE_PROPERTY, conditions);
  }

  UtamArgument(String name, String type) {
    this(null, name, type, null);
  }

  public UtamArgument(Object value) {
    this(value, null, null, null);
  }

  MethodParameter getArgByValue(TranslationContext translationContext) {
    if (value instanceof UtamSelector) {
      UtamSelector selector = (UtamSelector) value;
      LocatorCodeGeneration locatorCode = selector.getCodeGenerationHelper(translationContext);
      return locatorCode.getLiteralParameter();
    }
    if (value instanceof ElementReference) {
      ElementReference elementReference = (ElementReference) value;
      return elementReference.getElementGetterAsLiteralArg(translationContext);
    }
    if (value instanceof Boolean) {
      return new Literal(value.toString(), BOOLEAN);
    }
    if (value instanceof Number) {
      return new Literal(value.toString(), NUMBER);
    }
    if (value instanceof String) {
      return new Literal(value.toString(), STRING);
    }
    throw new UtamCompilationError(ERR_WHILE_PARSING);
  }

  MethodParameter getArgByNameType() {
    if (isPrimitiveType(type)) {
      return new Regular(name, PrimitiveType.fromString(type));
    }
    if (SELECTOR_TYPE_PROPERTY.equals(type)) {
      return new Regular(name, SELECTOR);
    }
    if (REFERENCE.getSimpleName().equals(type)) {
      return new Regular(name, REFERENCE);
    }
    if (ELEMENT_TYPE_PROPERTY.equals(type)) {
      return new Regular(name, BASIC_ELEMENT);
    }
    throw new UtamCompilationError(ERR_WHILE_PARSING);
  }

  List<ComposeMethodStatement> getPredicate(TranslationContext context,
      MethodContext methodContext) {
    List<ComposeMethodStatement> predicateStatements = new ArrayList<>();
    for (int i = 0; i < conditions.length; i++) {
      boolean isLastPredicateStatement = i == conditions.length - 1;
      predicateStatements
          .add(conditions[i].getComposeAction(context, methodContext, isLastPredicateStatement));
    }
    return predicateStatements;
  }

  /**
   * helper class: holds information for a one time args processing. Includes: translation context,
   * validation message and list of expected args types (if known)
   *
   * @since 236
   */
  static class ArgsProcessor {

    final String validationString;
    final List<MethodParameter> parameters = new ArrayList<>();
    final TranslationContext context;

    ArgsProcessor(TranslationContext translationContext, String validationString) {
      this.validationString = validationString;
      this.context = translationContext;
    }

    ArgsProcessor(TranslationContext context, MethodContext methodContext) {
      this(context, String.format("method '%s'", methodContext.getName()));
    }

    List<MethodParameter> getParameters(UtamArgument[] args) {
      if (args == null) {
        return parameters;
      }
      for(UtamArgument argument: args) {
        parameters.add(getParameter(argument));
      }
      return parameters;
    }

    MethodParameter getParameter(UtamArgument utamArgument) {
      // predicate is not used as a parameter
      if (FUNCTION_TYPE_PROPERTY.equals(utamArgument.type)) {
        return null;
      }

      MethodParameter parameter = utamArgument.value == null ?
          utamArgument.getArgByNameType()
          : utamArgument.getArgByValue(context);

      if (!parameter.isLiteral()) {
        parameters.forEach(arg -> {
          // compare non literal names to avoid collisions
          if (!arg.isLiteral() && parameter.getValue().equals(arg.getValue())) {
            throw new UtamCompilationError(
                String.format(ERR_ARGS_DUPLICATE_NAMES, validationString,
                    parameter.getValue()));
          }
        });
      }
      return parameter;
    }

    void checkExpectedType(TypeProvider expectedType, TypeProvider actualType) {
      if (expectedType != null && !actualType.isSameType(expectedType)) {
        throw new UtamCompilationError(String.format(ERR_ARGS_WRONG_TYPE,
            validationString,
            expectedType.getSimpleName(),
            actualType.getSimpleName()));
      }
    }

    void checkParametersCount(UtamArgument[] args, int expectedCnt) {
      int actualCnt = args == null ? 0 : args.length;
      if (expectedCnt != actualCnt) {
        throw new UtamCompilationError(String.format(ERR_ARGS_WRONG_COUNT,
            validationString,
            expectedCnt,
            actualCnt));
      }
    }
  }

  /**
   * args processor with a list of expected args types
   *
   * @since 236
   */
  static class ArgsProcessorWithExpectedTypes extends ArgsProcessor {

    private final List<TypeProvider> expectedTypes;

    ArgsProcessorWithExpectedTypes(TranslationContext translationContext, String validationString,
        List<TypeProvider> expectedParametersTypes) {
      super(translationContext, validationString);
      this.expectedTypes = expectedParametersTypes;
    }

    ArgsProcessorWithExpectedTypes(TranslationContext context, MethodContext methodContext,
        List<TypeProvider> expectedParametersTypes) {
      this(context, String.format("method '%s'", methodContext.getName()), expectedParametersTypes);
    }

    ArgsProcessorWithExpectedTypes(TranslationContext context, String matcherContext, MatcherType matcherType) {
      this(context, String.format("matcher '%s' for %s", matcherType, matcherContext), matcherType.getExpectedParametersTypes());
    }

    @Override
    List<MethodParameter> getParameters(UtamArgument[] args) {
      checkParametersCount(args, expectedTypes.size());
      if (args != null) {
        for (int i = 0; i < args.length; i++) {
          MethodParameter parameter = getParameter(args[i]);
          if(parameter!= null) { // function parameter is returned as null
            checkExpectedType(expectedTypes.get(i), parameter.getType());
            parameters.add(parameter);
          }
        }
      }
      return parameters;
    }
  }

  /**
   * basic action can have more than one possible set of parameter types
   *
   * @since 236
   */
  static class ArgsProcessorBasicAction extends ArgsProcessor {

    static final String ERR_MATCHING_TYPES_NOT_FOUND = "%s: could not find matching parameters option for provided args";

    private final List<List<TypeProvider>> parametersTypesOptions;
    private final ActionType action;

    ArgsProcessorBasicAction(TranslationContext translationContext, String validationString, ActionType actionType) {
      super(translationContext, validationString);
      parametersTypesOptions = actionType.getParametersTypesOptions();
      this.action = actionType;
    }

    /**
     * pick expected parameters types based on the number of provided args and a match of the first
     * arg type
     *
     * @param args transformed arguments
     * @return proper args processor
     */
    private ArgsProcessor getMatchingProcessor(UtamArgument[] args) {
      int argsCount = args == null ? 0 : args.length;
      for (List<TypeProvider> expectedTypesOption : action.getParametersTypesOptions()) {
        if (expectedTypesOption.size() == argsCount) {
          ArgsProcessor argsProcessor = new ArgsProcessorWithExpectedTypes(context,
              validationString,
              expectedTypesOption);
          // if number of args and first expected type is a match
          if (argsProcessor.getParameter(args[0]).getType()
              .isSameType(expectedTypesOption.get(0))) {
            return argsProcessor;
          }
        }
      }
      throw new UtamCompilationError(String.format(ERR_MATCHING_TYPES_NOT_FOUND, validationString));
    }

    @Override
    List<MethodParameter> getParameters(UtamArgument[] args) {
      UtamArgument[] transformedArgs = action.getTransformedArgs(args);
      if (parametersTypesOptions.size() == 1) { // most action have one possible set of parameters
        return new ArgsProcessorWithExpectedTypes(context, validationString,
            parametersTypesOptions.get(0)).getParameters(transformedArgs);
      }
      return getMatchingProcessor(transformedArgs).getParameters(transformedArgs);
    }
  }
}
