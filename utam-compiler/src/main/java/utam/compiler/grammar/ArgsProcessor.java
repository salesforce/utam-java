/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import static utam.compiler.helpers.TypeUtilities.FUNCTION;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.function.Function;
import utam.compiler.UtamCompilationError;
import utam.compiler.helpers.ActionType;
import utam.compiler.helpers.MatcherType;
import utam.compiler.helpers.MethodContext;
import utam.compiler.helpers.TranslationContext;
import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.TypeProvider;

/**
 * helper class: holds information for a one time args processing. Includes: translation context,
 * validation message and list of expected args types (if known)
 *
 * @since 236
 */
class ArgsProcessor {

  final String validationString;
  private final List<MethodParameter> parameters = new ArrayList<>();
  final Function<MethodParameter,MethodParameter> parameterReferenceTransformer;
  final TranslationContext context;

  ArgsProcessor(TranslationContext translationContext, String validationString, Function<MethodParameter,MethodParameter> parameterReferenceTransformer) {
    this.validationString = validationString;
    this.context = translationContext;
    this.parameterReferenceTransformer = parameterReferenceTransformer;
  }

  ArgsProcessor(TranslationContext context, MethodContext methodContext) {
    this(context, methodContext, p -> p);
  }

  ArgsProcessor(TranslationContext context, MethodContext methodContext, Function<MethodParameter,MethodParameter> parameterReferenceTransformer) {
    this(context, String.format("method '%s'", methodContext.getName()), parameterReferenceTransformer);
  }

  ArgsProcessor(TranslationContext context, String argsContext) {
    this(context, argsContext, p -> p);
  }

  List<MethodParameter> getParameters(UtamArgument[] args) {
    checkParametersCount(args, getExpectedParametersCount());
    if (args == null) {
      return parameters;
    }
    for (int i = 0; i < args.length; i++) {
      MethodParameter parameter = parameterReferenceTransformer.apply(args[i].asParameter(context, parameterReferenceTransformer));
      if(parameter!= null) { // function parameter is returned as null
        checkParameter(parameter, getExpectedType(i));
        if(!parameter.isLiteral()) {
          checkDuplicateName(parameter);
        }
        parameters.add(parameter);
      }
    }
    return parameters;
  }

  Integer getExpectedParametersCount() {
    return null;
  }

  TypeProvider getExpectedType(int index) {
    return null;
  }

  final void checkParameter(MethodParameter parameter, TypeProvider expectedType) {
    if(parameter == null) { //function
      return;
    }
    TypeProvider actualType = parameter.getType();
    if (expectedType != null && !expectedType.isSameType(actualType)) {
      throw new UtamCompilationError(String.format(UtamArgument.ERR_ARGS_WRONG_TYPE,
          validationString,
          parameter.getValue(),
          expectedType.getSimpleName(),
          actualType.getSimpleName()));
    }
  }

  private void checkParametersCount(UtamArgument[] args, Integer expectedCnt) {
    if(expectedCnt == null) {
      return;
    }
    int actualCnt = args == null ? 0 : args.length;
    if (expectedCnt != actualCnt) {
      throw new UtamCompilationError(String.format(UtamArgument.ERR_ARGS_WRONG_COUNT,
          validationString,
          expectedCnt,
          actualCnt));
    }
  }

  private void checkDuplicateName(MethodParameter parameter) {
    parameters.forEach(arg -> {
      // compare non literal names to avoid collisions
      if (!arg.isLiteral() && parameter.getValue().equals(arg.getValue())) {
        throw new UtamCompilationError(
            String.format(UtamArgument.ERR_ARGS_DUPLICATE_NAMES, validationString,
                parameter.getValue()));
      }
    });
  }

  /**
   * args processor with a list of expected args types
   *
   * @since 236
   */
  static class ArgsProcessorWithExpectedTypes extends ArgsProcessor {

    private final List<TypeProvider> expectedTypes;

    ArgsProcessorWithExpectedTypes(TranslationContext translationContext, String validationString,
        List<TypeProvider> expectedParametersTypes, Function<MethodParameter,MethodParameter> parameterReferenceTransformer) {
      super(translationContext, validationString, parameterReferenceTransformer);
      this.expectedTypes = expectedParametersTypes;
    }

    ArgsProcessorWithExpectedTypes(TranslationContext translationContext, String validationString,
        List<TypeProvider> expectedParametersTypes) {
      this(translationContext, validationString, expectedParametersTypes, p -> p);
    }

    ArgsProcessorWithExpectedTypes(TranslationContext context, MethodContext methodContext,
        List<TypeProvider> expectedParametersTypes, Function<MethodParameter,MethodParameter> parameterReferenceTransformer) {
      this(context, String.format("method '%s'", methodContext.getName()), expectedParametersTypes, parameterReferenceTransformer);
    }

    ArgsProcessorWithExpectedTypes(TranslationContext context, String matcherContext, MatcherType matcherType, Function<MethodParameter,MethodParameter> parameterReferenceTransformer) {
      this(context, String.format("matcher '%s' for %s", matcherType, matcherContext), matcherType.getExpectedParametersTypes(), parameterReferenceTransformer);
    }

    ArgsProcessorWithExpectedTypes(TranslationContext context, String matcherContext, MatcherType matcherType) {
      this(context, matcherContext, matcherType, p -> p);
    }

    @Override
    Integer getExpectedParametersCount() {
      return expectedTypes.size();
    }

    @Override
    TypeProvider getExpectedType(int index) {
      return expectedTypes.get(index);
    }
  }

  static class ArgsProcessorPredicate extends ArgsProcessorWithExpectedTypes {

    ArgsProcessorPredicate(TranslationContext translationContext, MethodContext methodContext) {
      super(translationContext, methodContext, Collections.singletonList(FUNCTION), p -> p);
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

    ArgsProcessorBasicAction(TranslationContext translationContext, String validationString, ActionType actionType, Function<MethodParameter,MethodParameter> parameterReferenceTransformer) {
      super(translationContext, validationString, parameterReferenceTransformer);
      parametersTypesOptions = actionType.getParametersTypesOptions();
      this.action = actionType;
    }

    ArgsProcessorBasicAction(TranslationContext translationContext, String validationString, ActionType actionType) {
      this(translationContext, validationString, actionType, p -> p);
    }

    /**
     * pick expected parameters types based on the number of provided args and a match of the args types
     *
     * @param args transformed arguments
     * @return proper args processor or throw
     */
    private ArgsProcessor getMatchingProcessor(UtamArgument[] args) {
      int argsCount = args == null ? 0 : args.length;
      for (List<TypeProvider> expectedTypesOption : action.getParametersTypesOptions()) {
        if (expectedTypesOption.size() == argsCount) {
          ArgsProcessor argsProcessor = new ArgsProcessorWithExpectedTypes(context,
              validationString,
              expectedTypesOption, parameterReferenceTransformer);
          boolean isMatch = true;
          for (int i = 0; i < argsCount; i++) {
            TypeProvider parameterType = args[i].asParameter(context).getType();
            if (!parameterType.isSameType(expectedTypesOption.get(i))) {
              isMatch = false;
              break;
            }
          }
          if (isMatch) {
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
            parametersTypesOptions.get(0), parameterReferenceTransformer).getParameters(transformedArgs);
      }
      return getMatchingProcessor(transformedArgs).getParameters(transformedArgs);
    }
  }
}
