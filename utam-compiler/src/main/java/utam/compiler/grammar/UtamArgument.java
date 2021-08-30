/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import static utam.compiler.helpers.ParameterUtils.getParametersValuesString;
import static utam.compiler.helpers.PrimitiveType.BOOLEAN;
import static utam.compiler.helpers.PrimitiveType.NUMBER;
import static utam.compiler.helpers.PrimitiveType.STRING;
import static utam.compiler.helpers.PrimitiveType.isPrimitiveType;
import static utam.compiler.helpers.TypeUtilities.BASIC_ELEMENT;
import static utam.compiler.helpers.TypeUtilities.FRAME_ELEMENT;
import static utam.compiler.helpers.TypeUtilities.REFERENCE;
import static utam.compiler.helpers.TypeUtilities.ROOT_PAGE_OBJECT;
import static utam.compiler.helpers.TypeUtilities.SELECTOR;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import utam.compiler.UtamCompilationError;
import utam.compiler.grammar.UtamArgumentDeserializer.PageObjectType;
import utam.compiler.helpers.ActionType;
import utam.compiler.helpers.ElementContext;
import utam.compiler.helpers.LocatorCodeGeneration;
import utam.compiler.helpers.MatcherType;
import utam.compiler.helpers.MethodContext;
import utam.compiler.helpers.ParameterUtils.Literal;
import utam.compiler.helpers.ParameterUtils.LiteralClass;
import utam.compiler.helpers.ParameterUtils.Regular;
import utam.compiler.helpers.PrimitiveType;
import utam.compiler.helpers.TranslationContext;
import utam.compiler.helpers.TypeUtilities.BoundedClass;
import utam.compiler.representation.ComposeMethodStatement;
import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.PageObjectMethod;
import utam.core.declarative.representation.TypeProvider;

/**
 * UTAM argument inside selector, method, filter, matcher etc.
 *
 * @author elizaveta.ivanova
 * @since 228
 */
@JsonDeserialize(using = UtamArgumentDeserializer.class)
public abstract class UtamArgument {

  static final String FUNCTION_TYPE_PROPERTY = "function";
  static final String SELECTOR_TYPE_PROPERTY = "locator";
  static final String ELEMENT_REFERENCE_TYPE_PROPERTY = "elementReference";
  static final String ELEMENT_TYPE_PROPERTY = "element";
  static final String PAGE_OBJECT_TYPE_PROPERTY = "pageObject";
  static final String FRAME_ELEMENT_TYPE_PROPERTY = "frame";
  static final String ERR_ARGS_WRONG_TYPE = "%s: expected type is '%s', actual was '%s'";
  static final String ERR_ARGS_DUPLICATE_NAMES = "%s: duplicate arguments names '%s'";
  static final String ERR_ARGS_WRONG_COUNT = "%s: expected %s parameters, provided %s";
  static final String ERR_WHILE_PARSING = "not reachable because of deserializer";
  static final String ERR_GET_PREDICATE_NEEDS_PREDICATE_ARG = "Only predicate argument supports this method";
  final Object value;
  final String name;
  final String type;
  final UtamMethodAction[] conditions;
  final UtamArgument[] nestedArgs;

  @JsonCreator
  UtamArgument(
      @JsonProperty(value = "value") Object value,
      @JsonProperty(value = "name") String name,
      @JsonProperty(value = "type") String type,
      @JsonProperty(value = "predicate") UtamMethodAction[] conditions,
      @JsonProperty(value = "args") UtamArgument[] nestedArgs) {
    this.name = name;
    this.type = type;
    this.value = value;
    this.conditions = conditions;
    this.nestedArgs = nestedArgs;
  }

  abstract MethodParameter asParameter(TranslationContext translationContext);

  List<ComposeMethodStatement> getPredicate(TranslationContext context, MethodContext methodContext) {
    throw new IllegalStateException(ERR_GET_PREDICATE_NEEDS_PREDICATE_ARG);
  }

  static class UtamArgumentNonLiteral extends UtamArgument {

    @JsonCreator
    UtamArgumentNonLiteral(
        @JsonProperty(value = "name") String name,
        @JsonProperty(value = "type") String type) {
      super(null, name, type, null, null);
    }

    @Override
    MethodParameter asParameter(TranslationContext translationContext) {
      if (isPrimitiveType(type)) {
        return new Regular(name, PrimitiveType.fromString(type));
      }
      if (SELECTOR_TYPE_PROPERTY.equals(type)) {
        return new Regular(name, SELECTOR);
      }
      if (REFERENCE.getSimpleName().equals(type)) {
        return new Regular(name, REFERENCE);
      }
      if (FRAME_ELEMENT_TYPE_PROPERTY.equals(type)) {
        return new Regular(name, FRAME_ELEMENT);
      }
      if (PAGE_OBJECT_TYPE_PROPERTY.equals(type)) {
        return new Regular(name, new BoundedClass(ROOT_PAGE_OBJECT, null));
      }
      if (ELEMENT_TYPE_PROPERTY.equals(type)) {
        return new Regular(name, BASIC_ELEMENT);
      }
      throw new UtamCompilationError(ERR_WHILE_PARSING);
    }
  }

  @JsonDeserialize //use default deserializer for nested args
  public static class UtamArgumentLiteral extends UtamArgument {

    @JsonCreator
    public UtamArgumentLiteral(@JsonProperty(value = "value") Object value) {
      super(value, null, null, null, null);
    }

    @Override
    MethodParameter asParameter(TranslationContext translationContext) {
      if (value instanceof UtamSelector) {
        UtamSelector selector = (UtamSelector) value;
        LocatorCodeGeneration locatorCode = selector.getCodeGenerationHelper(translationContext);
        return locatorCode.getLiteralParameter();
      }
      if (value instanceof PageObjectType) {
        PageObjectType pageObjectType = (PageObjectType) value;
        TypeProvider literalType = pageObjectType.getLiteralType(translationContext);
        return new LiteralClass(literalType);
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
  }

  static class UtamArgumentPredicate extends UtamArgument {

    @JsonCreator
    UtamArgumentPredicate(
        @JsonProperty(value = "name") String name,
        @JsonProperty(value = "type") String type,
        @JsonProperty(value = "predicate") UtamMethodAction[] conditions) {
      super(null, name, type, conditions, null);
    }

    UtamArgumentPredicate(UtamMethodAction[] conditions){
      this(null, FUNCTION_TYPE_PROPERTY, conditions);
    }

    @Override
    MethodParameter asParameter(TranslationContext translationContext) {
      // predicate is not used as a parameter
      return null;
    }

    @Override
    List<ComposeMethodStatement> getPredicate(TranslationContext context, MethodContext methodContext) {
      List<ComposeMethodStatement> predicateStatements = new ArrayList<>();
      for (int i = 0; i < conditions.length; i++) {
        boolean isLastPredicateStatement = i == conditions.length - 1;
        predicateStatements
            .add(conditions[i].getComposeAction(context, methodContext, isLastPredicateStatement));
      }
      return predicateStatements;
    }
  }

  static class UtamArgumentElementReference extends UtamArgument {

    @JsonCreator
    UtamArgumentElementReference(
        @JsonProperty(value = "name") String name,
        @JsonProperty(value = "type") String type,
        @JsonProperty(value = "args") UtamArgument[] nestedArgs) {
      super(null, name, type, null, nestedArgs);
    }

    // used in tests
    UtamArgumentElementReference(String name, UtamArgument[] nestedArgs) {
      this(name, ELEMENT_REFERENCE_TYPE_PROPERTY, nestedArgs);
    }

    /**
     * returns element getter invocation code like this.getMyElement() also returns all its args as
     * they need to be added to the method parameters
     *
     * @param translationContext allows to get element getter name
     * @return literal value as an invocation code
     */
    @Override
    MethodParameter asParameter(TranslationContext translationContext) {
      ElementContext elementContext = translationContext.getElement(name);
      PageObjectMethod elementGetter = elementContext.getElementMethod();
      List<MethodParameter> actualParameters;
      // if args not set - no need to check number of parameters
      if(nestedArgs != null) {
        List<TypeProvider> expectedElementArgs = elementGetter
            .getDeclaration()
            .getParameters()
            .stream()
            .map(MethodParameter::getType)
            .collect(Collectors.toList());
        ArgsProcessor argsProcessor =
            new ArgsProcessorWithExpectedTypes(translationContext,
                String.format("element '%s' reference", name), expectedElementArgs);
        actualParameters = argsProcessor.getParameters(nestedArgs);
      } else {
        actualParameters = elementGetter.getDeclaration().getParameters();
      }
      String argsString = getParametersValuesString(actualParameters);
      String elementGetterName = elementGetter.getDeclaration().getName();
      translationContext.setMethodUsage(elementGetterName);
      String literalValue = String.format("this.%s(%s)", elementGetterName, argsString);
      return new Literal(literalValue, BASIC_ELEMENT, actualParameters);
    }
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
        MethodParameter parameter = argument.asParameter(context);
        if(!parameter.isLiteral()) {
          checkDuplicateName(parameter);
        }
        parameters.add(parameter);
      }
      return parameters;
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

    void checkDuplicateName(MethodParameter parameter) {
      parameters.forEach(arg -> {
        // compare non literal names to avoid collisions
        if (!arg.isLiteral() && parameter.getValue().equals(arg.getValue())) {
          throw new UtamCompilationError(
              String.format(ERR_ARGS_DUPLICATE_NAMES, validationString,
                  parameter.getValue()));
        }
      });
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
          MethodParameter parameter = args[i].asParameter(context);
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
          TypeProvider parameterType = args[0].asParameter(context).getType();
          // if number of args and first expected type is a match
          if (parameterType.isSameType(expectedTypesOption.get(0))) {
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
