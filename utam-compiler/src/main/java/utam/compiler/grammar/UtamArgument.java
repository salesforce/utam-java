/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import static utam.compiler.grammar.UtamArgumentDeserializer.SUPPORTED_NON_LITERAL_TYPES;
import static utam.compiler.grammar.UtamArgumentDeserializer.getErrorMessage;
import static utam.compiler.grammar.UtamArgumentDeserializer.getUnsupportedTypeErr;
import static utam.compiler.grammar.UtamMethod.isUsedAsChain;
import static utam.compiler.helpers.ParameterUtils.getParametersValuesString;
import static utam.compiler.helpers.PrimitiveType.BOOLEAN;
import static utam.compiler.helpers.PrimitiveType.NUMBER;
import static utam.compiler.helpers.PrimitiveType.STRING;
import static utam.compiler.helpers.PrimitiveType.isPrimitiveType;
import static utam.compiler.helpers.StatementContext.StatementType.PREDICATE_LAST_STATEMENT;
import static utam.compiler.helpers.StatementContext.StatementType.PREDICATE_STATEMENT;
import static utam.compiler.helpers.TypeUtilities.BASIC_ELEMENT;
import static utam.compiler.representation.FrameMethod.FRAME_ELEMENT;
import static utam.compiler.helpers.TypeUtilities.PAGE_OBJECT_PARAMETER;
import static utam.compiler.helpers.TypeUtilities.PARAMETER_REFERENCE;
import static utam.compiler.helpers.TypeUtilities.ROOT_PAGE_OBJECT_PARAMETER;
import static utam.compiler.helpers.TypeUtilities.SELECTOR;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;
import utam.compiler.UtamCompilationError;
import utam.compiler.grammar.ArgsProcessor.ArgsProcessorWithExpectedTypes;
import utam.compiler.helpers.ElementContext;
import utam.compiler.helpers.LocatorCodeGeneration;
import utam.compiler.helpers.MethodContext;
import utam.compiler.helpers.ParameterUtils.Literal;
import utam.compiler.helpers.ParameterUtils.LiteralPageObjectClass;
import utam.compiler.helpers.ParameterUtils.Regular;
import utam.compiler.helpers.PrimitiveType;
import utam.compiler.helpers.StatementContext;
import utam.compiler.helpers.TranslationContext;
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
  static final String PAGE_OBJECT_TYPE_PROPERTY = "pageObject";
  static final String ROOT_PAGE_OBJECT_TYPE_PROPERTY = "rootPageObject";
  static final String FRAME_ELEMENT_TYPE_PROPERTY = "frame";
  static final String ERR_ARGS_WRONG_TYPE = "%s parameter '%s': expected type is '%s', actual was '%s'";
  static final String ERR_ARGS_DUPLICATE_NAMES = "%s: duplicate arguments names '%s'";
  static final String ERR_ARGS_WRONG_COUNT = "%s: expected %s parameters, provided %s";
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

  abstract MethodParameter asParameter(TranslationContext translationContext,
      Function<MethodParameter, MethodParameter> parameterReferenceTransformer);

  final MethodParameter asParameter(TranslationContext translationContext) {
    return asParameter(translationContext, p -> p);
  }

  List<ComposeMethodStatement> getPredicate(TranslationContext context,
      MethodContext methodContext) {
    throw new IllegalStateException(ERR_GET_PREDICATE_NEEDS_PREDICATE_ARG);
  }

  @JsonDeserialize // reset to default
  static class UtamArgumentNonLiteral extends UtamArgument {

    @JsonCreator
    UtamArgumentNonLiteral(
        @JsonProperty(value = "name", required = true) String name,
        @JsonProperty(value = "type", required = true) String type) {
      super(null, name, type, null, null);
      if (!SUPPORTED_NON_LITERAL_TYPES.contains(type)) {
        throw new UtamCompilationError(getUnsupportedTypeErr(type));
      }
    }

    @Override
    MethodParameter asParameter(TranslationContext translationContext,
        Function<MethodParameter, MethodParameter> parameterReferenceTransformer) {
      if (isPrimitiveType(type)) {
        return new Regular(name, PrimitiveType.fromString(type));
      }
      if (SELECTOR_TYPE_PROPERTY.equals(type)) {
        return new Regular(name, SELECTOR);
      }
      if (PARAMETER_REFERENCE.getSimpleName().equals(type)) {
        return new Regular(name, PARAMETER_REFERENCE);
      }
      if (FRAME_ELEMENT_TYPE_PROPERTY.equals(type)) {
        return new Regular(name, FRAME_ELEMENT);
      }
      if (PAGE_OBJECT_TYPE_PROPERTY.equals(type)) {
        return new Regular(name, PAGE_OBJECT_PARAMETER);
      }
      if (ROOT_PAGE_OBJECT_TYPE_PROPERTY.equals(type)) {
        return new Regular(name, ROOT_PAGE_OBJECT_PARAMETER);
      }
      // this can only mean bug, never thrown
      throw new IllegalStateException(String.format("Bug in %s Unsupported argument type", UtamArgumentDeserializer.class.getName()));
    }
  }

  @JsonDeserialize //use default deserializer for nested args
  public static class UtamArgumentLiteralPrimitive extends UtamArgument {

    static final String ERR_ARGS_UNKNOWN = "unsupported literal argument type";

    private final MethodParameter methodParameter;

    @JsonCreator
    UtamArgumentLiteralPrimitive(
        @JsonProperty(value = "value", required = true) JsonNode valueNode) {
      super(null, null, null, null, null);
      if (valueNode.isTextual()) {
        methodParameter = new Literal(valueNode.asText(), STRING);
      } else if (valueNode.isBoolean()) {
        methodParameter = new Literal(String.valueOf(valueNode.asBoolean()), BOOLEAN);
      } else if (valueNode.isInt()) {
        methodParameter = new Literal(String.valueOf(valueNode.asInt()), NUMBER);
      } else {
        throw new UtamCompilationError(getErrorMessage(valueNode, ERR_ARGS_UNKNOWN));
      }
    }

    public UtamArgumentLiteralPrimitive(Boolean booleanValue) {
      super(null, null, null, null, null);
      methodParameter = new Literal(String.valueOf(booleanValue), BOOLEAN);
    }

    public UtamArgumentLiteralPrimitive(int numericValue) {
      super(null, null, null, null, null);
      methodParameter = new Literal(String.valueOf(numericValue), NUMBER);
    }

    UtamArgumentLiteralPrimitive(String strValue) {
      super(null, null, null, null, null);
      methodParameter = new Literal(strValue, STRING);
    }

    @Override
    MethodParameter asParameter(TranslationContext translationContext,
        Function<MethodParameter, MethodParameter> parameterReferenceTransformer) {
      return methodParameter;
    }
  }

  @JsonDeserialize // reset to default
  static class UtamArgumentLiteralSelector extends UtamArgument {

    UtamArgumentLiteralSelector(
        @JsonProperty(value = "value", required = true) UtamSelector value,
        @JsonProperty(value = "type", required = true) String type) {
      super(value, null, type, null, null);
    }

    @Override
    MethodParameter asParameter(TranslationContext translationContext,
        Function<MethodParameter, MethodParameter> parameterReferenceTransformer) {
      UtamSelector selector = (UtamSelector) value;
      LocatorCodeGeneration locatorCode = selector.getCodeGenerationHelper(translationContext);
      return locatorCode.getLiteralParameter();
    }
  }

  @JsonDeserialize // reset to default deserializer
  static class UtamArgumentPredicate extends UtamArgument {

    @JsonCreator
    UtamArgumentPredicate(
        @JsonProperty(value = "name") String name,
        @JsonProperty(value = "type", required = true) String type,
        @JsonProperty(value = "predicate", required = true) UtamMethodAction[] conditions) {
      super(null, name, type, conditions, null);
    }

    UtamArgumentPredicate(UtamMethodAction[] conditions) {
      this(null, FUNCTION_TYPE_PROPERTY, conditions);
    }

    @Override
    MethodParameter asParameter(TranslationContext translationContext,
        Function<MethodParameter, MethodParameter> parameterReferenceTransformer) {
      // predicate is not used as a parameter
      return null;
    }

    @Override
    List<ComposeMethodStatement> getPredicate(TranslationContext context,
        MethodContext methodContext) {
      List<ComposeMethodStatement> predicateStatements = new ArrayList<>();
      TypeProvider previousStatementReturn = null;
      for (int i = 0; i < conditions.length; i++) {
        StatementContext statementContext = new StatementContext(
            previousStatementReturn,
            i,
            isUsedAsChain(conditions, i),
            i == conditions.length - 1 ? PREDICATE_LAST_STATEMENT : PREDICATE_STATEMENT,
            conditions[i].getDeclaredReturnType(methodContext.getName()));
        ComposeMethodStatement statement = conditions[i]
            .getComposeAction(context, methodContext, statementContext);
        previousStatementReturn = statement.getReturnType();
        predicateStatements.add(statement);
      }
      return predicateStatements;
    }
  }

  @JsonDeserialize // reset to default deserializer
  static class UtamArgumentLiteralPageObjectType extends UtamArgument {

    @JsonCreator
    UtamArgumentLiteralPageObjectType(
        @JsonProperty(value = "value", required = true) String typeName,
        @JsonProperty(value = "type", required = true) String type) {
      super(typeName, null, type, null, null);
    }

    @Override
    MethodParameter asParameter(TranslationContext translationContext,
        Function<MethodParameter, MethodParameter> parameterReferenceTransformer) {
      TypeProvider literalType = translationContext.getType(value.toString());
      return new LiteralPageObjectClass(literalType);
    }
  }

  @JsonDeserialize // reset to default deserializer
  static class UtamArgumentElementReference extends UtamArgument {

    @JsonCreator
    UtamArgumentElementReference(
        @JsonProperty(value = "value", required = true) String elementName,
        @JsonProperty(value = "type", required = true) String type,
        @JsonProperty(value = "args") UtamArgument[] nestedArgs) {
      super(elementName, null, type, null, nestedArgs);
    }

    /**
     * returns element getter invocation code like this.getMyElement() also returns all its args as
     * they need to be added to the method parameters
     *
     * @param translationContext allows to get element getter name
     * @return literal value as an invocation code
     */
    @Override
    MethodParameter asParameter(TranslationContext translationContext,
        Function<MethodParameter, MethodParameter> parameterReferenceTransformer) {
      ElementContext elementContext = translationContext.getElement(value.toString());
      PageObjectMethod elementGetter = elementContext.getElementMethod();
      List<MethodParameter> actualParameters;
      // if args not set - no need to check number of parameters
      if (nestedArgs != null) {
        List<TypeProvider> expectedElementArgs = elementGetter
            .getDeclaration()
            .getParameters()
            .stream()
            .map(MethodParameter::getType)
            .collect(Collectors.toList());
        ArgsProcessor argsProcessor =
            new ArgsProcessorWithExpectedTypes(translationContext,
                String.format("element '%s' reference", name), expectedElementArgs, parameterReferenceTransformer);
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

}
