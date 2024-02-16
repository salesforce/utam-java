/*
 * Copyright (c) 2021, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: MIT
 * For full license text, see the LICENSE file in the repo root
 * or https://opensource.org/licenses/MIT
 */
package utam.compiler.grammar;

import static utam.compiler.diagnostics.ValidationUtilities.VALIDATION;
import static utam.compiler.grammar.JsonDeserializer.isEmptyNode;
import static utam.compiler.grammar.JsonDeserializer.nodeToString;
import static utam.compiler.grammar.JsonDeserializer.readNode;
import static utam.compiler.grammar.UtamComposeMethod.isUsedAsChain;
import static utam.compiler.grammar.UtamComposeMethod.processComposeNodes;
import static utam.compiler.helpers.ParameterUtils.getParametersValuesString;
import static utam.compiler.helpers.PrimitiveType.BOOLEAN;
import static utam.compiler.helpers.PrimitiveType.NUMBER;
import static utam.compiler.helpers.PrimitiveType.STRING;
import static utam.compiler.helpers.PrimitiveType.isPrimitiveType;
import static utam.compiler.helpers.StatementContext.StatementType.PREDICATE_LAST_STATEMENT;
import static utam.compiler.helpers.StatementContext.StatementType.PREDICATE_STATEMENT;
import static utam.compiler.helpers.TypeUtilities.FRAME_ELEMENT;
import static utam.compiler.helpers.TypeUtilities.PAGE_OBJECT_TYPE_NAME;
import static utam.compiler.helpers.TypeUtilities.PARAMETER_REFERENCE;
import static utam.compiler.helpers.TypeUtilities.ROOT_PAGE_OBJECT_TYPE_NAME;
import static utam.compiler.helpers.TypeUtilities.SELECTOR;
import static utam.compiler.helpers.TypeUtilities.W_PAGE_OBJECT_TYPE_PARAMETER;
import static utam.compiler.helpers.TypeUtilities.W_ROOT_PAGE_OBJECT_TYPE_PARAMETER;
import static utam.compiler.helpers.TypeUtilities.getPageObjectTypeParameter;
import static utam.compiler.helpers.TypeUtilities.getRootPageObjectTypeParameter;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.JsonNode;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import utam.compiler.UtamCompilationError;
import utam.compiler.grammar.UtamMethodAction.ArgumentsProvider;
import utam.compiler.helpers.ElementContext;
import utam.compiler.helpers.LocatorCodeGeneration;
import utam.compiler.helpers.MethodContext;
import utam.compiler.helpers.ParameterUtils.Literal;
import utam.compiler.helpers.ParameterUtils.LiteralPageObjectTypeParameter;
import utam.compiler.helpers.ParameterUtils.Regular;
import utam.compiler.helpers.ParametersContext;
import utam.compiler.helpers.PrimitiveType;
import utam.compiler.helpers.StatementContext;
import utam.compiler.helpers.TranslationContext;
import utam.compiler.representation.ComposeMethodStatement;
import utam.core.declarative.representation.MethodDeclaration;
import utam.core.declarative.representation.MethodParameter;
import utam.core.declarative.representation.TypeProvider;

/**
 * argument inside selector, method, filter, matcher etc.
 *
 * @author elizaveta.ivanova
 * @since 228
 */
abstract class UtamArgument {

  private static final String FUNCTION_TYPE_PROPERTY = "function";
  private static final String SELECTOR_TYPE_PROPERTY = "locator";
  private static final String ELEMENT_REFERENCE_TYPE_PROPERTY = "elementReference";
  private static final String FRAME_ELEMENT_TYPE_PROPERTY = "frame";
  private static final String SUPPORTED_LITERALS = String.join(", ", getSupportedLiteralTypes());
  private static final String SUPPORTED_NON_LITERALS =
      String.join(", ", getSupportedNonLiteralTypes());

  private static Set<String> getSupportedLiteralTypes() {
    Set<String> supported =
        Stream.of(PrimitiveType.values())
            .map(PrimitiveType::getJsonTypeName)
            .collect(Collectors.toSet());
    supported.add(SELECTOR_TYPE_PROPERTY);
    supported.add(FUNCTION_TYPE_PROPERTY);
    supported.add(ELEMENT_REFERENCE_TYPE_PROPERTY);
    supported.add(PAGE_OBJECT_TYPE_NAME);
    supported.add(ROOT_PAGE_OBJECT_TYPE_NAME);
    return supported;
  }

  private static Set<String> getSupportedNonLiteralTypes() {
    Set<String> supported =
        Stream.of(PrimitiveType.values())
            .map(PrimitiveType::getJsonTypeName)
            .collect(Collectors.toSet());
    supported.add(SELECTOR_TYPE_PROPERTY);
    supported.add(PARAMETER_REFERENCE.getSimpleName());
    supported.add(PAGE_OBJECT_TYPE_NAME);
    supported.add(ROOT_PAGE_OBJECT_TYPE_NAME);
    supported.add(FRAME_ELEMENT_TYPE_PROPERTY);
    return supported;
  }

  /**
   * process args node parser
   *
   * @param argsNode json node
   * @param parserContext parser context
   * @param literalArgsValidationMode defines if literal args are allowed
   * @return list of declared arguments
   */
  static List<UtamArgument> processArgsNode(
      JsonNode argsNode, String parserContext, ArgsValidationMode literalArgsValidationMode) {
    List<UtamArgument> args = new ArrayList<>();
    VALIDATION.validateOptionalNotEmptyArray(argsNode, parserContext, "args");
    if (isEmptyNode(argsNode)) {
      return args;
    }
    for (int i = 0; i < argsNode.size(); i++) {
      JsonNode argNode = argsNode.get(i);
      VALIDATION.validateNotNullObject(argNode, parserContext, "argument");
      boolean isLiteralsAllowed =
          literalArgsValidationMode == ArgsValidationMode.LITERAL_ALLOWED
              || literalArgsValidationMode == ArgsValidationMode.PREDICATE && i == 1;
      UtamArgument arg = processArgNode(argNode, parserContext, isLiteralsAllowed);
      args.add(arg);
    }
    return args;
  }

  /**
   * Validation mode - predicate and method declaration can't have literals
   *
   * @since 244
   */
  enum ArgsValidationMode {
    LITERAL_ALLOWED,
    LITERAL_NOT_ALLOWED,
    PREDICATE
  }

  private static UtamArgument processArgNode(
      JsonNode argNode, String parserContext, boolean isLiteralsAllowed) {
    boolean isLiteral = argNode.has("value");
    if (isLiteral && !isLiteralsAllowed) {
      throw new UtamCompilationError(argNode, VALIDATION.getErrorMessage(105, parserContext));
    }
    if (isLiteral && argNode.has("name")) {
      throw new UtamCompilationError(argNode, VALIDATION.getErrorMessage(111, parserContext));
    }
    if (isLiteral && argNode.has("description")) {
      throw new UtamCompilationError(argNode, VALIDATION.getErrorMessage(114, parserContext));
    }
    String type;
    if (argNode.has("type")) {
      JsonNode typeNode = argNode.get("type");
      type = VALIDATION.validateNotNullOrEmptyString(typeNode, parserContext, "type");
      if (FUNCTION_TYPE_PROPERTY.equals(type)) {
        if (argNode.has("description")) {
          throw new UtamCompilationError(argNode, VALIDATION.getErrorMessage(114, parserContext));
        }
        return readNode(
            argNode, UtamArgumentPredicate.class, VALIDATION.getErrorMessage(104, parserContext));
      }
      if (isLiteral && !getSupportedLiteralTypes().contains(type)) {
        throw new UtamCompilationError(
            argNode, VALIDATION.getErrorMessage(102, parserContext, type, SUPPORTED_LITERALS));
      }
      if (!isLiteral && !getSupportedNonLiteralTypes().contains(type)) {
        throw new UtamCompilationError(
            argNode, VALIDATION.getErrorMessage(103, parserContext, type, SUPPORTED_NON_LITERALS));
      }
    } else {
      // acceptable for predicate
      type = null;
    }
    if (isLiteral) {
      return processLiteralNode(argNode, type, parserContext);
    }
    if (!argNode.has("name")) {
      throw new UtamCompilationError(argNode, VALIDATION.getErrorMessage(113, parserContext));
    }
    UtamArgumentNonLiteral arg =
        readNode(
            argNode, UtamArgumentNonLiteral.class, VALIDATION.getErrorMessage(100, parserContext));
    VALIDATION.validateNotEmptyString(arg.name, parserContext, "name");
    return arg;
  }

  private static UtamArgument processLiteralNode(
      JsonNode argNode, String typeStr, String parserContext) {
    JsonNode valueNode = argNode.get("value");
    if (valueNode.isTextual()) {
      String valueStr = valueNode.asText();
      if (typeStr != null) {
        if (PAGE_OBJECT_TYPE_NAME.equals(typeStr)) {
          return new UtamArgumentLiteralPageObject(valueStr, W_PAGE_OBJECT_TYPE_PARAMETER);
        }
        if (ROOT_PAGE_OBJECT_TYPE_NAME.equals(typeStr)) {
          return new UtamArgumentLiteralPageObject(valueStr, W_ROOT_PAGE_OBJECT_TYPE_PARAMETER);
        }
        if (ELEMENT_REFERENCE_TYPE_PROPERTY.equals(typeStr)) {
          return readNode(
              argNode,
              UtamArgumentElementReference.class,
              VALIDATION.getErrorMessage(106, parserContext));
        }
      }
    }
    if (SELECTOR_TYPE_PROPERTY.equals(typeStr)) {
      UtamSelector selector =
          readNode(valueNode, UtamSelector.class, VALIDATION.getErrorMessage(1000, parserContext));
      return new UtamArgumentLiteralSelector(selector, parserContext);
    }
    if (valueNode.isBoolean() || valueNode.isInt() || valueNode.isTextual()) {
      return readNode(
          argNode,
          UtamArgumentLiteralPrimitive.class,
          VALIDATION.getErrorMessage(100, parserContext));
    }
    throw new UtamCompilationError(
        argNode,
        VALIDATION.getErrorMessage(
            102, parserContext, nodeToString(valueNode), SUPPORTED_LITERALS));
  }

  /**
   * unwraps argument as a parameter
   *
   * @param context translation context
   * @param methodContext method context, can be null
   * @param parametersContext parameters context
   * @return parameter instance
   */
  abstract MethodParameter asParameter(
      TranslationContext context, MethodContext methodContext, ParametersContext parametersContext);

  /**
   * get predicate statements from nested function argument
   *
   * @param context translation context
   * @param methodContext method context
   * @return list of predicate statements
   */
  List<ComposeMethodStatement> getPredicate(
      TranslationContext context, MethodContext methodContext) {
    throw new IllegalStateException("Only predicate argument supports this method");
  }

  /**
   * literal selector argument
   *
   * @author elizaveta.ivanova
   * @since 238
   */
  static class UtamArgumentLiteralSelector extends UtamArgument {

    private final UtamSelector selector;
    private final String parserContext;

    UtamArgumentLiteralSelector(UtamSelector selector, String parserContext) {
      this.selector = selector;
      this.parserContext = parserContext;
    }

    @Override
    MethodParameter asParameter(
        TranslationContext context,
        MethodContext methodContext,
        ParametersContext parametersContext) {
      LocatorCodeGeneration locatorCode =
          selector.getArgCodeGenerationHelper(parserContext, methodContext, context);
      return locatorCode.getLiteralParameter();
    }
  }

  /**
   * literal page object or root page object argument
   *
   * @author elizaveta.ivanova
   * @since 238
   */
  static class UtamArgumentLiteralPageObject extends UtamArgument {

    private final String pageObjectType;
    private final TypeProvider baseType;

    UtamArgumentLiteralPageObject(String pageObjectType, TypeProvider baseType) {
      this.pageObjectType = pageObjectType;
      this.baseType = baseType;
    }

    @Override
    MethodParameter asParameter(
        TranslationContext context,
        MethodContext methodContext,
        ParametersContext parametersContext) {
      TypeProvider literalType = context.getType(pageObjectType);
      return new LiteralPageObjectTypeParameter(literalType, baseType);
    }
  }

  /**
   * literal promitive argument
   *
   * @author elizaveta.ivanova
   * @since 238
   */
  static class UtamArgumentLiteralPrimitive extends UtamArgument {

    private final MethodParameter parameter;

    @JsonCreator
    UtamArgumentLiteralPrimitive(
        @JsonProperty(value = "value", required = true) JsonNode valueNode) {
      if (valueNode.isBoolean()) {
        parameter = new Literal(String.valueOf(valueNode.asBoolean()), BOOLEAN);
      } else if (valueNode.isInt()) {
        parameter = new Literal(String.valueOf(valueNode.asInt()), NUMBER);
      } else if (valueNode.isTextual()) {
        parameter = new Literal(valueNode.asText(), STRING);
      } else {
        throw new IllegalArgumentException();
      }
    }

    @Override
    MethodParameter asParameter(
        TranslationContext translationContext,
        MethodContext methodContext,
        ParametersContext context) {
      return parameter;
    }
  }

  /**
   * non literal argument (with name and type)
   *
   * @author elizaveta.ivanova
   * @since 238
   */
  static class UtamArgumentNonLiteral extends UtamArgument {

    private final String name;
    private final String type;
    private final String description;

    @JsonCreator
    UtamArgumentNonLiteral(
        @JsonProperty(value = "name", required = true) String name,
        @JsonProperty(value = "type") String type,
        @JsonProperty(value = "description") String description) {
      this.type = type;
      this.name = name;
      this.description = description;
    }

    @Override
    MethodParameter asParameter(
        TranslationContext context,
        MethodContext methodContext,
        ParametersContext parametersContext) {
      if (type == null) {
        throw new UtamCompilationError(
            VALIDATION.getErrorMessage(
                112, String.format("method \"%s\"", methodContext.getName()), name));
      }
      if (isPrimitiveType(type)) {
        return new Regular(name, PrimitiveType.fromString(type), this.description);
      }
      if (SELECTOR_TYPE_PROPERTY.equals(type)) {
        return new Regular(name, SELECTOR, this.description);
      }
      if (PARAMETER_REFERENCE.getSimpleName().equals(type)) {
        return new Regular(name, PARAMETER_REFERENCE, this.description);
      }
      if (FRAME_ELEMENT_TYPE_PROPERTY.equals(type)) {
        return new Regular(name, FRAME_ELEMENT, this.description);
      }
      if (PAGE_OBJECT_TYPE_NAME.equals(type)) {
        // if return type is not page object itself, use Class with wildcards
        // void test(Class<? extends PageObject> param), but T test(Class<T extends PageObject>
        // param)
        TypeProvider type = getPageObjectTypeParameter(methodContext.getDeclaredReturnType());
        return new Regular(name, type, this.description);
      }
      if (ROOT_PAGE_OBJECT_TYPE_NAME.equals(type)) {
        // if return type is not page object itself, use Class with wildcards
        // void test(Class<? extends RootPageObject> param), but T test(Class<T extends
        // RootPageObject> param)
        TypeProvider type = getRootPageObjectTypeParameter(methodContext.getDeclaredReturnType());
        return new Regular(name, type, this.description);
      }
      // this can only mean bug, never thrown
      throw new IllegalStateException(String.format("Unsupported argument type %s", type));
    }
  }

  /**
   * literal primitive argument
   *
   * @author elizaveta.ivanova
   * @since 238
   */
  static class UtamArgumentPredicate extends UtamArgumentNonLiteral {

    private final List<UtamMethodAction> conditions;

    @JsonCreator
    UtamArgumentPredicate(
        @JsonProperty(value = "name") String name,
        @JsonProperty(value = "type", required = true) String type,
        @JsonProperty(value = "description") String description,
        @JsonProperty(value = "predicate", required = true) JsonNode conditionsNode) {
      super(name, type, description);
      this.conditions = processComposeNodes("predicate", conditionsNode);
    }

    /**
     * This constructor is used to add waitForElement
     *
     * @param getterInvocation statement that invokes geter
     */
    UtamArgumentPredicate(UtamMethodAction getterInvocation) {
      super(null, "function", null);
      this.conditions = Collections.singletonList(getterInvocation);
    }

    @Override
    MethodParameter asParameter(
        TranslationContext translationContext,
        MethodContext methodContext,
        ParametersContext parametersContext) {
      // predicate is not used as a parameter
      return null;
    }

    @Override
    List<ComposeMethodStatement> getPredicate(
        TranslationContext context, MethodContext methodContext) {
      List<ComposeMethodStatement> predicateStatements = new ArrayList<>();
      TypeProvider previousStatementReturn = null;
      for (int i = 0; i < conditions.size(); i++) {
        UtamMethodAction condition = conditions.get(i);
        StatementContext statementContext =
            new StatementContext(
                previousStatementReturn,
                i,
                isUsedAsChain(conditions, i),
                i == conditions.size() - 1 ? PREDICATE_LAST_STATEMENT : PREDICATE_STATEMENT,
                condition.getDeclaredReturnType(methodContext.getName()));
        condition.checkBeforeLoadElements(methodContext);
        ComposeMethodStatement statement =
            condition.getComposeAction(context, methodContext, statementContext);
        previousStatementReturn = statement.getReturnType();
        predicateStatements.add(statement);
      }
      return predicateStatements;
    }
  }

  /**
   * literal element reference argument
   *
   * @author elizaveta.ivanova
   * @since 238
   */
  static class UtamArgumentElementReference extends UtamArgument {

    private final JsonNode argsNode;
    private final String value;

    @JsonCreator
    UtamArgumentElementReference(
        @JsonProperty(value = "value", required = true) String elementName,
        @JsonProperty(value = "type", required = true) String type,
        @JsonProperty(value = "args") JsonNode nestedArgs) {
      this.argsNode = nestedArgs;
      this.value = elementName;
    }

    /**
     * returns element getter invocation code like this.getMyElement() also returns all its args as
     * they need to be added to the method parameters
     *
     * @param context allows to get element getter name
     * @return literal value as an invocation code
     */
    @Override
    MethodParameter asParameter(
        TranslationContext context,
        MethodContext methodContext,
        ParametersContext parametersContext) {
      String parserContext = String.format("method \"%s\"", methodContext.getName());
      ArgumentsProvider provider = new ArgumentsProvider(argsNode, parserContext);
      ElementContext elementContext = provider.getElementArgument(context, value);
      MethodDeclaration elementGetter = elementContext.getElementMethod().getDeclaration();

      List<UtamArgument> args = provider.getArguments(ArgsValidationMode.LITERAL_ALLOWED);
      List<MethodParameter> parameters;
      if (args.isEmpty()) {
        // if args are not overwritten, get parameters from context
        parameters = elementGetter.getParameters();
      } else {
        List<TypeProvider> expectedElementArgs =
            elementGetter.getParameters().stream()
                .map(MethodParameter::getType)
                .collect(Collectors.toList());
        parameters =
            args.stream()
                .map(argument -> argument.asParameter(context, methodContext, parametersContext))
                .collect(Collectors.toList());
        parametersContext.setNestedParameters(parameters, expectedElementArgs);
      }
      String argsString = getParametersValuesString(parameters);
      String elementGetterName = elementGetter.getName();
      // mark that element getter is invoked, otherwise private element getter will not be generated
      context.setMethodUsage(elementGetterName);
      String literalValue = String.format("this.%s(%s)", elementGetterName, argsString);
      return new Literal(literalValue, elementContext.getType(), parameters);
    }
  }
}
